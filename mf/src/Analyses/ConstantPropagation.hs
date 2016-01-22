{-# LANGUAGE TypeOperators #-}
{- | Implements a monotone framework for the constant propagation analysis.
-}
module Analyses.ConstantPropagation where

import qualified Analyses.Builder as Builder
import qualified AttributeGrammar as AG
import qualified MonotoneFrameworks.Lattice as L
import           MonotoneFrameworks.Description
import qualified Util.Printing as UPP

import qualified CCO.Printing as PP
import           Data.Coerce
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Variables can either hold an integer or a boolean.
data Val = VBool Bool | VInt Int
  deriving (Eq, Ord, Read, Show)

-- | The lattice of values with an additional 'L.Top' value. 
-- To facilitate reuse, 'L.Lifted' also provides a 'L.Bottom', but that
-- is not used in this instance.
type ValT = L.Lifted Val

-- | The mapping of variables to our 'ValT' lattice.
newtype VarMap = VarMap { getVarMap :: Map.Map AG.Var ValT }
  deriving (Eq, Ord, Read, Show)

-- | Returns the contained integer value or an error if it is not an integer.
expectInt :: Val -> Int
expectInt (VInt i)  = i
expectInt (VBool _) = error "integer expected, got boolean"

-- | Returns the contained boolean value or an error if it is not a boolean.
expectBool :: Val -> Bool
expectBool (VBool b)  = b
expectBool (VInt _) = error "boolean expected, got integer"  

-- | Returns the value of a variable in the mapping, or 'L.Top' if it could not be found. 
lookupVar :: AG.Var -> VarMap -> ValT
lookupVar v = Map.findWithDefault L.Top v . getVarMap

-- | Binds a variable to a new value in the mapping.
bindVar :: AG.Var -> ValT -> VarMap -> VarMap
bindVar var val = VarMap . Map.insert var val . getVarMap

-- | Removes the binding of a variable.
unbindVar :: AG.Var -> VarMap -> VarMap
unbindVar v = VarMap . Map.delete v . getVarMap

-- | The 'ValT' lattice instance.
ztLattice :: L.Lattice ValT
ztLattice = L.lifted

-- | The 'VarMap' lattice instance.
varMapLattice :: L.Lattice VarMap
varMapLattice = coerce $ L.strictMapUnion ztLattice
-- note that the above is a safe coercion making use of GHC's coercible feature for newtypes

evalIExpr :: VarMap -> AG.IExpr -> L.Lifted Val
evalIExpr env = fmap VInt . AG.evalIExpr (\v -> fmap expectInt $ lookupVar v env) (\_ -> L.Top)

evalBExpr :: VarMap -> AG.BExpr -> L.Lifted Val
evalBExpr env = fmap VBool . AG.evalBExpr
  (\v -> fmap expectBool $ lookupVar v env) 
  (\v -> fmap expectInt $ lookupVar v env)
  (\_ -> L.Top)

-- | Transfer function for assignments.
assignmentTransfer :: (VarMap -> e -> L.Lifted Val) -> AG.Scope -> AG.Var -> e -> UnaryTransfer VarMap
assignmentTransfer eval _ vSet expr env = bindVar vSet (eval env expr) env

-- | Transfer function for a function call (unary)
callTransfer :: AG.Scope -> AG.Proc' -> AG.Exprs -> AG.Var -> UnaryTransfer VarMap
callTransfer scope proc args _ outerEnv = foldr assignArg removeLocals paramsWithArgs where
  paramsWithArgs = zip (AG.inp_Proc'_Proc' proc) args
  -- remove all locals from the environment before passing it to the procedure
  removeLocals = Set.foldr unbindVar outerEnv (AG.localVars scope)
  -- bind variable in proc environment, but read value from outer environment
  assignArg (vSet, AG.I e) procEnv = bindVar vSet (evalIExpr outerEnv e) procEnv
  assignArg (vSet, AG.B e) procEnv = bindVar vSet (evalBExpr outerEnv e) procEnv

-- | Transfer function for returning from a call (binary)
returnTransfer :: AG.Scope -> AG.Proc' -> AG.Exprs -> AG.Var -> BinaryTransfer VarMap
returnTransfer _ proc _ outerVar outerEnv procEnv =
  -- lookup value assigned to ret var in procedure and assign it to the
  -- variable receiving the result from the call. 
  bindVar outerVar (lookupVar procRetVar procEnv) outerEnv 
  where
    procRetVar = AG.out_Proc'_Proc' proc

blueprint :: AG.Syn_Program' -> Builder.Blueprint VarMap
blueprint pSyn = Builder.Blueprint
  { Builder.direction = Builder.Forward
  , Builder.lattice   = varMapLattice
  , Builder.extremalValue  = VarMap $ Map.fromList [ (v, L.Top) | v <- Set.toList $ AG.globalVars_Syn_Program' pSyn ]
  , Builder.transferFunctions = Builder.defaultTransfer
      { Builder.assignIExpr = assignmentTransfer evalIExpr
      , Builder.callIn      = callTransfer
      , Builder.callOut     = returnTransfer
      , Builder.assignBExpr = assignmentTransfer evalBExpr
      -- invalidate malloced variable (we don't know which address will be assigned)
      , Builder.malloc      = \_ v _ -> bindVar v L.Top
      }
  }

-- | Returns an analysis instance for a given labelled program.
constantPropagation :: AG.Program' -> MF AG.Label VarMap
constantPropagation = Builder.buildMonotoneFramework blueprint

instance PP.Printable Val where
  pp (VBool b) = PP.text $ if b then "true" else "false"
  pp (VInt i) = PP.showable i

instance PP.Printable VarMap where
  pp = UPP.ppMappingFlat . map (\(var,val) -> (PP.text var, PP.pp val)) . Map.toList . getVarMap