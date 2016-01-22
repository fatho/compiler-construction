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

-- | The lattice of integer values with an additional 'L.Top' value. 
-- To facilitate reuse, 'L.Lifted' also provides a 'L.Bottom', but that
-- is not used in this instance.
type ZT = L.Lifted Int

-- | The mapping of variables to our 'ZT' lattice.
newtype VarMap = VarMap { getVarMap :: Map.Map AG.Var ZT }
  deriving (Eq, Ord, Read, Show)

-- | Returns the value of a variable in the mapping, or 'L.Top' if it could not be found. 
lookupVar :: AG.Var -> VarMap -> ZT
lookupVar v = Map.findWithDefault L.Top v . getVarMap

-- | Binds a variable to a new value in the mapping.
bindVar :: AG.Var -> ZT -> VarMap -> VarMap
bindVar var val = VarMap . Map.insert var val . getVarMap

unbindVar :: AG.Var -> VarMap -> VarMap
unbindVar v = VarMap . Map.delete v . getVarMap

-- | The 'ZT' lattice instance.
ztLattice :: L.Lattice ZT
ztLattice = L.lifted

-- | The 'VarMap' lattice instance.
varMapLattice :: L.Lattice VarMap
varMapLattice = coerce $ L.strictMapUnion ztLattice
-- note that the above is a safe coercion making use of GHC's coercible feature for newtypes

-- | Transfer function for assignments.
assignmentTransfer :: AG.Scope -> AG.Var -> AG.IExpr -> UnaryTransfer VarMap
assignmentTransfer _ vSet expr env =
    bindVar vSet (AG.evalIExpr expr (\v -> lookupVar v env)) env

-- | Transfer function for a function call (unary)
callTransfer :: AG.Scope -> AG.Proc' -> AG.Exprs -> AG.Var -> UnaryTransfer VarMap
callTransfer scope proc args _ outerEnv = foldr assignArg removeLocals paramsWithArgs where
  paramsWithArgs = zip (AG.inp_Proc'_Proc' proc) args
  -- remove all locals from the environment before passing it to the procedure
  removeLocals = Set.foldr unbindVar outerEnv (AG.localVars scope)
  -- bind variable in proc environment, but read value from outer environment
  assignArg (vSet, AG.I e) procEnv = bindVar vSet (AG.evalIExpr e (\v -> lookupVar v outerEnv)) procEnv
  assignArg (_, AG.B _) _ = error "boolean variables not supported"

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
      { Builder.assignIExpr = assignmentTransfer
      , Builder.callIn      = callTransfer
      , Builder.callOut     = returnTransfer
      }
  }

-- | Returns an analysis instance for a given labelled program.
constantPropagation :: AG.Program' -> MF AG.Label VarMap
constantPropagation = Builder.buildMonotoneFramework blueprint

instance PP.Printable VarMap where
  pp = UPP.ppMappingFlat . map UPP.ppBoth . Map.toList . getVarMap