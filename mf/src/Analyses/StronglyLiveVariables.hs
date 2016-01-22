{- | Implements a monotone framework for the strongly live variable analysis.
-}
module Analyses.StronglyLiveVariables where

import qualified Analyses.Builder as Builder
import qualified AttributeGrammar as AG
import           MonotoneFrameworks.Description
import qualified MonotoneFrameworks.Lattice as L

import qualified CCO.Printing as PP
import qualified Data.Set as Set
import           Data.Coerce

newtype VarSet = VarSet { getVarSet :: Set.Set AG.Var }
  deriving (Eq, Ord, Show, Read)

assignmentTransfer :: AG.Var -> AG.Expr -> VarSet -> VarSet
assignmentTransfer set expr (VarSet vs)
  -- only add if variable itself is live
  | set `Set.member` vs = VarSet $ Set.union (Set.delete set vs) (AG.vars_Syn_Expr $ AG.synthesize expr) 
  | otherwise           = VarSet vs

-- | Transfer function for a boolean expression in a condition.
-- Since conditions are always evaluated, all variables used inside are considered "live".
booleanExpTransfer :: AG.Scope -> AG.BExpr -> VarSet -> VarSet
booleanExpTransfer _ bexpr (VarSet vs) = VarSet $ Set.union vs (AG.vars_Syn_BExpr $ AG.synthesize bexpr)

-- | Transfer function at call site.
callTransfer :: AG.Scope -> AG.Proc' -> AG.Exprs -> AG.Var -> BinaryTransfer VarSet
callTransfer _ proc args retVar (VarSet outerLive) (VarSet procLive) = 
    VarSet $ Set.union callLive (Set.delete retVar outerLive) 
  where
    callLive = Set.unions $ zipWith propagateArg (AG.inp_Proc'_Proc' proc) args 
    -- when a function parameter is live, all variables used in the argument are also live 
    propagateArg v (AG.I e)
        | v `Set.member` procLive = AG.vars_Syn_IExpr $ AG.synthesize e
        | otherwise               = Set.empty
    propagateArg _ (AG.B _) = error "boolean variables not supported"

-- | Transfer function for function return.
-- If the variable receiving the return value is live, so is the output variable of the procedure
returnTransfer :: AG.Scope -> AG.Proc' -> AG.Exprs -> AG.Var -> UnaryTransfer VarSet
returnTransfer scope proc _ outerVar (VarSet outerEnv)
  | outerVar `Set.member` outerEnv = VarSet $ Set.insert (AG.out_Proc'_Proc' proc) procEnv
  | otherwise                      = VarSet $ procEnv
  where
    -- remove information about liveness of our local variables
    procEnv = outerEnv Set.\\ AG.localVars scope

blueprint :: AG.Syn_Program' -> Builder.Blueprint VarSet
blueprint pSyn = Builder.Blueprint
  { Builder.direction         = Builder.Backward
  , Builder.lattice           = coerce L.setUnion
  , Builder.extremalValue     = VarSet $ AG.globalVars_Syn_Program' pSyn
  , Builder.transferFunctions = Builder.defaultTransfer
      { Builder.assignIExpr = \_ v expr -> assignmentTransfer v (AG.I expr)
      , Builder.booleanExpr = booleanExpTransfer
      , Builder.callIn      = returnTransfer
      , Builder.callOut     = callTransfer
      , Builder.assignBExpr = \_ v expr -> assignmentTransfer v (AG.B expr)
      , Builder.assignRef   = \_ _ expr -> VarSet . Set.union (AG.vars_Syn_IExpr $ AG.synthesize expr) . getVarSet
      , Builder.malloc      = \_ v size -> VarSet . Set.union (AG.vars_Syn_IExpr $ AG.synthesize size) . Set.delete v . getVarSet
      , Builder.free        = \_ addr -> VarSet . Set.union (AG.vars_Syn_IExpr $ AG.synthesize addr) . getVarSet
      }
  }

-- | Returns an analysis instance for a given labelled program.
stronglyLiveVariables :: AG.Program' -> MF AG.Label VarSet
stronglyLiveVariables = Builder.buildMonotoneFramework blueprint

instance PP.Printable VarSet where
  pp (VarSet vs) = PP.sepBy (map PP.text $ Set.toList vs) (PP.text ", ")