{- | Implements a monotone framework for the strongly live variable analysis.
-}
module Analyses.StronglyLiveVariables where

import qualified Analyses.Builder as Builder
import qualified AttributeGrammar as AG
import           MonotoneFrameworks.Description
import qualified MonotoneFrameworks.Lattice as L

import qualified Data.Set as Set

type VarSet = Set.Set AG.Var

assignmentTransfer :: AG.Scope -> AG.Var -> AG.IExpr -> VarSet -> VarSet
assignmentTransfer _ v iexpr vs
  -- only add if variable itself is live
  | v `Set.member` vs = Set.union (Set.delete v vs) (AG.vars_Syn_IExpr $ AG.synthesize iexpr) 
  | otherwise       = vs

-- | Transfer function for a boolean expression in a condition.
-- Since conditions are always evaluated, all variables used inside are considered "live".
booleanExpTransfer :: AG.Scope -> AG.BExpr -> VarSet -> VarSet
booleanExpTransfer _ bexpr vs = Set.union vs (AG.vars_Syn_BExpr $ AG.synthesize bexpr)

-- | Transfer function at call site.
callTransfer :: AG.Scope -> AG.Proc' -> AG.Exprs -> AG.Var -> BinaryTransfer VarSet
callTransfer _ proc args retVar outerLive procLive = Set.union callLive (Set.delete retVar outerLive) where
  callLive = Set.unions $ zipWith propagateArg (AG.inp_Proc'_Proc' proc) args 
  -- when a function parameter is live, all variables used in the argument are also live 
  propagateArg v (AG.I e)
    | v `Set.member` procLive = AG.vars_Syn_IExpr $ AG.synthesize e
    | otherwise               = Set.empty
  propagateArg _ (AG.B _) = error "boolean variables not supported"

-- | Transfer function for function return.
-- If the variable receiving the return value is live, so is the output variable of the procedure
returnTransfer :: AG.Scope -> AG.Proc' -> AG.Exprs -> AG.Var -> UnaryTransfer VarSet
returnTransfer scope proc _ outerVar outerEnv
  | outerVar `Set.member` outerEnv = Set.insert (AG.out_Proc'_Proc' proc) procEnv
  | otherwise                      = procEnv
  where
    procEnv = outerEnv Set.\\ (AG.localVars scope)

blueprint :: AG.Syn_Program' -> Builder.Blueprint VarSet
blueprint pSyn = Builder.Blueprint
  { Builder.direction         = Builder.Backward
  , Builder.lattice           = L.setUnion
  , Builder.extremalValue     = AG.globalVars_Syn_Program' pSyn
  , Builder.transferFunctions = Builder.defaultTransfer
      { Builder.assignIExpr = assignmentTransfer
      , Builder.booleanExpr = booleanExpTransfer
      , Builder.callIn      = returnTransfer
      , Builder.callOut     = callTransfer
      }
  }

-- | Returns an analysis instance for a given labelled program.
stronglyLiveVariables :: AG.Program' -> MF AG.Label VarSet
stronglyLiveVariables = Builder.buildMonotoneFramework blueprint