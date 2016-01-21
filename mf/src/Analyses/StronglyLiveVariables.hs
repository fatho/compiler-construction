{- | Implements a monotone framework for the strongly live variable analysis.
-}
module Analyses.StronglyLiveVariables where

import Control.Applicative
import Control.Monad.Reader

import qualified AttributeGrammar as AG
import MonotoneFrameworks.Description
import qualified MonotoneFrameworks.Lattice as L
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set

type VarSet = Set.Set AG.Var

assignmentTransfer :: AG.Var -> AG.IExpr -> VarSet -> VarSet
assignmentTransfer v iexpr vs
  -- only add if variable itself is live
  | v `Set.member` vs = Set.union (Set.delete v vs) (AG.vars_Syn_IExpr $ AG.synthesize iexpr) 
  | otherwise       = vs

booleanExpTransfer :: AG.BExpr -> VarSet -> VarSet
booleanExpTransfer bexpr vs = Set.union vs (AG.vars_Syn_BExpr $ AG.synthesize bexpr)

callTransfer :: AG.Var -> [(AG.Var, AG.Expr)] -> BinaryTransfer VarSet
callTransfer out vars outer procLive = Set.union callLive (Set.delete out outer) where
  callLive = foldr combine id vars procLive
  combine (v, AG.I e) f = f . assignmentTransfer v e

procReturnTransfer :: Set.Set AG.Var -> AG.Var -> AG.Var -> UnaryTransfer VarSet
procReturnTransfer locals outerVar procRetVar outerEnv
  | outerVar `Set.member` outerEnv = Set.insert procRetVar procEnv
  | otherwise                      = procEnv
  where
    procEnv = outerEnv Set.\\ locals

-- | Returns an analysis instance for a given labelled program.
stronglyLiveVariables :: AG.Program' -> MF AG.Label VarSet
stronglyLiveVariables prog = inst where
   
  pSyn = AG.synthesize prog
    
  findProc p = AG.findProc p (AG.procs_Program'_Program' prog)
  
  scopeOf lbl = AG.labelScope_Syn_Program' pSyn Map.! lbl
  localVars lbl = case scopeOf lbl of
      AG.Global  _  -> Set.empty
      AG.Local _ vs -> vs
  
  -- | Returns for each block its transfer functions.
  unaryBlockTransfer :: AG.Block -> [(AG.Label, UnaryTransfer VarSet)]
  unaryBlockTransfer (AG.IAssignBlock l v iexpr) = [(l, assignmentTransfer v iexpr)]
  unaryBlockTransfer (AG.BBlock       l   bexpr) = [(l, booleanExpTransfer bexpr)]
  unaryBlockTransfer (AG.CallBlock lcall lret proc args out) =
    let callee = findProc proc
    in [ (lret, procReturnTransfer (localVars lret) out (AG.out_Proc'_Proc' callee) ) ]
  unaryBlockTransfer b  = zip (AG.labelsFromBlock b) (repeat id)
  
  binaryBlockTransfer :: AG.Block -> [(AG.Label, BinaryTransfer VarSet)]
  binaryBlockTransfer (AG.CallBlock lcall lret proc args out) =
    let callee = findProc proc
        callAssigns = zip (AG.inp_Proc'_Proc' callee) args
    in [ (lcall, callTransfer out callAssigns) ]
  binaryBlockTransfer _ = []
  
  -- build a map from labels to the corresponding transfer functions
  transferMap = Map.fromList $ AG.blocks_Syn_Program' pSyn >>= unaryBlockTransfer
  returnTransferMap = Map.fromList $ AG.blocks_Syn_Program' pSyn >>= binaryBlockTransfer
   
  inst = MF
    { lattice        = L.setUnion
    -- backwards analysis
    , flow           = AG.reverseFlow $ AG.flow_Syn_Program' pSyn
    , interFlow      = AG.reverseInterFlow $ AG.interflow_Syn_Program' pSyn
    , extremalLabels = AG.finals_Syn_Program' pSyn
    , extremalValue  = AG.globalVars_Syn_Program' pSyn
    , transfer       = \l -> transferMap Map.! l
    , interReturn    = \_ lret -> returnTransferMap Map.! lret
    , labels         = AG.labels $ AG.blocks_Syn_Program' pSyn
    }
