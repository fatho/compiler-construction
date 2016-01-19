module Analyses.StronglyLiveVariables where

import Control.Applicative
import Control.Monad.Reader

import qualified AttributeGrammar as AG
import MonotoneFrameworks.Description
import qualified MonotoneFrameworks.Lattice as L
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as S

type VarSet = S.Set AG.Var

assignmentTransfer :: AG.Var -> AG.IExpr -> VarSet -> VarSet
assignmentTransfer v iexpr vs
  -- only add if variable itself is live
  | v `S.member` vs = S.union (S.delete v vs) (AG.vars_Syn_IExpr $ AG.synthesize iexpr) 
  | otherwise       = vs

booleanExpTransfer :: AG.BExpr -> VarSet -> VarSet
booleanExpTransfer bexpr vs = S.union vs (AG.vars_Syn_BExpr $ AG.synthesize bexpr)

callTransfer :: AG.Var -> [(AG.Var, AG.Expr)] -> BinaryTransfer VarSet
callTransfer out vars outer procLive = S.union callLive (S.delete out outer) where
  callLive = foldr combine id vars procLive
  combine (v, AG.I e) f = f . assignmentTransfer v e

procReturnTransfer :: AG.Var -> AG.Var -> UnaryTransfer VarSet
-- TODO: remove non-global variables from scope (alternatively: change scoping rules)
procReturnTransfer outerVar procRetVar = assignmentTransfer outerVar (AG.Var procRetVar)

-- | Returns an analysis instance for a given labelled program.
stronglyLiveVariables :: AG.Program' -> MF AG.Label VarSet
stronglyLiveVariables prog = inst where
   
  pSyn = AG.synthesize prog
    
  findProc p = AG.findProc p (AG.procs_Program'_Program' prog)
  
  -- | Returns for each block its transfer functions.
  blockTransfer :: AG.Block -> [(AG.Label, AnyTransfer VarSet)]
  blockTransfer (AG.IAssignBlock l v iexpr) = [(l, Unary $ assignmentTransfer v iexpr)]
  blockTransfer (AG.BBlock       l   bexpr) = [(l, Unary $ booleanExpTransfer bexpr)]
  blockTransfer (AG.CallBlock lcall lret proc args out) =
    let callee = findProc proc
        callAssigns = zip (AG.inp_Proc'_Proc' callee) args
    in [ (lcall, Binary $ callTransfer out callAssigns)
       , (lret,  Unary $ procReturnTransfer out (AG.out_Proc'_Proc' callee))]
  blockTransfer b  = zip (AG.labelsFromBlock b) (repeat $ Unary id)
  
  -- build a map from labels to the corresponding transfer functions
  transferMap = Map.fromList $ 
    [ (l, f) | blk <- AG.blocks_Syn_Program' pSyn
             , (l, Unary f) <- blockTransfer blk ] 
  returnTransferMap = Map.fromList $ 
    [ (l, f) | blk <- AG.blocks_Syn_Program' pSyn
             , (l, Binary f) <- blockTransfer blk ]
   
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
