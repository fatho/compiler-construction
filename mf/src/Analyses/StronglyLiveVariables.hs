module Analyses.StronglyLiveVariables where

import Control.Applicative
import Control.Monad.Reader

import qualified AttributeGrammar as AG
import MonotoneFrameworks.MaximumFixpoint
import qualified Data.Map as Map
import Data.Maybe
import Data.Set as S

type VarSet = S.Set AG.Var

varSetLattice :: Lattice VarSet
varSetLattice = Lattice Set.empty Set.union Set.isSubsetOf

-- | Returns for each block its transfer function.
blockTransfer :: AG.Block -> [(AG.Label, VarSet -> VarSet)]
blockTransfer (AG.IAssignBlock l v iexpr) = [(l, \vs -> if (v `S.member` vs) then S.union (S.delete v vs) (AG.varSetIExpr iexpr S.empty) else vs)]
blockTransfer (AG.BBlock       l   bexpr) = [(l, \vs -> vs `S.union` (AG.varSetBExpr bexpr S.empty))]
blockTransfer b                           = zip (AG.labelsFromBlock b) (repeat id)

-- | Returns an analysis instance for a given labelled program.
stronglyLiveVariables :: AG.Program' -> Instance AG.Label VarSet
stronglyLiveVariables prog = inst where
   
  pSyn = AG.programSyn prog
  
  -- build a map from labels to the corresponding transfer functions
  transferMap = Map.fromList $ concatMap blockTransfer $ AG.blocks_Syn_Program' pSyn 
   
  inst = Instance
    { transferFunction = \l -> fromMaybe (error "invalid label") $ Map.lookup l transferMap 
    , transitions      = AG.reverseFlow $ AG.flow_Syn_Program' pSyn
    , extremalLabels   = AG.finals_Syn_Program' pSyn
    , extremalValue    = S.fromList $ AG.vars_Syn_Program' pSyn
    , labels           = AG.labels_Syn_Program' pSyn
    }
