module Analyses.ConstantPropagation where

import Control.Applicative

import qualified AttributeGrammar as AG

import qualified MonotoneFrameworks.Lattice as Lattice
--import qualified MonotoneFrameworks.MaximumFixpoint as MF

import MonotoneFrameworks.Embellished
import MonotoneFrameworks.MaximumFixpoint

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

import Debug.Trace

-- | The mapping from variables to values is the total function lattice from the set of all variables
-- (represented by the type 'AG.Var') to the lattice of integers enriched by top ('Lattice.Top') and ('Lattice.Bottom'). 
-- type VarMap = Lattice.Function AG.Var (Lattice.Lifted Int)

type ZT = Lattice.Lifted Int

type VarMap = Map.Map AG.Var ZT

ztLattice :: Lattice ZT
ztLattice = Lattice Lattice.bottom Lattice.join Lattice.leq

varMapLattice :: Lattice VarMap
varMapLattice = Lattice Map.empty vmjoin vmleq where
  vmjoin = Map.unionWith (join ztLattice)
  vmleq  = Map.isSubmapOfBy (leq ztLattice)

assignmentTransfer :: AG.Var -> AG.IExpr -> VarMap -> VarMap
assignmentTransfer vSet expr env = 
    Map.insert vSet (AG.evalIExpr expr (\v -> Map.findWithDefault Lattice.Top v env)) env

callTransfer :: [(AG.Var, AG.Expr)] -> VarMap -> VarMap
callTransfer = foldr combine id where
  combine (v, AG.I e) f = f . assignmentTransfer v e

procReturnTransfer :: AG.Var -> AG.Var -> VarMap -> VarMap -> VarMap
procReturnTransfer outerVar procRetVar outerEnv procEnv =
  -- lookup value assigned to ret var in procedure and assign it to the
  -- variable receiving the result from the call.
  Map.insert outerVar (Map.findWithDefault Lattice.Top procRetVar procEnv) outerEnv

-- | Returns an analysis instance for a given labelled program.
constantPropagation :: AG.Program' -> MF AG.Label VarMap
constantPropagation prog = inst where
   
  pSyn = AG.programSyn prog
  
  findProc p = AG.findProc p (AG.procs_Program'_Program' prog)
  
  -- | Returns for each block its transfer functions.
  blockTransfer :: AG.Block -> [(AG.Label, VarMap -> VarMap)]
  blockTransfer (AG.IAssignBlock l v expr) = [(l, assignmentTransfer v expr)]
  blockTransfer (AG.CallBlock lcall lret proc args out) =
    let callee = findProc proc
        callAssigns = zip (AG.inp_Proc'_Proc' callee) args
    in [ (lcall, callTransfer callAssigns) ]
  blockTransfer b  = zip (AG.labelsFromBlock b) (repeat id)

  returnTransfer :: AG.Block -> [(AG.Label, VarMap -> VarMap -> VarMap)]
  returnTransfer  (AG.CallBlock lcall lret proc args out) =
    let callee = findProc proc
        callAssigns = zip (AG.inp_Proc'_Proc' callee) args
    in [ (lret, procReturnTransfer out (AG.out_Proc'_Proc' callee)) ]
  returnTransfer _ = []

  -- build a map from labels to the corresponding transfer functions
  transferMap = Map.fromList $ concatMap blockTransfer $ AG.blocks_Syn_Program' pSyn
  returnTransferMap = Map.fromList $ concatMap returnTransfer $ AG.blocks_Syn_Program' pSyn
   
  inst = MF
    { lattice        = varMapLattice
    , flow           = AG.flow_Syn_Program' pSyn
    , interFlow      = AG.interflow_Syn_Program' pSyn
    , extremalLabels = [ AG.init_Syn_Program' pSyn ]
    , extremalValue  = Map.fromList [ (v, Lattice.Top) | v <- Set.toList $ AG.globalVars_Syn_Program' pSyn ]
    , transfer       = \l -> lookupMap l transferMap
    , interReturn    = \_ lret -> lookupMap lret returnTransferMap
    , labels         = AG.labels $ AG.blocks_Syn_Program' pSyn
    }
