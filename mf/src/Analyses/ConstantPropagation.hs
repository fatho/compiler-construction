module Analyses.ConstantPropagation where

import Control.Applicative

import qualified AttributeGrammar as AG

--import qualified MonotoneFrameworks.Embellished as Embellished
--import qualified MonotoneFrameworks.MaximumFixpoint as MF

import MonotoneFrameworks.Embellished
import MonotoneFrameworks.MaximumFixpoint

import qualified Data.Map as Map
import Data.Maybe

-- | The mapping from variables to values is the total function lattice from the set of all variables
-- (represented by the type 'AG.Var') to the lattice of integers enriched by top ('Lattice.Top') and ('Lattice.Bottom'). 
-- type VarEnv = Lattice.Function AG.Var (Lattice.Lifted Int)

data ZT = Z Int | T
  deriving (Eq, Ord, Show, Read)

type VarMap = Map.Map AG.Var ZT

ztLattice :: Lattice ZT
ztLattice = Lattice undefined {- we don't have bottom there -} ztjoin ztleq where
  ztjoin (Z i) (Z j) | i == j = Z i
  ztjoin _     _              = T
  ztleq a b = join a b == b

varMapLattice :: Lattice VarMap
varMapLattice = Lattice Map.empty vmjoin vmleq where
  vmjoin = Map.unionWith (join ztLattice)
  leq    = Map.isSubmapOfBy (leq ztLattice)

assignmentTransfer :: AG.Var -> AG.IExpr -> VarMap -> VarMap
assignmentTransfer vSet expr env = Map.insert vSet (AG.evalIExpr expr (\v -> Map.findWithDefault T v env)) env

-- | Returns an analysis instance for a given labelled program.
constantPropagation :: AG.Program' -> Instance AG.Label VarEnv
constantPropagation prog = inst where
   
  pSyn = AG.programSyn prog
  
  findProc p = AG.findProc p (procs_Program'_Program' prog) 
  
  -- | Returns for each block its transfer functions.
  blockTransfer :: AG.Block -> [(AG.Label, VarEnv -> VarEnv)]
  blockTransfer (AG.IAssignBlock l v expr) = [(l, assignmentTransfer v expr)]
  blockTransfer (AG.CallBlock lcall lret proc args out) =
    let callee = findProc proc
        callAssigns = zip (inp_Proc'_Proc' callee) args
  blockTransfer b  = zip (AG.labelsFromBlock b) (repeat id)

  -- build a map from labels to the corresponding transfer functions
  transferMap = Map.fromList $ concatMap blockTransfer $ AG.blocks_Syn_Program' pSyn
   
  inst = Instance
    { transferFunction = \l -> fromMaybe (error $ "invalid label: " ++ show l) $ Map.lookup l transferMap 
    , transitions      = AG.flow_Syn_Program' pSyn
    , extremalLabels   = [ AG.init_Syn_Program' pSyn ]
    , extremalValue    = pure Lattice.Top -- (equivalent to a constant function returnin top for each value)
    , labels           = AG.labels_Syn_Program' pSyn
    }
