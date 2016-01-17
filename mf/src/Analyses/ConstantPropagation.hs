module Analyses.ConstantPropagation where

import Control.Applicative
import Control.Monad.Reader

import qualified AttributeGrammar as AG
import qualified MonotoneFrameworks.Embellished as Embellished
import qualified MonotoneFrameworks.Lattice as Lattice
import MonotoneFrameworks.Instance
import qualified Data.Map as Map
import Data.Maybe

-- | The mapping from variables to values is the total function lattice from the set of all variables
-- (represented by the type 'AG.Var') to the lattice of integers enriched by top ('Lattice.Top') and ('Lattice.Bottom'). 
type VarEnv = Lattice.Function AG.Var (Lattice.Lifted Int)

-- | Returns for each block its transfer functions.
blockTransfer :: AG.Block -> [(AG.Label, VarEnv -> VarEnv)]
blockTransfer (AG.IAssignBlock l v expr) = [(l, \env -> Lattice.functionSet v (AG.evalIExpr expr (Lattice.functionApply env)) env)]
blockTransfer b  = zip (AG.labelsFromBlock b) (repeat id)

-- | Returns an analysis instance for a given labelled program.
constantPropagation :: AG.Program' -> Instance AG.Label VarEnv
constantPropagation prog = inst where
   
  pSyn = AG.programSyn prog
  
  -- build a map from labels to the corresponding transfer functions
  transferMap = Map.fromList $ concatMap blockTransfer $ AG.blocks_Syn_Program' pSyn
   
  inst = Instance
    { transferFunction = \l -> fromMaybe (error $ "invalid label: " ++ show l) $ Map.lookup l transferMap 
    , transitions      = AG.flow_Syn_Program' pSyn
    , extremalLabels   = [ AG.init_Syn_Program' pSyn ]
    , extremalValue    = pure Lattice.Top -- (equivalent to a constant function returnin top for each value)
    , labels           = AG.labels_Syn_Program' pSyn
    }
