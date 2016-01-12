module Analyses.ConstantPropagation where

import Control.Applicative
import Control.Monad.Reader

import qualified AttributeGrammar as AG
import qualified MonotoneFrameworks.Lattice as Lattice
import MonotoneFrameworks.Instance
import qualified Data.Map as Map
import Data.Maybe

type VarEnv = Lattice.Function AG.Var (Lattice.Lifted Int)

blockTransfer :: AG.Block -> VarEnv -> VarEnv
blockTransfer (AG.IAssignBlock _ v expr)  env = Lattice.functionSet v (runReader (evalIExpr expr) env) env
blockTransfer _  env = env

evalIExpr :: AG.IExpr -> Reader VarEnv (Lattice.Lifted Int)
evalIExpr (AG.IConst x  ) = return $ Lattice.Value x
evalIExpr (AG.Var    v  ) = asks $ flip Lattice.functionApply v
evalIExpr (AG.Plus   x y) = liftM2 (liftA2 (+)) (evalIExpr x) (evalIExpr y)
evalIExpr (AG.Minus  x y) = liftM2 (liftA2 (-)) (evalIExpr x) (evalIExpr y)
evalIExpr (AG.Times  x y) = liftM2 (liftA2 (*)) (evalIExpr x) (evalIExpr y)
evalIExpr (AG.Divide x y) = liftM2 (liftA2 div) (evalIExpr x) (evalIExpr y)
evalIExpr (AG.Deref  x  ) = error "not implemented (yet)"

constantPropagation :: AG.Program' -> Instance AG.Label VarEnv
constantPropagation prog = inst where
  syn = AG.wrap_Program' (AG.sem_Program' prog) AG.Inh_Program'
  
  transferMap = Map.fromList [ (AG.labelFromBlock blk, blockTransfer blk) | blk <- AG.blocks_Syn_Program' syn ] 
  
  inst = Instance
    { transferFunction = \l -> fromMaybe (error "invalid label") $ Map.lookup l transferMap 
    , transitions      = AG.flow_Syn_Program' syn
    , extremalLabels   = [ AG.init_Syn_Program' syn ]
    , extremalValue    = pure Lattice.Top
    , labels           = AG.labels_Syn_Program' syn
    }
