module Analyses.ConstantPropagation where

import Control.Monad.Reader

import qualified AttributeGrammar as AG
import qualified MonotoneFrameworks.Lattice as Lattice
import MonotoneFrameworks.Instance
import qualified Data.Map as Map
import Data.Maybe

data Element a
  = Bottom
  | Value a
  | Top
  deriving (Eq, Show, Read)
  
liftElement2 :: (a -> a -> a) -> Element a -> Element a -> Element a
liftElement2 f (Value x) (Value y) = Value (f x y)
liftElement2 f Bottom x = Bottom
liftElement2 f x Bottom = Bottom
liftElement2 f _ _      = Top
  
instance Eq a => Lattice.Lattice (Element a) where
  bottom = Bottom
  join Bottom x = x
  join x Bottom = x
  join Top _    = Top
  join _ Top    = Top
  join (Value a) (Value b) = if a == b then Value a else Top
      
newtype VarEnv = VarEnv { getEnv :: Map.Map AG.Var (Element Int) }
  deriving (Eq, Show, Read)

instance Lattice.Lattice VarEnv where
  bottom = VarEnv $ Map.empty
  join (VarEnv a) (VarEnv b) = VarEnv $ Map.unionWith Lattice.join a b
  
lookupVar :: AG.Var -> VarEnv -> Element Int
lookupVar v (VarEnv env) = fromMaybe Bottom $ Map.lookup v env

insertVar :: AG.Var -> Element Int -> VarEnv -> VarEnv
insertVar var val (VarEnv env) = VarEnv (Map.insert var val env)

blockTransfer :: AG.Block -> VarEnv -> VarEnv
blockTransfer (AG.IAssignBlock _ v expr)  env = insertVar v (runReader (evalIExpr expr) env) env
blockTransfer _  env = env

evalIExpr :: AG.IExpr -> Reader VarEnv (Element Int)
evalIExpr (AG.IConst x  ) = return $ Value x
evalIExpr (AG.Var    v  ) = asks $ lookupVar v
evalIExpr (AG.Plus   x y) = liftM2 (liftElement2 (+)) (evalIExpr x) (evalIExpr y)
evalIExpr (AG.Minus  x y) = liftM2 (liftElement2 (-)) (evalIExpr x) (evalIExpr y)
evalIExpr (AG.Times  x y) = liftM2 (liftElement2 (*)) (evalIExpr x) (evalIExpr y)
evalIExpr (AG.Divide x y) = liftM2 (liftElement2 div) (evalIExpr x) (evalIExpr y)
evalIExpr (AG.Deref  x  ) = error "not implemented (yet)"

constantPropagation :: AG.Program' -> Instance AG.Label VarEnv
constantPropagation prog = inst where
  syn = AG.wrap_Program' (AG.sem_Program' prog) AG.Inh_Program'
  
  transferMap = Map.fromList [ (AG.labelFromBlock blk, blockTransfer blk) | blk <- AG.blocks_Syn_Program' syn ] 
  
  inst = Instance
    { transferFunction = \l -> fromMaybe (error "invalid label") $ Map.lookup l transferMap 
    , transitions      = AG.flow_Syn_Program' syn
    , extremalLabels   = [ AG.init_Syn_Program' syn ]
    , extremalValue    = VarEnv $ Map.fromList [ (v, Top) | v <- AG.vars_Syn_Program' syn ]
    , labels           = AG.labels_Syn_Program' syn
    }
