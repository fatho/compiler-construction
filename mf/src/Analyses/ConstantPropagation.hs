module Analyses.ConstantPropagation where

import Control.Applicative

import qualified AttributeGrammar as AG

import qualified MonotoneFrameworks.Lattice as L

import MonotoneFrameworks.Description
import MonotoneFrameworks.Embellished

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.Coerce

import qualified CCO.Printing as PP
import qualified Util.Printing as UPP

import Debug.Trace

-- | The mapping from variables to values is the total function lattice from the set of all variables
-- (represented by the type 'AG.Var') to the lattice of integers enriched by top ('L.Top') and ('L.Bottom'). 
-- type VarMap = L.Function AG.Var (L.Lifted Int)

type ZT = L.Lifted Int

newtype VarMap = VarMap { getVarMap :: Map.Map AG.Var ZT }
  deriving (Eq, Ord, Read, Show)
  
type CPTransfer = AnyTransfer VarMap

lookupVar :: AG.Var -> VarMap -> ZT
lookupVar v = Map.findWithDefault L.Top v . getVarMap

bindVar :: AG.Var -> ZT -> VarMap -> VarMap
bindVar var val = VarMap . Map.insert var val . getVarMap

ztLattice :: L.Lattice ZT
ztLattice = L.lifted

varMapLattice :: L.Lattice VarMap
varMapLattice = coerce $ L.strictMapUnion ztLattice
-- note that the above is a safe coercion making use of GHC's coercible feature for newtypes

assignmentTransfer :: AG.Var -> AG.IExpr -> UnaryTransfer VarMap
assignmentTransfer vSet expr env =
    bindVar vSet (AG.evalIExpr expr (\v -> lookupVar v env)) env

callTransfer :: [(AG.Var, AG.Expr)] -> UnaryTransfer VarMap
callTransfer = foldr combine id where
  combine (v, AG.I e) f = f . assignmentTransfer v e

procReturnTransfer :: AG.Var -> AG.Var -> BinaryTransfer VarMap
procReturnTransfer outerVar procRetVar outerEnv procEnv =
  -- lookup value assigned to ret var in procedure and assign it to the
  -- variable receiving the result from the call.
  bindVar outerVar (lookupVar procRetVar procEnv) outerEnv

-- | Returns an analysis instance for a given labelled program.
constantPropagation :: AG.Program' -> MF AG.Label VarMap
constantPropagation prog = inst where
   
  pSyn = AG.synthesize prog
  
  findProc p = AG.findProc p (AG.procs_Program'_Program' prog)
  
  -- | Returns for each block its transfer functions.
  blockTransfer :: AG.Block -> [(AG.Label, CPTransfer)]
  blockTransfer (AG.IAssignBlock l v expr) = [(l, Unary $ assignmentTransfer v expr)]
  blockTransfer (AG.CallBlock lcall lret proc args out) =
    let callee = findProc proc
        callAssigns = zip (AG.inp_Proc'_Proc' callee) args
    in [ (lcall, Unary $ callTransfer callAssigns)
       , (lret,  Binary $ procReturnTransfer out (AG.out_Proc'_Proc' callee))]
  blockTransfer b  = zip (AG.labelsFromBlock b) (repeat $ Unary id)

  -- build a map from labels to the corresponding transfer functions
  transferMap = Map.fromList $ 
    [ (l, f) | blk <- AG.blocks_Syn_Program' pSyn
             , (l, Unary f) <- blockTransfer blk ] 
  returnTransferMap = Map.fromList $ 
    [ (l, f) | blk <- AG.blocks_Syn_Program' pSyn
             , (l, Binary f) <- blockTransfer blk ]
   
  inst = MF
    { lattice        = varMapLattice
    , flow           = AG.flow_Syn_Program' pSyn
    , interFlow      = AG.interflow_Syn_Program' pSyn
    , extremalLabels = [ AG.init_Syn_Program' pSyn ]
    , extremalValue  = VarMap $ Map.fromList [ (v, L.Top) | v <- Set.toList $ AG.globalVars_Syn_Program' pSyn ]
    , transfer       = \l -> transferMap Map.! l
    , interReturn    = \_ lret -> returnTransferMap Map.! lret
    , labels         = AG.labels $ AG.blocks_Syn_Program' pSyn
    }

instance PP.Printable VarMap where
  pp = UPP.ppMap PP.pp PP.pp . getVarMap