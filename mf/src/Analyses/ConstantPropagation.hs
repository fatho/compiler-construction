{- | Implements a monotone framework for the constant propagation analysis.
-}
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

-- | The lattice of integer values with an additional 'L.Top' value. 
-- To facilitate reuse, 'L.Lifted' also provides a 'L.Bottom', but that
-- is not used in this instance.
type ZT = L.Lifted Int

-- | The mapping of variables to our 'ZT' lattice.
newtype VarMap = VarMap { getVarMap :: Map.Map AG.Var ZT }
  deriving (Eq, Ord, Read, Show)

-- | Returns the value of a variable in the mapping, or 'L.Top' if it could not be found. 
lookupVar :: AG.Var -> VarMap -> ZT
lookupVar v = Map.findWithDefault L.Top v . getVarMap

-- | Binds a variable to a new value in the mapping.
bindVar :: AG.Var -> ZT -> VarMap -> VarMap
bindVar var val = VarMap . Map.insert var val . getVarMap

unbindVar :: AG.Var -> VarMap -> VarMap
unbindVar v = VarMap . Map.delete v . getVarMap

-- | The 'ZT' lattice instance.
ztLattice :: L.Lattice ZT
ztLattice = L.lifted

-- | The 'VarMap' lattice instance.
varMapLattice :: L.Lattice VarMap
varMapLattice = coerce $ L.strictMapUnion ztLattice
-- note that the above is a safe coercion making use of GHC's coercible feature for newtypes

-- | Transfer function for assignments.
assignmentTransfer :: AG.Var -> AG.IExpr -> UnaryTransfer VarMap
assignmentTransfer vSet expr env =
    bindVar vSet (AG.evalIExpr expr (\v -> lookupVar v env)) env

-- | Transfer function for a function call (unary)
callTransfer :: Set.Set AG.Var -> [(AG.Var, AG.Expr)] -> UnaryTransfer VarMap
callTransfer localVars args outerEnv = foldr assignArg removeLocals args where
  -- remove all locals from the environment before passing it to the procedure
  removeLocals = Set.foldr unbindVar outerEnv localVars
  -- bind variable in proc environment, but read value from outer environment
  assignArg (v, AG.I e) procEnv = bindVar v (AG.evalIExpr e (\v -> lookupVar v outerEnv)) procEnv

-- | Transfer function for returning from a call (binary)
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
  
  scopeOf lbl = AG.labelScope_Syn_Program' pSyn Map.! lbl
  localVars lbl = case scopeOf lbl of
      AG.Global  _  -> Set.empty
      AG.Local _ vs -> vs
  
  -- | Returns for each block its unary transfer function.
  unaryBlockTransfer :: AG.Block -> [(AG.Label, UnaryTransfer VarMap)]
  unaryBlockTransfer (AG.IAssignBlock l v expr) = [(l, assignmentTransfer v expr)]
  unaryBlockTransfer (AG.CallBlock lcall lret proc args out) =
    let callee = findProc proc
        callAssigns = zip (AG.inp_Proc'_Proc' callee) args
    in [ (lcall, callTransfer (localVars lcall) callAssigns) ]
  unaryBlockTransfer b = zip (AG.labelsFromBlock b) (repeat id)
  
  -- | Returns for each block its binary transfer function.
  binaryBlockTransfer :: AG.Block -> [(AG.Label, BinaryTransfer VarMap)]
  binaryBlockTransfer (AG.CallBlock lcall lret proc args out) =
    let callee = findProc proc
    in [ (lret, procReturnTransfer out (AG.out_Proc'_Proc' callee))]
  binaryBlockTransfer b = []

  -- build a map from labels to the corresponding transfer functions
  transferMap = Map.fromList $ AG.blocks_Syn_Program' pSyn >>= unaryBlockTransfer
  returnTransferMap = Map.fromList $ AG.blocks_Syn_Program' pSyn >>= binaryBlockTransfer
   
  inst = MF
    { lattice        = varMapLattice
    -- forward analysis:
    , flow           = AG.flow_Syn_Program' pSyn
    , interFlow      = AG.interflow_Syn_Program' pSyn
    , extremalLabels = [ AG.init_Syn_Program' pSyn ]
    
    , extremalValue  = VarMap $ Map.fromList [ (v, L.Top) | v <- Set.toList $ AG.globalVars_Syn_Program' pSyn ]
    , transfer       = \l -> transferMap Map.! l
    , interReturn    = \_ lret -> returnTransferMap Map.! lret
    , labels         = AG.labels $ AG.blocks_Syn_Program' pSyn
    }

instance PP.Printable VarMap where
  pp = UPP.ppMappingFlat . map UPP.ppBoth . Map.toList . getVarMap