{-# LANGUAGE RecordWildCards #-}
module Analyses.Builder where

import qualified AttributeGrammar as AG
import qualified MonotoneFrameworks as MF
import qualified Data.Map.Strict as Map
import Data.Maybe

data Direction = Forward | Backward
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data TransferFunctions a = TransferFunctions
  { assignIExpr :: AG.Scope -> AG.Var -> AG.IExpr -> a -> a
  -- ^ integer assignment expression
  , assignBExpr :: AG.Scope -> AG.Var -> AG.BExpr -> a -> a
  -- ^ boolean assignment expression
  , assignRef   :: AG.Scope -> AG.IExpr -> AG.IExpr -> a -> a
  -- ^ reference assignment expression
  , booleanExpr :: AG.Scope -> AG.BExpr -> a -> a
  -- ^ boolean expression (in a condition)
  , skip        :: AG.Scope -> a -> a
  -- ^ skip statement
  , procEntry   :: AG.Scope -> a -> a
  -- ^ proc "is" block
  , procExit    :: AG.Scope -> a -> a
  -- ^ proc "end" block
  , callIn      :: AG.Scope -> AG.Proc' -> AG.Exprs -> AG.Var -> a -> a
  -- ^ call transfer function (flow to procedure)
  -- This is for "call" when doing a forward analysis and for "return" when doing a backwards analysis
  , callOut     :: AG.Scope -> AG.Proc' -> AG.Exprs -> AG.Var -> a -> a -> a
  -- ^ call transfer function (flow from procedure)
  -- This is for "return" when doing a forward analysis and for "call" when doing a backwards analysis
  , malloc      :: AG.Scope -> AG.Var -> AG.IExpr -> a -> a
  -- ^ allocation of memory
  , free        :: AG.Scope -> AG.IExpr -> a -> a
  -- ^ freeing memory
  , continue    :: AG.Scope -> a -> a
  -- ^ continuing with next loop iteration
  , breakLoop   :: AG.Scope -> a -> a
  -- ^ breaking out of loop
  }

data Blueprint a = Blueprint
  { direction         :: Direction
  , lattice           :: MF.Lattice a
  , extremalValue     :: a
  , transferFunctions :: TransferFunctions a
  }

defaultTransfer :: TransferFunctions a
defaultTransfer = TransferFunctions
  { assignIExpr = \_ _ _ -> id
  , booleanExpr = \_ _ -> id
  , skip        = \_ -> id
  , procEntry   = \_ -> id
  , procExit    = \_ -> id
  , callIn      = \_ _ _ _ -> id
  , callOut     = error "no sensible default for function return"
  , assignBExpr = \_ _ _ -> id
  , assignRef   = \_ _ _ -> id
  , malloc      = \_ _ _ -> id
  , free        = \_ _ -> id
  , continue    = \_ -> id
  , breakLoop   = \_ -> id
  }

-- | Takes a function generating a blueprint from the synthesized program attributes
-- and a program and returns the monotone framework described by the blueprint.
buildMonotoneFramework :: (AG.Syn_Program' -> Blueprint a) -> AG.Program' -> MF.MF AG.Label a
buildMonotoneFramework mkBlueprint prog = inst where
  -- compute required information like flow, labels, etc.
  pSyn = AG.synthesize prog
  
  Blueprint {..} = mkBlueprint pSyn
  TransferFunctions {..} = transferFunctions
  
  -- helper function to lookup procedures
  findProc p = AG.findProc p (AG.procs_Program'_Program' prog)
  -- helper functions to get information about scopes
  scopeOf lbl = AG.labelScope_Syn_Program' pSyn Map.! lbl
  
  -- Maps each block to its unary transfer function and the label its assigned to
  unaryBlockTransfer (AG.BBlock lbl bexpr)           = (lbl, booleanExpr (scopeOf lbl) bexpr)
  unaryBlockTransfer (AG.SkipBlock lbl)              = (lbl, skip (scopeOf lbl))
  unaryBlockTransfer (AG.IAssignBlock lbl var iexpr) = (lbl, assignIExpr (scopeOf lbl) var iexpr)
  unaryBlockTransfer (AG.ProcBeginBlock lbl)         = (lbl, procEntry (scopeOf lbl))
  unaryBlockTransfer (AG.ProcEndBlock lbl)           = (lbl, procExit (scopeOf lbl))
  unaryBlockTransfer (AG.CallBlock lblcall lblret procName args retVar) =
    let lbl = if direction == Forward then lblcall else lblret
    in (lbl, callIn (scopeOf lbl) (findProc procName) args retVar)
  
  unaryBlockTransfer (AG.BAssignBlock lbl var bexpr)    = (lbl, assignBExpr (scopeOf lbl) var bexpr)
  unaryBlockTransfer (AG.MallocBlock lbl var size)      = (lbl, malloc (scopeOf lbl) var size)
  unaryBlockTransfer (AG.FreeBlock lbl addr)            = (lbl, free (scopeOf lbl) addr)
  unaryBlockTransfer (AG.RefAssignBlock lbl addr iexpr) = (lbl, assignRef (scopeOf lbl) addr iexpr)
  unaryBlockTransfer (AG.ContinueBlock lbl)             = (lbl, continue (scopeOf lbl))
  unaryBlockTransfer (AG.BreakBlock lbl)                = (lbl, breakLoop (scopeOf lbl))
  
  -- | Returns for each block its binary transfer function.
  binaryBlockTransfer (AG.CallBlock lcall lret procName args retVar) =
    let lbl = if direction == Forward then lret else lcall
    in Just (lbl, callOut (scopeOf lbl) (findProc procName) args retVar)
  binaryBlockTransfer _ = Nothing

  -- build a map from labels to the corresponding transfer functions
  transferMap = Map.fromList $ map unaryBlockTransfer $ AG.blocks_Syn_Program' pSyn
  returnTransferMap = Map.fromList $ mapMaybe binaryBlockTransfer $ AG.blocks_Syn_Program' pSyn
  
  inst = MF.MF
    { MF.lattice        = lattice
    , MF.extremalValue  = extremalValue
    , MF.transfer       = \l -> transferMap Map.! l
    , MF.interReturn    = \_ lret -> returnTransferMap Map.! lret
    , MF.labels         = AG.labels $ AG.blocks_Syn_Program' pSyn
    -- Depending on direction:
    , MF.flow           = (if direction == Forward then id else AG.reverseFlow) $ AG.flow_Syn_Program' pSyn
    , MF.interFlow      = (if direction == Forward then id else AG.reverseInterFlow) $ AG.interflow_Syn_Program' pSyn
    , MF.extremalLabels = if direction == Forward then [ AG.init_Syn_Program' pSyn ] else AG.finals_Syn_Program' pSyn
    }