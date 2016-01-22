{- | Contains the types used to describe a monotone framework and its fixpoint.
-}
{-# LANGUAGE DeriveFunctor #-}
module MonotoneFrameworks.Description where

import qualified Data.Map.Strict as StrictMap

import MonotoneFrameworks.Lattice
import AttributeGrammar (Flow(..), InterFlow(..))

import qualified CCO.Printing as PP
import qualified Util.Printing as UPP

-- | Signature of a unary transfer function
type UnaryTransfer a = a -> a
-- | Signature of a binary transfer function
type BinaryTransfer a = a -> a -> a

-- | An instance of a monotone framework.
data MF l a = MF 
  { lattice        :: Lattice a
  -- ^ the underlying lattice of the monotone framework 
  , flow           :: [Flow l]
  -- ^ the flow between labels
  , interFlow      :: [InterFlow l]
  -- ^ the interprocedural flow
  , extremalLabels :: [l]
  -- ^ extremal labels of the flow graph
  , extremalValue  :: a
  -- ^ value for extremal labels
  , transfer       :: l -> UnaryTransfer a
  -- ^ returns the corresponding unary transfer function of a label 
  , interReturn    :: l -> l -> BinaryTransfer a
  -- ^ returns the corresponding binary transfer function to handle leaving a procedure
  , labels         :: [l]
  -- ^ all the labels in the monotone framework
  }

-- | The fixpoint of a monotone framework.
data Fixpoint l a = Fixpoint 
  { contextValues :: StrictMap.Map l a
  -- ^ the context values of the labels (the values they receive from their predecessors)
  , effectValues  :: StrictMap.Map l a
  -- ^ the effect values of the labels (after applying the transfer function)
  }
  deriving (Show, Read, Eq, Ord, Functor)

instance (PP.Printable l, PP.Printable a) => PP.Printable (Fixpoint l a) where
  pp (Fixpoint open closed) = 
      PP.text "Context Values: "
      PP.>-<
      PP.indent 2 (UPP.ppMappingList $ map UPP.ppBoth $ StrictMap.toList open)
      PP.>-<
      PP.text "Effect Values: "
      PP.>-<
      PP.indent 2 (UPP.ppMappingList $ map UPP.ppBoth $ StrictMap.toList closed)