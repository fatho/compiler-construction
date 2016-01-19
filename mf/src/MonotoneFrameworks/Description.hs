{-# LANGUAGE DeriveFunctor #-}
module MonotoneFrameworks.Description where

import qualified Data.Map.Strict as StrictMap

import MonotoneFrameworks.Lattice
import AttributeGrammar (Flow(..), InterFlow(..))

import qualified CCO.Printing as PP
import qualified Util.Printing as UPP

type UnaryTransfer a = a -> a
type BinaryTransfer a = a -> a -> a

data AnyTransfer a = Unary (UnaryTransfer a) | Binary (BinaryTransfer a)

-- | An instance of a monotone framework.
data MF l a = MF 
  { lattice        :: Lattice a
  , flow           :: [Flow l]
  , interFlow      :: [InterFlow l]
  , extremalLabels :: [l]
  , extremalValue  :: a
  , transfer       :: l -> UnaryTransfer a
  , interReturn    :: l -> l -> BinaryTransfer a
  , labels         :: [l]
  }

-- | The fixpoint of a monotone framework.
data Fixpoint l a = Fixpoint 
  { contextValues :: StrictMap.Map l a
  , effectValues  :: StrictMap.Map l a
  }
  deriving (Show, Read, Eq, Ord, Functor)

instance (PP.Printable l, PP.Printable a) => PP.Printable (Fixpoint l a) where
  pp (Fixpoint open closed) = 
      PP.text "Context Values: "
      PP.>-<
      PP.indent 2 (UPP.ppMap PP.pp PP.pp open)
      PP.>-<
      PP.text "Effect Values: "
      PP.>-<
      PP.indent 2 (UPP.ppMap PP.pp PP.pp closed)