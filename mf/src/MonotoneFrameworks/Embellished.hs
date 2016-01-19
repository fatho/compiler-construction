{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
module MonotoneFrameworks.Embellished where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Maybe

import AttributeGrammar (Flow(..), InterFlow(..))
import MonotoneFrameworks.Description
import MonotoneFrameworks.Lattice

import qualified CCO.Printing as PP
import qualified Util.Printing as UPP

newtype Context c a = Context { getContext :: Map c a }
  deriving (Eq, Ord, Read, Show, Functor)

instance (PP.Printable c, PP.Printable a) => PP.Printable (Context c a) where
  pp = UPP.ppMap PP.pp PP.pp . getContext

data Embellished c l a = Embellished
  { liftVal :: a -> Context c a
  , procIn  :: l -> (a -> a) -> Context c a -> Context c a
  , procOut :: l -> l -> (a -> a -> a) -> Context c a -> Context c a -> Context c a
  }

contextLattice :: Ord c => Lattice a -> Lattice (Context c a)
contextLattice l = Lattice
  { bottom = Context Map.empty
  , join   = \a b -> Context $ Map.unionWith (join l) (getContext a) (getContext b)
  , leq    = \a b -> Map.isSubmapOfBy (leq l) (getContext a) (getContext b)
  }

embellish :: (Ord c, Ord l) => (Lattice a -> Embellished c l a) -> MF l a -> MF l (Context c a)
embellish emb mf = new where
  -- pass lattice to embellished part
  Embellished{..} = emb (lattice mf)

  -- labels which flow into a procedure
  toProc = Set.fromList $ map fromOuter $ interFlow mf
  
  -- wrap normal flow (wrap calls)
  embTransfer l val
    | Set.member l toProc = procIn l (transfer mf l) val
    | otherwise           = fmap (transfer mf l) val
  
  -- wrap outgoing flow
  embInterTransfer lcall lret callVal retVal =
    procOut lcall lret (interReturn mf lcall lret) callVal retVal 
  
  new = MF 
    { lattice        = contextLattice $ lattice mf
    , flow           = flow mf
    , interFlow      = interFlow mf
    , extremalLabels = extremalLabels mf 
    , extremalValue  = liftVal (extremalValue mf)
    , transfer       = embTransfer
    , interReturn    = embInterTransfer
    , labels         = labels mf
    }
