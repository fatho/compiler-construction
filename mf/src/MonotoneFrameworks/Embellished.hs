{- | Provides means to embellish monotone frameworks with context information. 
-}
{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
module MonotoneFrameworks.Embellished where

import qualified AttributeGrammar as AG
import           MonotoneFrameworks.Description
import           MonotoneFrameworks.Lattice
import qualified Util.Printing as UPP

import qualified CCO.Printing as PP
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | A context is a function (here represented by a map) from some context type to the actual lattice 
-- of the underlying monotone framework. 
newtype Context c a = Context { getContext :: Map c a }
  deriving (Eq, Ord, Read, Show, Functor)

instance (PP.Printable c, PP.Printable a) => PP.Printable (Context c a) where
  pp = UPP.ppMapping . map UPP.ppBoth . Map.toList . getContext

-- | Encompasses the information required to embellish a monotone framework with context.
data Embellished c l a = Embellished
  { liftExtremal :: a -> Context c a
    -- ^ lifts a single value into the new context
  , procIn  :: l -> (a -> a) -> Context c a -> Context c a
    -- ^ handles procedure entry, receives the underlying transfer function for that label
  , procOut :: l -> l -> (a -> a -> a) -> Context c a -> Context c a -> Context c a
    -- ^ handles procedure exit, receives the underlying transfer function for that label
  }

-- | Build the lattice of the context type given a lattice for the underlying type.
contextLattice :: Ord c => Lattice a -> Lattice (Context c a)
contextLattice l = Lattice
  { bottom = Context Map.empty
  , join   = \a b -> Context $ Map.unionWith (join l) (getContext a) (getContext b)
  , leq    = \a b -> Map.isSubmapOfBy (leq l) (getContext a) (getContext b)
  }

-- | Collapses a context to a single value of the underlying lattice.
collapse :: Lattice a -> Context c a -> a
collapse lat = Map.foldl' (join lat) (bottom lat) . getContext 

-- | Embellishes a monotone framework with context.
embellish :: (Ord c, Ord l) => (Lattice a -> Embellished c l a) -> MF l a -> MF l (Context c a)
embellish emb mf = new where
  -- pass lattice to embellished part
  Embellished{..} = emb (lattice mf)

  -- labels which flow into a procedure
  toProc = Set.fromList $ map AG.fromOuter $ interFlow mf
  
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
    , extremalValue  = liftExtremal (extremalValue mf)
    , transfer       = embTransfer
    , interReturn    = embInterTransfer
    , labels         = labels mf
    }
