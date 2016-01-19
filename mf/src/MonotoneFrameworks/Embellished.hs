{-# LANGUAGE RecordWildCards #-}
module MonotoneFrameworks.Embellished where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Maybe

import MonotoneFrameworks.MaximumFixpoint

type Context = Map

data Embellished c l a = Embellished
  { liftVal :: a -> Context c a
  , procIn  :: l -> (a -> a) -> Context c a -> Context c a
  , procOut :: l -> l -> (a -> a -> a) -> Context c a -> Context c a -> Context c a
  }

type CallString l = [l]

contextLattice :: Ord c => Lattice a -> Lattice (Context c a)
contextLattice l = Lattice
  { bottom = Map.empty
  , join   = Map.unionWith (join l)
  , leq    = Map.isSubmapOfBy (leq l)
  }

callstrings :: Ord l => Int -> Lattice a -> Embellished (CallString l) l a
callstrings k Lattice{..} = Embellished (Map.singleton []) pin pout where
  pushCall l = take k . (l:)
  pushContext l = Map.insert [] bottom . Map.mapKeysWith join (pushCall l)
  lookup cs = fromMaybe bottom . Map.lookup cs

  pin lblCall transfer = pushContext lblCall . fmap transfer
  pout lblCall lblRet transfer ctxBefore ctxProc = Map.mapWithKey (\k v -> transfer v (lookup (pushCall lblCall k) ctxProc)) ctxBefore

embellish :: (Ord c, Ord l) => (Lattice a -> Embellished c l a) -> MF l a -> MF l (Context c a)
embellish emb mf = new where
  -- pass lattice to embellished part
  Embellished{..} = emb (lattice mf)

  -- labels which flow into a procedure
  toProc = Set.fromList $ map toOuter $ interFlow mf
  
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