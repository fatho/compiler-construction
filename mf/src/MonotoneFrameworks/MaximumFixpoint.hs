{-# LANGUAGE RecordWildCards, BangPatterns #-}
module MonotoneFrameworks.MaximumFixpoint where

import qualified Data.Map.Strict as Map
import           Data.Maybe

import           MonotoneFrameworks.Instance
import           MonotoneFrameworks.Lattice (Lattice)
import qualified MonotoneFrameworks.Lattice as Lattice

type Results l a = Map.Map l a

data MFP l a = MFP
  { mfpOpen   :: Results l a
  , mfpClosed :: Results l a
  }
  deriving (Show)

lookupValue :: (Ord l, Lattice a) => l -> Results l a -> a
lookupValue l res = fromMaybe Lattice.bottom $ Map.lookup l res

mf :: (Ord l, Lattice a) => Instance l a -> MFP l a
mf Instance{..} = mfpResults where
  -- initial results have extremal value for extremal labels and
  -- bottom (represented by not being present in the map) at other labels
  initialResults = Map.fromList [ (e, extremalValue) | e <- extremalLabels ]
  
  -- build map from label to its outgoing flow
  successorCache = Map.fromListWith (++) $ [ (l, [(l, l')]) | (l, l') <- transitions ]
  -- returns the outgoing flow edges for a label
  successorFlow l = fromMaybe [] $ Map.lookup l successorCache
    
  iterFun [] !res = res -- no more edges to process, we're done
  iterFun ((l,l') : w) !res
      -- if not inconsistent...
      | not  (tval_l `Lattice.leq` val_l') =
          -- incorporate result (the implementation exploits that bottom is the identity for join)
          let res' = Map.insertWith Lattice.join l' tval_l res
          -- and enlist successors for update
          in iterFun (successorFlow l' ++ w) res'
       
      | otherwise = iterFun w res
    where
      f_l    = transferFunction l
      val_l  = lookupValue l res  -- current value at l
      val_l' = lookupValue l' res -- current value at l'
      tval_l = f_l val_l          -- transferred value at l

  -- performs all iterations until a fixpoint is reached
  finalResults = iterFun transitions initialResults
  
  -- final step of the algorithm
  mfpResults = MFP
    { mfpOpen   = finalResults
    , mfpClosed = Map.fromList 
        [ (l, closed) | l <- labels
                     , let open = lookupValue l finalResults
                     , let closed = transferFunction l open
                     , closed /= Lattice.bottom ]
    }
