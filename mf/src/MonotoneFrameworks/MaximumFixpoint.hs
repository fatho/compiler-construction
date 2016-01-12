{-# LANGUAGE RecordWildCards #-}
module MonotoneFrameworks.MaximumFixpoint where

import           Control.Arrow (second)
import           Control.Monad.State.Strict
import qualified Data.Map.Strict as Map
import           Data.Maybe


import           MonotoneFrameworks.Instance
import           MonotoneFrameworks.Lattice (Lattice)
import qualified MonotoneFrameworks.Lattice as Lattice

type Results l a = Map.Map l a 

lookupValue :: (Ord l, Lattice a) => Results l a -> l -> a
lookupValue res l = fromMaybe Lattice.bottom $ Map.lookup l res

mf :: (Ord l, Lattice a) => Instance l a -> Results l a
mf Instance{..} = iter transitions initialResults where
  initialResults = Map.fromList [ (e, extremalValue) | e <- extremalLabels ]
  
  successorCache = Map.fromListWith (++) $ map (second (:[])) transitions
  successors l = [ (l, l') | l' <- fromMaybe [] $ Map.lookup l successorCache ]
  
  iter [] res = res
  iter ((l,l') : w) res
      | not  (y `Lattice.leq` x) = 
          let res' = Map.insertWith Lattice.join l' y res
          in iter (successors l' ++ w) res'
      | otherwise = iter w res
    where
      f_l = transferFunction l
      x   = lookupValue res l
      y   = f_l x