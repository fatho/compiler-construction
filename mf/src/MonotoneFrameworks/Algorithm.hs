{- | Implements the Maximum Fixed Point (MFP) algorithm for monotone frameworks.
-}
{-# LANGUAGE RecordWildCards, BangPatterns #-}
module MonotoneFrameworks.Algorithm where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe

import AttributeGrammar (Flow(..), InterFlow(..))

import MonotoneFrameworks.Description
import MonotoneFrameworks.Lattice

import Debug.Trace

-- | Takes a monotone framework and returns the corresponding fixpoint.
maximumFixedPoint :: (Show l, Ord l) => MF l a ->  Fixpoint l a
maximumFixedPoint mf = mfp where
  -- bring lattice into scope
  Lattice{..} = lattice mf
  
  -- maps from return point to the full inter-procedure flow
  -- since call targets are static (meaning the block a return label belongs 
  -- to always has called the same procedure), this is indeed a unique mapping
  interReturnMap = Map.fromList [ ( toOuter ifl, ifl ) | ifl <- interFlow mf ]
  
  -- cache outgoing edges
  outgoingMap = Map.fromListWith (++) $ [ ( from f, [f] ) | f <- flow mf ]
  outgoing l  = Map.findWithDefault [] l outgoingMap 
  -- additionally propagating to outgoing edges, changes to the call label of a block
  -- affect the successors of the return label
  propagateMap = Map.unionWith (++) outgoingMap $ Map.fromList
    [ (fromOuter ifl, outgoing (toOuter ifl)) | ifl <- interFlow mf ]
  propagate l = Map.findWithDefault [] l propagateMap
  
  lookupSolution l = Map.findWithDefault (error $ "solution for label " ++ show l ++ " not found") l
  
  -- 1. initial solution
  initialSolution = Map.fromList
    $  zip (labels mf) (repeat bottom)
    ++ zip (extremalLabels mf) (repeat $ extremalValue mf) -- last values take precedence

  computeEffect lbl solution = case Map.lookup lbl interReturnMap of
    -- not a return, just transfer context value at "from" label to effect value
    Nothing -> transfer mf lbl (lookupSolution lbl solution)
    -- flowing out of return value: apply binary transfer function (lret == from f)
    Just (InterFlow lcall lentry lexit lret) -> 
      interReturn mf lcall lret (lookupSolution lcall solution) (lookupSolution lret solution)

  iter []     solution = solution -- no more edges to process, we're done
  iter (f:fs) solution =
    let toCtxVal   = solution Map.! (to f)           -- context value of successor
        fromEffVal = computeEffect (from f) solution
    in if fromEffVal `leq` toCtxVal -- is effect consistent?
         then iter fs solution
         else let newToCtxVal = toCtxVal `join` fromEffVal
              in iter (propagate (to f) ++ fs) (Map.insert (to f) newToCtxVal solution)
  
  -- run fixpoint iteration            
  mfp' = iter (flow mf) initialSolution
  
  mfp = Fixpoint mfp' (Map.mapWithKey (\l _ -> computeEffect l mfp') mfp')
