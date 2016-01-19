{-# LANGUAGE RecordWildCards, BangPatterns #-}
module MonotoneFrameworks.MaximumFixpoint where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
{-
import qualified CCO.Printing as PP
import qualified Util.Printing as UPP
-}
data Lattice a = Lattice
  { bottom :: a
  , join   :: a -> a -> a
  , leq    :: a -> a -> Bool
  }
  
data Flow l = Flow { from :: l, to :: l }
  deriving (Eq, Ord, Show, Read)

data InterFlow l = InterFlow { fromOuter, toProc, fromProc, toOuter :: l }
  deriving (Eq, Ord, Show, Read)
  
callFlow :: InterFlow l -> Flow l
callFlow InterFlow{..} = Flow fromOuter toProc
  
returnFlow :: InterFlow l -> Flow l
returnFlow InterFlow{..} = Flow fromProc toOuter

data MF l a = MF 
  { lattice        :: Lattice a
  , flow           :: [Flow l]
  , interFlow      :: [InterFlow l]
  , extremalLabels :: [l]
  , extremalValue  :: a
  , transfer       :: l -> (a -> a)
  , interReturn    :: l -> l -> (a -> a -> a)
  , labels         :: [l]
  }

data Fixpoint l a = Fixpoint 
  { contextValues :: Map l a
  , effectValues  :: Map l a
  }

fixpoint :: Ord l => MF l a ->  Fixpoint l a
fixpoint mf = mfp where
  -- bring lattice into scope
  Lattice{..} = lattice mf
  
  -- maps from return point to the full inter-procedure flow
  -- since call targets are static (meaning the block a return label belongs 
  -- to always has called the same procedure), this is indeed a unique mapping
  interReturnMap = Map.fromList [ ( toOuter ifl, ifl ) | ifl <- interFlow mf ]
  
  -- cache outgoing edges
  outgoingMap = Map.fromListWith (++) $ [ ( from f, [f] ) | f <- flow mf ]
  outgoing l  = outgoingMap Map.! l 
  -- additionally propagating to outgoing edges, changes to the call label of a block
  -- affect the successors of the return label
  propagateMap = Map.unionWith (++) outgoingMap $ Map.fromList
    [ (fromOuter ifl, outgoing (toOuter ifl)) | ifl <- interFlow mf ]
  propagate l = propagateMap Map.! l
  
  -- 1. initial solution
  initialSolution = Map.fromListWith (curry fst) -- first occurrence of label takes precedence
    $  zip (extremalLabels mf) (repeat $ extremalValue mf) 
    ++ zip (labels mf) (repeat bottom)

  iter []     solution = solution -- no more edges to process, we're done
  iter (f:fs) solution =
    let toCtxVal   = solution Map.! (to f)           -- context value of successor
        fromEffVal = case Map.lookup (from f) interReturnMap of
          -- not a return, just transfer context value at "from" label to effect value
          Nothing -> transfer mf (from f) (solution Map.! from f)
          -- flowing out of return value: apply binary transfer function (lret == from f)
          Just (InterFlow lcall lentry lexit lret) -> 
            interReturn mf lcall lret (solution Map.! lcall) (solution Map.! lret)
    in if fromEffVal `leq` toCtxVal -- is effect consistent?
         then iter fs solution
         else let newToCtxVal = toCtxVal `join` fromEffVal
              in iter (propagate (to f) ++ fs) (Map.insert (to f) newToCtxVal solution)
  
  -- run fixpoint iteration            
  mfp' = iter (flow mf) initialSolution
  
  mfp = Fixpoint mfp' (Map.mapWithKey (transfer mf) mfp')

{-
instance (PP.Printable l, PP.Printable a) => PP.Printable (Fixpoint l a) where
  pp (Fixpoint open closed) = 
      PP.text "Context Values: "
      PP.>-<
      PP.indent 2 (UPP.ppMap open PP.pp PP.pp)
      PP.>-<
      PP.text "Effect Values: "
      PP.>-<
      PP.indent 2 (UPP.ppMap closed PP.pp PP.pp)
      -}