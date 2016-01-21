{- | This module provides functionality to extend a monotone framework with call string context.
-}
{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving #-}
module Analyses.Context.Callstrings where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Maybe

import AttributeGrammar (Flow(..), InterFlow(..))
import MonotoneFrameworks.Description
import MonotoneFrameworks.Embellished
import MonotoneFrameworks.Lattice

import qualified CCO.Printing as PP
import qualified Util.Printing as UPP

import Debug.Trace

-- | Type of a callstring parameterized by the type of labels.
newtype CallString l = CallString { getCallString :: [l] }
  deriving (Eq, Ord, Read, Show, Functor)

-- | The root call string (i.e. the empty call string)
callRoot :: CallString l
callRoot = CallString []

-- | @pushCall k l cs@ pushes a call at label @l@ to the string @cs@ while retaining at most the latest @k@ call sites.
pushCall :: Int -> l -> CallString l -> CallString l
pushCall k l = CallString . take k . (l:) . getCallString 

-- | Provides the function to enhance a given lattice and monotone framework with call strings.
callstrings :: Ord l => Int -> Lattice a -> Embellished (CallString l) l a
callstrings k Lattice{..} = Embellished (Context . Map.singleton callRoot) pin pout where
  pushContext l = Context . Map.insert callRoot bottom . Map.mapKeysWith join (pushCall k l) . getContext
  lookup cs = Map.findWithDefault bottom cs . getContext

  pin lblCall transfer = trace "call" $ pushContext lblCall . fmap transfer
  pout lblCall lblRet transfer ctxBefore ctxProc = trace "ret" $ 
    Context $ Map.mapWithKey (\c v -> transfer v (lookup (pushCall k lblCall c) ctxProc)) (getContext ctxBefore)

instance PP.Printable l => PP.Printable (CallString l) where
  pp cs = PP.brackets (PP.sepBy (map PP.pp $ getCallString cs) (PP.text ", "))
