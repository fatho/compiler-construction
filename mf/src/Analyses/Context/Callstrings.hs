{- | This module provides functionality to extend a monotone framework with call string context.
-}
{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving #-}
module Analyses.Context.Callstrings where

import qualified Data.Map as Map

import MonotoneFrameworks.Embellished
import MonotoneFrameworks.Lattice

import qualified CCO.Printing as PP

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
  pushContext l = Context . Map.insertWith join callRoot bottom . Map.mapKeysWith join (pushCall k l) . getContext
  lookupCtx cs = Map.findWithDefault bottom cs . getContext

  pin lblCall transfer = pushContext lblCall . fmap transfer
  pout lblCall _ transfer ctxBefore ctxProc = 
    Context $ Map.mapWithKey (\c v -> transfer v (lookupCtx (pushCall k lblCall c) ctxProc)) (getContext ctxBefore)

instance PP.Printable l => PP.Printable (CallString l) where
  pp cs = PP.brackets (PP.sepBy (map PP.pp $ getCallString cs) (PP.text ", "))
