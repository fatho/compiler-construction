{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}
module MonotoneFrameworks.Lattice where
    
import           Control.Applicative

import qualified Data.Set as Set
import qualified Data.Map.Strict as StrictMap

import           Data.Maybe (fromMaybe)

import qualified CCO.Printing as PP
import qualified Util.Printing as UPP

-- | A lattice that can be explicitly passed around.
data Lattice a = Lattice
  { bottom :: a
    -- ^ the bottom value in the lattice.
  , join   :: a -> a -> a
    -- ^ Computes the join of two values in the lattice.
  , leq    :: a -> a -> Bool
    -- @a `leq` b@ returns whether @a@ is less than or equal to @b@.
    -- The default definition is implemented in terms of 'join' and might be
    -- overriden by a more efficient implementation if necessary.
  }

defaultLeq :: Eq a => (a -> a -> a) -> (a -> a -> Bool)
defaultLeq ljoin x y = ljoin x y == y

setUnion :: Ord a => Lattice (Set.Set a)
setUnion = Lattice Set.empty Set.union Set.isSubsetOf

setIntersection :: Ord a => Set.Set a -> Lattice (Set.Set a)
setIntersection universe = Lattice universe Set.intersection (flip Set.isSubsetOf)

strictMapUnion :: Ord k => Lattice a -> Lattice (StrictMap.Map k a)
strictMapUnion inner = Lattice StrictMap.empty 
                               (StrictMap.unionWith (join inner))
                               (StrictMap.isSubmapOfBy (leq inner))

strictMapIntersection :: Ord k => Lattice a -> Lattice (StrictMap.Map k a)
strictMapIntersection inner = 
  Lattice StrictMap.empty 
          (StrictMap.unionWith (join inner))
          (StrictMap.isSubmapOfBy (leq inner))

-- | Lifts a flat datatype into a lattice.
data Lifted a
  = Bottom
  | Value !a
  | Top
  deriving (Eq, Ord, Show, Read, Functor)

lifted :: Eq a => Lattice (Lifted a)
lifted = Lattice Bottom liftedJoin (defaultLeq liftedJoin) where
  liftedJoin Bottom    x         = x
  liftedJoin x         Bottom    = x
  liftedJoin Top       _         = Top
  liftedJoin _         Top       = Top
  liftedJoin (Value a) (Value b) = if a == b then Value a else Top

instance Applicative Lifted where
  pure = Value
  (Value f) <*> (Value x) = Value (f x)
  Top       <*> _         = Top
  _         <*> Top       = Top
  _         <*> _         = Bottom

instance Num a => Num (Lifted a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  negate = fmap negate
  fromInteger = Value . fromInteger

instance Real a => Real (Lifted a) where
  toRational Bottom    = error "bottom is not a rational"
  toRational (Value v) = toRational v
  toRational Top       = error "top is not a rational"

-- | Why on earth is "Enum" a requirement for Integral? We actually just need the 'div' operation.
instance Enum a => Enum (Lifted a) where
  fromEnum Bottom    = error "bottom is not enumerable"
  fromEnum (Value v) = fromEnum v
  fromEnum Top       = error "top is not enumerable"
  toEnum = Value . toEnum

instance Integral a => Integral (Lifted a) where
  toInteger Bottom    = error "bottom is not an integer"
  toInteger (Value v) = toInteger v
  toInteger Top       = error "top is not an integer"
  quotRem x y         = 
    case liftA2 quotRem x y of
      Bottom -> (Bottom, Bottom)
      Value (a,b) -> (Value a, Value b)
      Top -> (Top, Top)

instance Show a => PP.Printable (Lifted a) where
  pp Top       = PP.text "T"
  pp (Value x) = PP.showable x
  pp Bottom    = PP.text "_|_" 
