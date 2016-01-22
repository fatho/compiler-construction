{- | Defines the interface for lattices and provides some basic lattice implementations.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}
module MonotoneFrameworks.Lattice where
    
import           Control.Applicative

import qualified Data.Set as Set
import qualified Data.Map.Strict as StrictMap

import qualified CCO.Printing as PP

-- | A lattice that can be explicitly passed around.
-- We decided against modelling lattices as a type classe because sometimes
-- the concrete value for 'bottom' depends on the previous program flow. In the
-- available expression analysis for example, bottom is the set of all non-trivial
-- expressions contained in the analysed program.
-- Getting this kind of dynamic information into a type class is not possible in Haskell, or
-- at least rather inconvenient. 
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

-- | A default implementation for 'leq' depending on an implementation for 'join'.
defaultLeq :: Eq a => (a -> a -> a) -> (a -> a -> Bool)
defaultLeq ljoin x y = ljoin x y == y

-- | The lattice of sets with union.
setUnion :: Ord a => Lattice (Set.Set a)
setUnion = Lattice Set.empty Set.union Set.isSubsetOf

-- | The lattice of sets with intersection for some given bottom set. 
setIntersection :: Ord a => Set.Set a -> Lattice (Set.Set a)
setIntersection universe = Lattice universe Set.intersection (flip Set.isSubsetOf)

-- | The lattice of strict maps with union.
strictMapUnion :: Ord k => Lattice a -> Lattice (StrictMap.Map k a)
strictMapUnion inner = Lattice StrictMap.empty 
                               (StrictMap.unionWith (join inner))
                               (StrictMap.isSubmapOfBy (leq inner))

-- | The lattice of strict maps with intersection for some given bottom map. 
strictMapIntersection :: Ord k => Lattice a -> Lattice (StrictMap.Map k a)
strictMapIntersection inner = 
  Lattice StrictMap.empty 
          (StrictMap.unionWith (join inner))
          (StrictMap.isSubmapOfBy (leq inner))

-- | Lifts a flat datatype into a lattice.
data Lifted a
  = Bottom   -- ^ an artificial bottom value less than every other value in this type
  | Value !a -- ^ all values of the inner type are on the same level in the lattice, i.e. their order is incomparable
  | Top      -- ^ an artificial top value larger than every other value in this type
  deriving (Eq, Ord, Show, Read, Functor)

-- | A lifted lattice from a regular data type. The equality constraint is needed for joining.
lifted :: Eq a => Lattice (Lifted a)
lifted = Lattice Bottom liftedJoin (defaultLeq liftedJoin) where
  liftedJoin Bottom    x         = x
  liftedJoin x         Bottom    = x
  liftedJoin Top       _         = Top
  liftedJoin _         Top       = Top
  liftedJoin (Value a) (Value b) = if a == b then Value a else Top

-- Some useful instances when working with lifted values:

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

instance PP.Printable a => PP.Printable (Lifted a) where
  pp Top       = PP.text "T"
  pp (Value x) = PP.pp x
  pp Bottom    = PP.text "_|_" 
