{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}
module MonotoneFrameworks.Lattice where
    
import           Control.Applicative
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)

import qualified CCO.Printing as PP
import qualified Util.Printing as UPP

  -- | Describes the operations of a join semi-lattice 
class Eq a => JoinSemiLattice a where
  -- | Returns the bottom value in the lattice.
  bottom :: a
  -- | Computes the join of two values in the lattice.
  join   :: a -> a -> a
  -- | @a `leq` b@ returns whether @a@ is less than or equal to @b@.
  -- The default definition is implemented in terms of 'join' and might be
  -- overriden by a more efficient implementation if necessary.
  leq    :: a -> a -> Bool
  leq x y = join x y == y


-- | Describes the operations of a meet semi-lattice 
class Eq a => MeetSemiLattice a where
  -- | Returns the bottom value in the lattice.
  top :: a
  -- | Computes the meet of two values in the lattice.
  meet   :: a -> a -> a
  -- | @a `geq` b@ returns whether @a@ is greater than or equal to @b@.
  -- The default definition is implemented in terms of 'meet' and might be
  -- overriden by a more efficient implementation if necessary.
  geq    :: a -> a -> Bool
  geq x y = meet x y == y

instance Ord a => JoinSemiLattice (Set.Set a) where
  bottom = Set.empty
  join   = Set.union
  leq    = Set.isSubsetOf

-- | Lifts a flat datatype into a lattice.
data Lifted a
  = Bottom
  | Value a
  | Top
  deriving (Eq, Ord, Show, Read, Functor)

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

instance Eq a => JoinSemiLattice (Lifted a) where
  bottom = Bottom
  join Bottom    x         = x
  join x         Bottom    = x
  join Top       _         = Top
  join _         Top       = Top
  join (Value a) (Value b) = if a == b then Value a else Top

instance Eq a => MeetSemiLattice (Lifted a) where
  top = Top
  meet Top       x         = x
  meet x         Top       = x
  meet Bottom    _         = Bottom
  meet _         Bottom    = Bottom
  meet (Value a) (Value b) = if a == b then Value a else Top

-- | Representation of a function as a lattice.
-- The domain @a@ of the function may be infinite, but the
-- function has to be almost constant (i.e. for all but finitely many inputs).
data Function a b = Function 
  { functionSpecial :: Map.Map a b
  , functionDefault :: b 
  } deriving (Eq, Functor, Show)

-- | Applies a represented function to a value.
functionApply :: (Ord a) => Function a b -> a -> b
functionApply (Function spec def) x = fromMaybe def $ Map.lookup x spec 

-- | Sets a specific mapping of a represented function.
functionSet :: (Ord a) => a -> b -> Function a b -> Function a b
functionSet k v (Function spec def) = Function (Map.insert k v spec) def

instance Ord a => Applicative (Function a) where
  pure x = Function Map.empty x
  (Function fa ba) <*> (Function fb bb) = Function newfn (ba bb) where
    newfn = Map.mergeWithKey combine onlya onlyb fa fb 
    combine _ x y = Just $ x y
    onlya = fmap ($ bb)
    onlyb = fmap (ba $)  

instance (Ord a, JoinSemiLattice b) => JoinSemiLattice (Function a b) where
  bottom = Function Map.empty bottom
  join = liftA2 join

instance Show a => PP.Printable (Lifted a) where
  pp = PP.showable

instance (PP.Printable a, PP.Printable b) => PP.Printable (Function a b) where
  pp (Function spec def) = UPP.ppMapping  (map UPP.ppBoth (Map.toList spec)  ++ [(PP.text "_", PP.pp def)]) where
