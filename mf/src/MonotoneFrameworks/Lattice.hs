{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}
module MonotoneFrameworks.Lattice where
    
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
  deriving (Eq, Show, Read, Functor)

instance Applicative Lifted where
  pure = Value
  (Value f) <*> (Value x) = Value (f x)
  Top       <*> _         = Top
  _         <*> Top       = Top
  _         <*> _         = Bottom

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
  join a b = Function fun' bottom' where
    fun' = Map.mergeWithKey combine onlya onlyb (functionSpecial a) (functionSpecial b) 
    combine _ x y = Just $ join x y
    onlya = fmap (join (functionDefault b))
    onlyb = fmap (join (functionDefault a))
    bottom' = join (functionDefault a) (functionDefault b)

instance Show a => PP.Printable (Lifted a) where
  pp = PP.showable

instance (PP.Printable a, PP.Printable b) => PP.Printable (Function a b) where
  pp (Function spec def) = UPP.ppMapping  (map UPP.ppBoth (Map.toList spec)  ++ [(PP.text "_", PP.pp def)]) where
