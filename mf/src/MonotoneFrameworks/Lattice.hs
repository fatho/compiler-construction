{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MonotoneFrameworks.Lattice where
    
import qualified Data.Set as Set

-- | Describes the operations on a 
class Eq a => Lattice a where
  -- | Returns the bottom value in the lattice.
  bottom :: a
  -- | Computes the join of two values in the lattice.
  join   :: a -> a -> a
  -- | @a `leq` b@ returns whether @a@ is less than or equal to @b@.
  -- The default definition is implemented in terms of join and might be
  -- overriden by a more efficient implementation if necessary.
  leq    :: a -> a -> Bool
  leq x y = join x y == y

instance Ord a => Lattice (Set.Set a) where
  bottom = Set.empty
  join   = Set.union
  leq    = Set.isSubsetOf
