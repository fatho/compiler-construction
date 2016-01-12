module MonotoneFrameworks.Lattice where
    
class Lattice a where
  -- | Returns the bottom value in the lattice.
  bottom :: a
  -- | Computes the join of two values in the lattice.
  join   :: a -> a -> a
  -- | @a `leq` b@ returns whether @a@ is less than or equal to @b@.
  leq    :: a -> a -> Bool