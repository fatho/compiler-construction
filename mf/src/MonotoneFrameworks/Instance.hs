module MonotoneFrameworks.Instance where
    
data Instance l a = Instance 
  { transferFunction :: l -> (a -> a)
  , transitions      :: [(l,l)]
  , extremalLabels   :: [l]
  , extremalValue    :: a
  }
