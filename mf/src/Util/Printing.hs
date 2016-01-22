{-| Contains some utility function for pretty-printing stuff.
-}
{-# LANGUAGE FlexibleInstances #-}
module Util.Printing where

import qualified Data.Set as Set
    
import qualified CCO.Printing as PP

-- | Renders a mapping of values either as a single line or as list.
ppMapping :: [(PP.Doc, PP.Doc)] -> PP.Doc
ppMapping m = ppMappingFlat m PP.>//< ppMappingList m

-- | Renders a mapping of values as a single line.
ppMappingFlat :: [(PP.Doc, PP.Doc)] -> PP.Doc
ppMappingFlat mapping = PP.brackets $ PP.sepBy (map ppAssign mapping) (PP.text ", ")

-- | Renders an assignment of a value.
ppAssign :: (PP.Doc, PP.Doc) -> PP.Doc
ppAssign (k,v) = k PP.>|< PP.text " = " PP.>|< v
  
-- | Renders a mapping of values as a list.
ppMappingList :: [(PP.Doc, PP.Doc)] -> PP.Doc
ppMappingList mapping = list where
  list = PP.above (map ppAssign mapping)

-- | Pretty-prints both values of a pair.
ppBoth :: (PP.Printable a, PP.Printable b) => (a, b) -> (PP.Doc, PP.Doc)
ppBoth (x,y) = (PP.pp x, PP.pp y)

instance PP.Printable [Char] where
  pp = PP.showable

instance Show a => PP.Printable (Set.Set a) where
  pp = PP.showable
