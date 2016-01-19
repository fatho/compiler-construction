{-# LANGUAGE FlexibleInstances #-}
module Util.Printing where

import Control.Arrow ((***))    
import qualified Data.Map as Map
import qualified Data.Set as Set
    
import qualified CCO.Printing as PP

ppMapping :: [(PP.Doc, PP.Doc)] -> PP.Doc
ppMapping mapping = flat PP.>//< list where
  flat = PP.brackets $ PP.sepBy (map ppAssign mapping) (PP.comma PP.>|< PP.space)
  list = PP.above (map ppAssign mapping)
  ppAssign (k,v) = k PP.>#< PP.text "|->" PP.>#< v

ppMap :: (a -> PP.Doc) -> (b -> PP.Doc) -> Map.Map a b -> PP.Doc
ppMap pa pb m = ppMapping $ map (pa *** pb) $ Map.toList m

ppBoth :: (PP.Printable a, PP.Printable b) => (a, b) -> (PP.Doc, PP.Doc)
ppBoth (x,y) = (PP.pp x, PP.pp y)

instance PP.Printable [Char] where
  pp = PP.showable
 
instance Show a => PP.Printable (Set.Set a) where
  pp = PP.showable
