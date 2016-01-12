{-# LANGUAGE FlexibleInstances #-}
module Util.Printing where

import Control.Arrow ((***))    
import qualified Data.Map as Map
    
import qualified CCO.Printing as PP

ppMapping :: [(PP.Doc, PP.Doc)] -> PP.Doc
ppMapping mapping = flat PP.>//< list where
  flat = PP.brackets $ PP.sepBy (map ppAssign mapping) (PP.comma PP.>|< PP.space)
  list = PP.above (map ppAssign mapping)
  ppAssign (k,v) = k PP.>#< PP.text "|->" PP.>#< v

ppMap :: Map.Map a b -> (a -> PP.Doc) -> (b -> PP.Doc) -> PP.Doc
ppMap m pa pb = ppMapping $ map (pa *** pb) $ Map.toList m

ppBoth :: (PP.Printable a, PP.Printable b) => (a, b) -> (PP.Doc, PP.Doc)
ppBoth (x,y) = (PP.pp x, PP.pp y)

instance PP.Printable [Char] where
  pp = PP.text
