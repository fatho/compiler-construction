{-# LANGUAGE OverloadedStrings #-}
module Util.Visual where

import Data.Monoid

import qualified Data.List as List
import qualified Data.Map.Strict as Map 
import qualified Data.Set as Set 
import qualified AttributeGrammar as AG
import Data.Text (Text)
import qualified Data.Text as T

type Attributes = Map.Map Text Text

attrText :: Attributes -> Text
attrText attrs = "[" <> Map.foldMapWithKey ppattr attrs <> "]" where
  ppattr k v = k <> "= \"" <> T.replace "\"" "\\\"" v <> "\", "

tshow :: Show a => a -> Text
tshow = T.pack . show

replaceAngleBrackets :: Text -> Text
replaceAngleBrackets = T.replace "<" "&lt;" . T.replace ">" "&gt;"

blockLabel :: AG.Block -> Text
blockLabel (AG.BBlock l expr)           = "<code> " 
  <> replaceAngleBrackets (T.pack $ AG.pretty_Syn_BExpr $ AG.synthesize expr) 
  <> "|<label> " <> tshow l
blockLabel (AG.SkipBlock l)             = "<code> skip|<label> " <> tshow l
blockLabel (AG.IAssignBlock l var expr) = "<code> " <> T.pack var <> " = "
  <> replaceAngleBrackets (T.pack $ AG.pretty_Syn_IExpr $ AG.synthesize expr) 
  <> "|<label> " <> tshow l 
blockLabel (AG.ProcBeginBlock l)        = "<code> is|<label> " <> tshow l
blockLabel (AG.ProcEndBlock l)          = "<code> end|<label> " <> tshow l
blockLabel (AG.CallBlock lc lr n es o)  = "<code> " <> T.pack n <> "(" 
  <> foldr (\e rs -> replaceAngleBrackets (T.pack $ AG.pretty_Syn_Expr $ AG.synthesize e) 
                      <> ", " <> rs
           ) (T.pack o) es 
  <> ")|{<labelcall> " <> tshow lc <> "|<labelret> " <> tshow lr <> "}"  

highlightExtremal :: [AG.Label] -> AG.Block -> Attributes
highlightExtremal extremal blk
  | isExtremal = Map.fromList [("style", "bold"), ("shape", "Mrecord")]
  | otherwise  = Map.empty 
  where
    isExtremal = any (`elem` extremal) (AG.labelsFromBlock blk)

codeOnly :: AG.Block -> Attributes
codeOnly blk = Map.fromList [("label", blockLabel blk), ("shape","record")]

makeDot :: [AG.Block] -> [AG.Flow AG.Label] -> (AG.Block -> Attributes) -> Text -> Text
makeDot blocks flow attrs graphName = T.unlines graphLines where
  blockMap = Map.fromList [ (l, blk) | blk <- blocks, l <- AG.labelsFromBlock blk ]
  -- returns the label that is used as ID for the block
  idLabel = head . AG.labelsFromBlock
 
  nodeDefs = [ "n" <> tshow (idLabel blk) <> attrText (attrs blk) <> ";" | blk <- blocks ]
  
  showFlow from to = case (blockMap Map.! from, blockMap Map.! to) of
    (AG.CallBlock lc lr _ _ _, AG.ProcBeginBlock le) -> (tshow lc <> ":labelcall", tshow le <> ":code", callEdge)
    (AG.ProcBeginBlock le, AG.CallBlock lc lr _ _ _) -> (tshow le <> ":code", tshow lc <> ":labelcall", callEdge)
    -- call label is used for identifying a call block node in the graph
    (AG.CallBlock lc lr _ _ _, AG.ProcEndBlock le) -> (tshow lc <> ":labelret", tshow le <> ":code", retEdge)
    (AG.ProcEndBlock le, AG.CallBlock lc lr _ _ _) -> (tshow le <> ":code", tshow lc <> ":labelret", retEdge)
    (b1, b2) -> (tshow (idLabel b1) <> ":code", tshow (idLabel b2) <> ":code", Map.empty)
 
  callEdge = Map.fromList [ ("color", "blue") ]
  retEdge  = Map.fromList [ ("color", "red")  ]
  
  edges = [ "n" <> from <> " -> n" <> to <> attrText attrs <> ";" 
          | AG.Flow a b <- flow
          , let (from, to, attrs) = showFlow a b
          ]
  
  graphLines = 
    ["digraph " <> graphName <> "{"
    ,"overlap = false; splines = true;" ]
    ++ nodeDefs
    ++ edges
    ++ ["}"]