{- | Provides functionality to render the flow graph of a program optionally with additonal information. 
-}
{-# LANGUAGE OverloadedStrings #-}
module Util.Graphviz
  ( highlightExtremal
  , codeWithLabels
  , blockCodeLabel
  , withResults
  , makeDot
  ) where

import Data.Monoid

import qualified Data.Map.Strict as Map
import qualified AttributeGrammar as AG
import Data.Text (Text)
import qualified Data.Text as T
import qualified MonotoneFrameworks.Description as MF

import qualified CCO.Printing as PP

-- | Mapping of Graphviz attributes.
type Attributes = Map.Map Text Text

-- | Converts a map of attributes to the corresponding DOT syntax.
attrText :: Attributes -> Text
attrText attrs = "[" <> Map.foldMapWithKey ppattr attrs <> "]" where
  ppattr k v = k <> "= \"" <> T.replace "\"" "\\\"" v <> "\", "

-- | Helper function for showing something as 'Text'
tshow :: Show a => a -> Text
tshow = T.pack . show

-- | Helper function to replace angle brackets by their HTML encoding, since
-- those brackets have a special in DOT.
escapeRecordChars :: Text -> Text
escapeRecordChars = T.replace "<" "\\<" . T.replace ">" "\\>" . T.replace "|" "\\|"

prettyExpr :: AG.Expr -> Text
prettyExpr expr = escapeRecordChars (T.pack $ AG.pretty_Syn_Expr $ AG.synthesize expr)

prettyIExpr :: AG.IExpr -> Text
prettyIExpr = prettyExpr . AG.I

prettyBExpr :: AG.BExpr -> Text
prettyBExpr = prettyExpr . AG.B

-- | Converts a block to a part of a DOT record label showing the code and associated labels.
-- The field containing the code is named "code", if there is only one label, that field is
-- named "label", for calls, the label fields are named "labelcall" and "labelret".
blockCodeLabel :: AG.Block -> Text
blockCodeLabel (AG.BBlock l expr)           = "<code> " <> prettyBExpr expr <> "|<label> " <> tshow l
blockCodeLabel (AG.SkipBlock l)             = "<code> skip|<label> " <> tshow l
blockCodeLabel (AG.IAssignBlock l var expr) = "<code> " <> T.pack var <> " := " <> prettyIExpr expr <> "|<label> " <> tshow l 
blockCodeLabel (AG.ProcBeginBlock l)        = "<code> is|<label> " <> tshow l
blockCodeLabel (AG.ProcEndBlock l)          = "<code> end|<label> " <> tshow l
blockCodeLabel (AG.CallBlock lc lr n es o)  = "<code> " <> T.pack n <> "(" 
  <> foldr (\e rs -> prettyExpr e <> ", " <> rs) (T.pack o) es 
  <> ")|{<labelcall> " <> tshow lc <> "|<labelret> " <> tshow lr <> "}"  
blockCodeLabel (AG.BAssignBlock   l var expr)   = 
  "<code> " <> T.pack var <> " := " <> prettyBExpr expr <> "|<label> " <> tshow l 
blockCodeLabel (AG.MallocBlock    l var size) = 
  "<code> malloc(" <> T.pack var <> ", " <> prettyIExpr size <> ")|<label> " <> tshow l
blockCodeLabel (AG.FreeBlock      l ptr)      = "<code> free(" <> prettyIExpr ptr <> ")|<label> " <> tshow l
blockCodeLabel (AG.RefAssignBlock l (AG.Plus (AG.Var var) idx) val) = 
  "<code> " <> T.pack var <> "[" <> prettyIExpr idx <> "] := " <> prettyIExpr val <> "|<label> " <> tshow l
blockCodeLabel (AG.RefAssignBlock l addr val) = 
  "<code> " <> prettyIExpr (AG.Deref addr) <> " := " <> prettyIExpr val <> "|<label> " <> tshow l
blockCodeLabel (AG.ContinueBlock  l)       = "<code> continue|<label> " <> tshow l
blockCodeLabel (AG.BreakBlock     l)       = "<code> break|<label> " <> tshow l

-- | If the block label is in the supplied list of extremal labels, returns attributes to highlight
-- the node. Intended for monoidal composition, i.e.
--
-- @
--  highlightExtremal extremal <> codeWithLabels
-- @
highlightExtremal :: [AG.Label] -> AG.Block -> Attributes
highlightExtremal extremal blk
  | isExtremal = Map.fromList [("style", "bold"), ("shape", "Mrecord")]
  | otherwise  = Map.empty 
  where
    isExtremal = any (`elem` extremal) (AG.labelsFromBlock blk)

-- | Creates node attributes describing a record node containing code and labels of the block. 
codeWithLabels :: AG.Block -> Attributes
codeWithLabels blk = Map.fromList [("label", blockCodeLabel blk), ("shape","record")]

-- | Wraps a label with the results at the given block, intended for monadic composition
withResults :: (PP.Printable a) => MF.Fixpoint AG.Label a -> Attributes -> AG.Block -> Attributes
withResults (MF.Fixpoint ctx eff) attrs blk = Map.adjust wrapLabel "label" attrs where
  labels = AG.labelsFromBlock blk
  showResults m = T.intercalate "|" $ map (escapeRecordChars . T.pack . PP.render_ maxBound . PP.pp . (m Map.!)) $ labels 
  ctxVals = showResults ctx
  effVals = showResults eff
  -- switches orientation between horizontal and vertical in a DOT record
  switchOrientation rec = "{" <> rec <> "}"
  wrapLabel lbl = switchOrientation ctxVals <> "|" <> lbl <> "|" <> switchOrientation effVals

-- | Generates a Graphviz graph in DOT syntax from a list of code blocks, the flow between them
-- and a specification of how to render individual blocks.
makeDot :: [AG.Block]               -- ^ list of code blocks 
        -> [AG.Flow AG.Label]       -- ^ flow between blocks
        -> (AG.Block -> Attributes) -- ^ render function
        -> Text                     -- ^ graph name
        -> Text                     -- ^ DOT graph
makeDot blocks flow attrs graphName = T.unlines graphLines where
  blockMap = Map.fromList [ (l, blk) | blk <- blocks, l <- AG.labelsFromBlock blk ]
  -- returns the label that is used as ID for the block
  idLabel = head . AG.labelsFromBlock
  
  -- DOT node definitions
  nodeDefs = [ "n" <> tshow (idLabel blk) <> attrText (attrs blk) <> ";" | blk <- blocks ]
  
  -- | returns node id's (with sub-specifier) for given flow
  showFlow from to = case (blockMap Map.! from, blockMap Map.! to) of
    (AG.CallBlock lc _ _ _ _, AG.ProcBeginBlock le) -> (tshow lc <> ":labelcall", tshow le <> ":code", callEdge)
    (AG.ProcBeginBlock le, AG.CallBlock lc _ _ _ _) -> (tshow le <> ":code", tshow lc <> ":labelcall", callEdge)
    -- call label is used for identifying a call block node in the graph
    (AG.CallBlock lc _ _ _ _, AG.ProcEndBlock le) -> (tshow lc <> ":labelret", tshow le <> ":code", retEdge)
    (AG.ProcEndBlock le, AG.CallBlock lc _ _ _ _) -> (tshow le <> ":code", tshow lc <> ":labelret", retEdge)
    (b1, b2) -> (tshow (idLabel b1) <> ":code", tshow (idLabel b2) <> ":code", Map.empty)
 
  -- formatting for a call edge
  callEdge = Map.fromList [ ("color", "blue") ]
  -- formatting for a return edge
  retEdge  = Map.fromList [ ("color", "red")  ]
  -- list of all graph edges
  edges = [ "n" <> from <> " -> n" <> to <> attrText edgeAttrs <> ";" 
          | AG.Flow a b <- flow
          , let (from, to, edgeAttrs) = showFlow a b
          ]
  -- lines of complete graph code
  graphLines =
    ["digraph " <> graphName <> "{"
    ,"overlap = false; splines = true;" ]
    ++ nodeDefs
    ++ edges
    ++ ["}"]