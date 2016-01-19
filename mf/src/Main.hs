module Main where

import Control.Monad

import qualified Lexer
import qualified Parser
import qualified Dev
import qualified AttributeGrammar as AG
import qualified MonotoneFrameworks as MF
import qualified Analyses.ConstantPropagation as CP

main = do
  putStrLn "Enter example name or leave empty to quit: "
  name <- getLine
  when (not $ null name) $ do
    Dev.run Dev.cp name
    main