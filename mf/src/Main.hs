module Main where

import Control.Monad
import System.IO.Error
import qualified Data.List as List

import qualified Lexer
import qualified Parser
import qualified Dev
import qualified AttributeGrammar as AG
import qualified MonotoneFrameworks as MF
import qualified Analyses.ConstantPropagation as CP

parts :: [(String, String -> IO ())]
parts = 
    [ ( "cp", Dev.run Dev.cp )
    , ( "slv", Dev.run Dev.slv) ]

main = do
  putStrLn "Enter example name or leave empty to quit: "
  name <- getLine
  when (not (null name)) $ do
    putStrLn "Enter analysis name ('cp', 'slv'): "
    analysis <- getLine
    case List.lookup analysis parts of
      Just doIt -> doIt name `catchIOError` print
      Nothing -> putStrLn "analysis does not exist"
    main