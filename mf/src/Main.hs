module Main where

import Control.Monad
import Control.Exception
import qualified Data.List as List
import System.Environment

import qualified Dev

analyses :: [(String, String -> String -> IO ())]
analyses = 
  [ ( "cp", Dev.run Dev.cp )
  , ( "slv", Dev.run Dev.slv) ]


analyzeIt :: String -> String -> IO ()
analyzeIt analysisName programName =
  case List.lookup analysisName analyses of
    Just doIt -> doIt programName analysisName `catch` (\(SomeException e) -> print e)
    Nothing -> putStrLn "analysis does not exist"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [analysis, name] -> analyzeIt analysis name
    _ -> do 
        putStrLn "Enter example name or leave empty to quit: "
        name <- getLine
        when (not (null name)) $ do
          putStrLn "Enter analysis name ('cp', 'slv'): "
          analysis <- getLine
          analyzeIt analysis name
          main