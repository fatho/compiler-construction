module Main where

import Control.Monad
import Control.Exception
import qualified Data.List as List
import System.Environment
import Text.Read (readMaybe)

import qualified Dev

analyses :: [(String, String -> String -> Int -> IO ())]
analyses = 
  [ ( "cp", Dev.run Dev.cp )
  , ( "slv", Dev.run Dev.slv) ]

parseDepth :: String -> Int
parseDepth str = maybe 0 id (readMaybe str)

analyzeIt :: String -> String -> Int -> IO ()
analyzeIt analysisName programName d =
  case List.lookup analysisName analyses of
    Just doIt -> doIt programName analysisName d `catch` (\(SomeException e) -> print e)
    Nothing -> putStrLn "analysis does not exist"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [analysis, name] -> analyzeIt analysis name 0
    [analysis, name, depth] -> analyzeIt analysis name (parseDepth depth)
    _ -> do 
        putStrLn "Enter example name or leave empty to quit: "
        name <- getLine
        when (not (null name)) $ do
          putStrLn "Enter analysis name ('cp', 'slv'): "
          analysis <- getLine
          putStrLn "Enter context depth (default is 0): "
          depth <- liftM parseDepth getLine
          analyzeIt analysis name depth
          main