module Main where

import Control.Monad
import Control.Exception
import qualified Data.List as List

import qualified Dev

parts :: [(String, String -> IO ())]
parts = 
    [ ( "cp", Dev.run Dev.cp )
    , ( "slv", Dev.run Dev.slv) ]

main :: IO ()
main = do
  putStrLn "Enter example name or leave empty to quit: "
  name <- getLine
  when (not (null name)) $ do
    putStrLn "Enter analysis name ('cp', 'slv'): "
    analysis <- getLine
    case List.lookup analysis parts of
      Just doIt -> doIt name `catch` (\(SomeException e) -> print e)
      Nothing -> putStrLn "analysis does not exist"
    main