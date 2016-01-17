module Dev where

import qualified Data.Map as M
import qualified Data.List as L

import AttributeGrammar
import Lexer
import Parser
import qualified CCO.Printing as PP

import qualified Analyses.ConstantPropagation as CP
import qualified Analyses.StronglyLiveVariables as SLV
import qualified MonotoneFrameworks as MF

-- | An analysis is an instance of monotone frameworks yielding some result @a@ for each label.  
type Analysis a = MF.Instance Label a

{-- How To Run (examples)

-- Strongly Live Variables
ghci> run slv "fib"

--}

slv = SLV.stronglyLiveVariables

cp :: Program' -> Analysis CP.VarEnv
cp  = CP.constantPropagation

run :: (MF.JoinSemiLattice a, PP.Printable a) => (Program' -> Analysis a) -> String -> IO ()
run = runAnalysis'

-- run some analysis by passing an analysis function and a 'show' function to display the result
runAnalysis' :: (MF.JoinSemiLattice a, PP.Printable a) => (Program' -> Analysis a) -> String -> IO ()
runAnalysis' analyze programName = do
  p <- parse programName
  let p' = toLabeledProgram p
  putStrLn "Program with Labels:"
  putStrLn (show p')
  putStrLn ""
  let pSyn = programSyn p'
  putStrLn "Labels:"
  putStrLn (show $ labels_Syn_Program' pSyn)
  putStrLn ""
  putStrLn "Blocks:"
  putStrLn (show $ blocks_Syn_Program' pSyn)
  putStrLn ""
  putStrLn "Flow:"
  putStrLn (show $ flow_Syn_Program' pSyn)
  putStrLn ""
  putStrLn "Reverse Flow:"
  putStrLn (show$ reverseFlow $ flow_Syn_Program' pSyn)
  putStrLn ""
  putStrLn "Result of the analysis:"
  PP.renderIO_ 80 (PP.pp $ MF.mfp (analyze p'))
  putStrLn ""

-- parse program

parse :: String -> IO Program
parse programName = do
  let fileName = "./examples/"++programName++".c"
  content <- readFile fileName
  return . happy . alex $ content





