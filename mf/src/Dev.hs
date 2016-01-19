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
type Analysis a = MF.MF Label a

{-- How To Run (examples)

-- Strongly Live Variables
ghci> run slv "fib"

--}

slv = undefined -- SLV.stronglyLiveVariables

cp :: Program' -> Analysis CP.VarMap
cp  = CP.constantPropagation

run :: (Show a) => (Program' -> Analysis a) -> String -> IO ()
run = runAnalysis'

printInfo :: Show a => String -> a -> IO ()
printInfo name val = putStrLn (name ++ ":") >> print val >> putStrLn ""

-- run some analysis by passing an analysis function and a 'show' function to display the result
runAnalysis' :: (Show a) => (Program' -> Analysis a) -> String -> IO ()
runAnalysis' analyze programName = do
  p <- parse programName
  let p' = toLabeledProgram p
  printInfo "Program with Labels" p
  let pSyn = programSyn p'
  printInfo "Labels" (show $ labels $ blocks_Syn_Program' pSyn)
  printInfo "Blocks" (show $ blocks_Syn_Program' pSyn)
  printInfo "Flow" (show $ flow_Syn_Program' pSyn)
  printInfo "InterFlow" (show $ interflow_Syn_Program' pSyn)
  printInfo "Global Vars" (show $ globalVars_Syn_Program' pSyn)
  let mf = analyze p'
  printInfo "Result of the analysis" $ MF.fixpoint mf
  printInfo "Extremal: " $ MF.extremalValue mf
  --PP.renderIO_ 80 (PP.pp $ MF.fixpoint (analyze p'))
  putStrLn ""

-- parse program

parse :: String -> IO Program
parse programName = do
  let fileName = "./examples/"++programName++".c"
  content <- readFile fileName
  return . happy . alex $ content





