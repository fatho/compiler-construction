module Dev where

import qualified Data.Map as M
import qualified Data.List as L
import Data.Monoid

import AttributeGrammar
import Lexer
import Parser
import qualified CCO.Printing as PP

import qualified Analyses.ConstantPropagation as CP
import qualified Analyses.StronglyLiveVariables as SLV

import qualified Analyses.Context as Context

import qualified MonotoneFrameworks as MF

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Util.Visual as Vis

-- | An analysis is an instance of monotone frameworks yielding some result @a@ for each label.  
type Analysis a = MF.MF Label a

{-- How To Run (examples)

-- Strongly Live Variables
ghci> run slv "fib"

--}

slv = MF.embellish (Context.callstrings 2) . SLV.stronglyLiveVariables

--cp :: Program' -> Analysis CP.VarMap
cp  = MF.embellish (Context.callstrings 2) . CP.constantPropagation

run :: (PP.Printable a) => (Program' -> Analysis a) -> String -> IO ()
run = runAnalysis'

printInfo :: PP.Printable a => String -> a -> IO ()
printInfo name val = putStrLn (name ++ ":") >> PP.renderIO_ 80 (PP.pp val) >> putStrLn ""

-- run some analysis by passing an analysis function and a 'show' function to display the result
runAnalysis' :: (PP.Printable a) => (Program' -> Analysis a) -> String -> IO ()
runAnalysis' analyze programName = do
  p <- parse programName
  let p' = toLabeledProgram p
  printInfo "Program with Labels" p
  let pSyn = synthesize p'
  printInfo "Labels" (show $ labels $ blocks_Syn_Program' pSyn)
  printInfo "Blocks" (show $ blocks_Syn_Program' pSyn)
  printInfo "Flow" (show $ flow_Syn_Program' pSyn)
  printInfo "InterFlow" (show $ interflow_Syn_Program' pSyn)
  printInfo "Global Vars" (show $ globalVars_Syn_Program' pSyn)
  let mf = analyze p'
  printInfo "Result of the analysis" $ MF.maximumFixedPoint mf
  printInfo "Extremal: " $ MF.extremalValue mf
  --PP.renderIO_ 80 (PP.pp $ MF.fixpoint (analyze p'))
  writeGraph programName (blocks_Syn_Program' pSyn) (MF.flow mf) (MF.extremalLabels mf)

-- parse program

parse :: String -> IO Program
parse programName = do
  let fileName = "./examples/"++programName++".c"
  content <- readFile fileName
  return . happy . alex $ content

writeGraph :: String -> [Block] -> [Flow Label] -> [Label] -> IO ()
writeGraph programName blocks flow ex = do
  let fileName = "./examples/"++programName++".dot"
  TIO.writeFile fileName (Vis.makeDot blocks flow (Vis.highlightExtremal ex <> Vis.codeOnly) (T.pack programName))
