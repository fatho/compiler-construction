module Dev where

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
import qualified Util.Graphviz as Viz

-- | An analysis is an instance of monotone frameworks yielding some result @a@ for each label.  
type Analysis a = MF.MF Label a

{-- How To Run (examples)

-- Strongly Live Variables
ghci> run slv "fib"

--}

slv :: Program' -> Analysis SLV.VarSet
slv = SLV.stronglyLiveVariables

cp :: Program' -> Analysis CP.VarMap
cp  = CP.constantPropagation

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
  let mf  = analyze p'
      emf = MF.embellish (Context.callstrings 2) mf 
      fp  = MF.maximumFixedPoint emf
      collapsedFp = fmap (MF.collapse $ MF.lattice mf) fp
  printInfo "Result of the analysis" fp
  printInfo "Collapsed results" collapsedFp  
  printInfo "Extremal" $ MF.extremalValue mf
  writeGraph programName (blocks_Syn_Program' pSyn) (MF.flow mf) (MF.extremalLabels mf) collapsedFp

-- parse program

parse :: String -> IO Program
parse programName = do
  let fileName = "./examples/"++programName++".c"
  content <- readFile fileName
  return . happy . alex $ content

writeGraph :: (PP.Printable a) => String -> [Block] -> [Flow Label] -> [Label] -> MF.Fixpoint Label a -> IO ()
writeGraph programName blocks flow ex fp = do
  let fileName = "./examples/"++programName++".dot"
  TIO.writeFile fileName (Viz.makeDot blocks flow (Viz.highlightExtremal ex <> Viz.codeWithLabels >>= Viz.withResults fp) (T.pack programName))
