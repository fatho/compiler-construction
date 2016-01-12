module Main where

import qualified Lexer
import qualified Parser
import qualified AttributeGrammar as AG
import qualified MonotoneFrameworks as MF
import qualified Analyses.ConstantPropagation as CP

import CCO.Component    (Component, printer, component, ioWrap)
import Control.Arrow    (arr, (>>>))

parser :: Component String AG.Program
parser = arr $ Parser.happy . Lexer.alex

doCP prog = (prog,  MF.mf $ CP.constantPropagation prog) 

main = ioWrap (parser >>> arr toLabeledProgram >>> arr doCP >>> arr show)

toLabeledProgram :: AG.Program -> AG.Program'
toLabeledProgram prog = 
    AG.prog_Syn_Program $ AG.wrap_Program (AG.sem_Program prog) AG.Inh_Program