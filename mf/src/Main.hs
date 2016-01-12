module Main where

import qualified Lexer
import qualified Parser
import qualified AttributeGrammar as AG
import MonotoneFrameworks

import CCO.Component    (Component, printer, component, ioWrap)
import Control.Arrow    (arr, (>>>))

parser :: Component String AG.Program
parser = arr $ Parser.happy . Lexer.alex

main = ioWrap (parser >>> arr toLabeledProgram >>> printer)

toLabeledProgram :: AG.Program -> AG.Program'
toLabeledProgram prog = 
    AG.prog_Syn_Program $ AG.wrap_Program (AG.sem_Program prog) AG.Inh_Program