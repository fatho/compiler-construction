module Main where

import qualified Lexer
import qualified Parser
import qualified AttributeGrammar as AG

import CCO.Component    (Component, printer, component, ioWrap)
import Control.Arrow    (arr, (>>>))

parser :: Component String AG.Program
parser = arr $ Parser.happy . Lexer.alex

main = ioWrap (parser >>> printer)
