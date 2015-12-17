import CCO.Component    (Component, component, printer, ioWrap)
import CCO.Tree         (ATerm, fromTree, toTree, parser)
import CCO.HM (Mod)
import CCO.Semantics
import Control.Arrow    (arr, (>>>))

main :: IO ()
main = ioWrap $ parser
            >>> (component toTree :: Component ATerm Mod)
            >>> arr toANF
            >>> arr fromTree
            >>> printer