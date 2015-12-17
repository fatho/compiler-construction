import CCO.Component    (Component, component, printer, ioWrap)
import CCO.Tree         (ATerm, fromTree, toTree, parser)
import CCO.Core         (Mod)
import CCO.Core.AG
import qualified CCO.HM as HM
import CCO.Semantics
import Control.Arrow    (arr, (>>>))

main :: IO ()
main = ioWrap $ parser
            >>> (component toTree :: Component ATerm HM.Mod)
            >>> arr hmToCr
            >>> arr fromTree
            >>> printer