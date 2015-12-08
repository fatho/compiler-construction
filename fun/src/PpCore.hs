import CCO.Component    (Component, component, printer, ioWrap)
import CCO.Core         (Mod)
import CCO.Core.AG
import CCO.Tree         (ATerm, toTree, parser)
import Control.Arrow    (arr, (>>>))

main = ioWrap $
       parser >>> (component toTree :: Component ATerm Mod) >>> crprinter

