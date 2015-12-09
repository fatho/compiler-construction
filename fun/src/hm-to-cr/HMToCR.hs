import CCO.Component    (Component, component, printer, ioWrap)
import CCO.Tree         (ATerm, fromTree, toTree, parser)
import CCO.Core         (Mod)
import CCO.Core.AG
import CCO.HM (Tm)
import CCO.HM.AG (mod_Syn_HMToCR, wrap_HMToCR, sem_HMToCR, Inh_HMToCR(..), HMToCR(..))
import Control.Arrow    (arr, (>>>))

hmToCr :: Tm -> Mod
hmToCr t = mod_Syn_HMToCR $ wrap_HMToCR (sem_HMToCR $ HMToCR t) Inh_HMToCR

main :: IO ()
main = ioWrap $ parser
            >>> (component toTree :: Component ATerm Tm)
            >>> arr hmToCr
            >>> arr fromTree
            >>> printer