import CCO.Component    (Component, component, printer, ioWrap)
import CCO.Tree         (ATerm, fromTree, toTree, parser)
import CCO.HM (Tm)
import CCO.HM.AG (anf_Syn_ToANF, wrap_ToANF, sem_ToANF, Inh_ToANF(..), ToANF(..))
import Control.Arrow    (arr, (>>>))

toANF :: Tm -> Tm
toANF t = anf_Syn_ToANF $ wrap_ToANF (sem_ToANF $ ToANF t) Inh_ToANF

main :: IO ()
main = ioWrap $ parser
            >>> (component toTree :: Component ATerm Tm)
            >>> arr toANF
            >>> arr fromTree
            >>> printer