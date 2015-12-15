import CCO.Component
import CCO.Tree
import qualified CCO.Core as CR
import qualified CCO.Core.AG as CAG

import qualified CCO.HM as HM
import qualified CCO.HM.AG as HAG
import Control.Arrow

hmToCr :: HM.Tm -> CR.Mod
hmToCr t = HAG.mod_Syn_HMToCR $ HAG.wrap_HMToCR (HAG.sem_HMToCR $ HAG.HMToCR t) HAG.Inh_HMToCR


toANF :: HM.Tm -> HM.Tm
toANF t = HAG.anf_Syn_ToANF $ HAG.wrap_ToANF (HAG.sem_ToANF $ HAG.ToANF t) HAG.Inh_ToANF


main = ioWrap (HM.parser >>> arr toANF >>> arr hmToCr >>> CAG.crprinter)
