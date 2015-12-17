import CCO.Component
import CCO.Tree
import qualified CCO.Core as CR
import qualified CCO.Core.AG as CAG

import qualified CCO.HM as HM
import qualified CCO.HM.AG as HAG

import CCO.Semantics

import Control.Arrow

main = ioWrap (HM.parser >>> arr toANF >>> arr hmToCr >>> CAG.crprinter)
