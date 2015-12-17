module CCO.Semantics where

import qualified CCO.HM as HM
import qualified CCO.Core as CR

import qualified CCO.HM.AG as HM
import qualified CCO.Core.AG as CR

toANF :: HM.Mod -> HM.Mod
toANF m = HM.anf_Syn_Mod $ HM.wrap_Mod (HM.sem_Mod m) HM.Inh_Mod

hmToCr :: HM.Mod -> CR.Mod
hmToCr m = HM.mod_Syn_Mod $ HM.wrap_Mod (HM.sem_Mod m) HM.Inh_Mod
