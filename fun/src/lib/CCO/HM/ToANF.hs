module ToANF where

import CCO.HM.Base

-- maybe we should synthesize this in an AG for consistency 
-- and because we need to keep track of Var bindings for applications  

hmToANF :: Tm -> Tm
hmToANF (Tm sp t) = Tm sp (toANF t)

toANF :: Tm_ -> Tm_
toANF n@(Nat _) = n
toANF v@(Var _) = v
toANF (Lam x t1) = Lam x (hmToANF t1)
-- TODO: figure out appBind and lamBind (name + counter?)
toANF (App t1 t2@(App _ _)) = Let (Var appBind) {- = -} (hmToANF t2) {- in -} (App (hmToANF t1) (Var appBind))
toANF (App t1 t2@(Lam _ _)) = Let (Var lamBind) {- = -} (hmToANF t2) {- in -} (App (hmToANF t1) (Var lamBind))
toANF (App t1 t2)           = App   (hmToANF t1) (hmToANF t2) 
toANF (Let x t1 t2)         = Let x (hmToANF t1) (hmToANF t2)

