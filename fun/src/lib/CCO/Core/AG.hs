

-- UUAGC 0.9.52.1 (src/CCO/Core/AG.ag)
module CCO.Core.AG where

import UHC.Util.Pretty
import UHC.Light.Compiler.Base.API    (defaultEHCOpts)
import UHC.Light.Compiler.CoreRun.API (printModule)
import CCO.Component

{-# LINE 2 "src/CCO/Core/AG/ToCoreRun.ag" #-}

import           UHC.Light.Compiler.Base.API
import qualified UHC.Light.Compiler.CoreRun.API as CR
{-# LINE 16 "src/CCO/Core/AG.hs" #-}

{-# LINE 2 "src/CCO/Core/AG/Base.ag" #-}

import qualified UHC.Light.Compiler.CoreRun.API as CR
{-# LINE 21 "src/CCO/Core/AG.hs" #-}
{-# LINE 26 "src/CCO/Core/AG.ag" #-}

crprinter :: Component Mod String
crprinter = component $ \mod -> do
  let crmod = crmod_Syn_Mod (wrap_Mod (sem_Mod mod) Inh_Mod)
  return $ show $ printModule defaultEHCOpts crmod
{-# LINE 28 "src/CCO/Core/AG.hs" #-}
-- Bind --------------------------------------------------------
data Bind = Bind (Ref) (Exp)
-- cata
sem_Bind :: Bind ->
            T_Bind
sem_Bind (Bind _x _xexp) =
    (sem_Bind_Bind (sem_Ref _x) (sem_Exp _xexp))
-- semantic domain
type T_Bind = Int ->
              ( ([CR.Bind]),Bind,Int)
data Inh_Bind = Inh_Bind {stkoff_Inh_Bind :: Int}
data Syn_Bind = Syn_Bind {crbindl_Syn_Bind :: ([CR.Bind]),self_Syn_Bind :: Bind,stkoff_Syn_Bind :: Int}
wrap_Bind :: T_Bind ->
             Inh_Bind ->
             Syn_Bind
wrap_Bind sem (Inh_Bind _lhsIstkoff) =
    (let ( _lhsOcrbindl,_lhsOself,_lhsOstkoff) = sem _lhsIstkoff
     in  (Syn_Bind _lhsOcrbindl _lhsOself _lhsOstkoff))
sem_Bind_Bind :: T_Ref ->
                 T_Exp ->
                 T_Bind
sem_Bind_Bind x_ xexp_ =
    (\ _lhsIstkoff ->
         (let _lhsOcrbindl :: ([CR.Bind])
              _xexpOstkoff :: Int
              _lhsOstkoff :: Int
              _lhsOself :: Bind
              _xIcrrefl :: ([CR.RRef])
              _xIself :: Ref
              _xexpIcrexp :: (CR.Exp)
              _xexpIcrexpl :: ([CR.Exp])
              _xexpIself :: Exp
              _lhsOcrbindl =
                  ({-# LINE 46 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   [_xexpIcrexp]
                   {-# LINE 64 "src/CCO/Core/AG.hs" #-}
                   )
              _xexpOstkoff =
                  ({-# LINE 70 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   0
                   {-# LINE 69 "src/CCO/Core/AG.hs" #-}
                   )
              _lhsOstkoff =
                  ({-# LINE 71 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   _lhsIstkoff + 1
                   {-# LINE 74 "src/CCO/Core/AG.hs" #-}
                   )
              _self =
                  Bind _xIself _xexpIself
              _lhsOself =
                  _self
              ( _xIcrrefl,_xIself) =
                  x_
              ( _xexpIcrexp,_xexpIcrexpl,_xexpIself) =
                  xexp_ _xexpOstkoff
          in  ( _lhsOcrbindl,_lhsOself,_lhsOstkoff)))
-- BindL -------------------------------------------------------
type BindL = [Bind]
-- cata
sem_BindL :: BindL ->
             T_BindL
sem_BindL list =
    (Prelude.foldr sem_BindL_Cons sem_BindL_Nil (Prelude.map sem_Bind list))
-- semantic domain
type T_BindL = Int ->
               ( ([CR.Bind]),BindL,Int)
data Inh_BindL = Inh_BindL {stkoff_Inh_BindL :: Int}
data Syn_BindL = Syn_BindL {crbindl_Syn_BindL :: ([CR.Bind]),self_Syn_BindL :: BindL,stkoff_Syn_BindL :: Int}
wrap_BindL :: T_BindL ->
              Inh_BindL ->
              Syn_BindL
wrap_BindL sem (Inh_BindL _lhsIstkoff) =
    (let ( _lhsOcrbindl,_lhsOself,_lhsOstkoff) = sem _lhsIstkoff
     in  (Syn_BindL _lhsOcrbindl _lhsOself _lhsOstkoff))
sem_BindL_Cons :: T_Bind ->
                  T_BindL ->
                  T_BindL
sem_BindL_Cons hd_ tl_ =
    (\ _lhsIstkoff ->
         (let _lhsOcrbindl :: ([CR.Bind])
              _lhsOself :: BindL
              _lhsOstkoff :: Int
              _hdOstkoff :: Int
              _tlOstkoff :: Int
              _hdIcrbindl :: ([CR.Bind])
              _hdIself :: Bind
              _hdIstkoff :: Int
              _tlIcrbindl :: ([CR.Bind])
              _tlIself :: BindL
              _tlIstkoff :: Int
              _lhsOcrbindl =
                  ({-# LINE 43 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   _hdIcrbindl ++ _tlIcrbindl
                   {-# LINE 122 "src/CCO/Core/AG.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _lhsOstkoff =
                  ({-# LINE 63 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   _tlIstkoff
                   {-# LINE 131 "src/CCO/Core/AG.hs" #-}
                   )
              _hdOstkoff =
                  ({-# LINE 63 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   _lhsIstkoff
                   {-# LINE 136 "src/CCO/Core/AG.hs" #-}
                   )
              _tlOstkoff =
                  ({-# LINE 63 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   _hdIstkoff
                   {-# LINE 141 "src/CCO/Core/AG.hs" #-}
                   )
              ( _hdIcrbindl,_hdIself,_hdIstkoff) =
                  hd_ _hdOstkoff
              ( _tlIcrbindl,_tlIself,_tlIstkoff) =
                  tl_ _tlOstkoff
          in  ( _lhsOcrbindl,_lhsOself,_lhsOstkoff)))
sem_BindL_Nil :: T_BindL
sem_BindL_Nil =
    (\ _lhsIstkoff ->
         (let _lhsOcrbindl :: ([CR.Bind])
              _lhsOself :: BindL
              _lhsOstkoff :: Int
              _lhsOcrbindl =
                  ({-# LINE 43 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   []
                   {-# LINE 157 "src/CCO/Core/AG.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
              _lhsOstkoff =
                  ({-# LINE 63 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   _lhsIstkoff
                   {-# LINE 166 "src/CCO/Core/AG.hs" #-}
                   )
          in  ( _lhsOcrbindl,_lhsOself,_lhsOstkoff)))
-- Exp ---------------------------------------------------------
data Exp = SExp (SExp)
         | Lam (RefL) (Exp)
         | App (Exp) (SExpL)
         | Prim (String) (SExpL)
         | Node (Int) (SExpL)
         | Case (SExp) (ExpL)
         | Let (Bind) (Exp)
         | Dbg (String)
-- cata
sem_Exp :: Exp ->
           T_Exp
sem_Exp (SExp _sexp) =
    (sem_Exp_SExp (sem_SExp _sexp))
sem_Exp (Lam _args _body) =
    (sem_Exp_Lam (sem_RefL _args) (sem_Exp _body))
sem_Exp (App _func _args) =
    (sem_Exp_App (sem_Exp _func) (sem_SExpL _args))
sem_Exp (Prim _func _args) =
    (sem_Exp_Prim _func (sem_SExpL _args))
sem_Exp (Node _tag _args) =
    (sem_Exp_Node _tag (sem_SExpL _args))
sem_Exp (Case _sexp _alts) =
    (sem_Exp_Case (sem_SExp _sexp) (sem_ExpL _alts))
sem_Exp (Let _bind _body) =
    (sem_Exp_Let (sem_Bind _bind) (sem_Exp _body))
sem_Exp (Dbg _info) =
    (sem_Exp_Dbg _info)
-- semantic domain
type T_Exp = Int ->
             ( (CR.Exp),([CR.Exp]),Exp)
data Inh_Exp = Inh_Exp {stkoff_Inh_Exp :: Int}
data Syn_Exp = Syn_Exp {crexp_Syn_Exp :: (CR.Exp),crexpl_Syn_Exp :: ([CR.Exp]),self_Syn_Exp :: Exp}
wrap_Exp :: T_Exp ->
            Inh_Exp ->
            Syn_Exp
wrap_Exp sem (Inh_Exp _lhsIstkoff) =
    (let ( _lhsOcrexp,_lhsOcrexpl,_lhsOself) = sem _lhsIstkoff
     in  (Syn_Exp _lhsOcrexp _lhsOcrexpl _lhsOself))
sem_Exp_SExp :: T_SExp ->
                T_Exp
sem_Exp_SExp sexp_ =
    (\ _lhsIstkoff ->
         (let _lhsOcrexpl :: ([CR.Exp])
              _lhsOself :: Exp
              _lhsOcrexp :: (CR.Exp)
              _sexpIcrsexpl :: ([CR.SExp])
              _sexpIself :: SExp
              _crexp =
                  ({-# LINE 30 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   CR.mkExp (head _sexpIcrsexpl)
                   {-# LINE 220 "src/CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 40 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 225 "src/CCO/Core/AG.hs" #-}
                   )
              _self =
                  SExp _sexpIself
              _lhsOself =
                  _self
              _lhsOcrexp =
                  ({-# LINE 25 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 234 "src/CCO/Core/AG.hs" #-}
                   )
              ( _sexpIcrsexpl,_sexpIself) =
                  sexp_
          in  ( _lhsOcrexp,_lhsOcrexpl,_lhsOself)))
sem_Exp_Lam :: T_RefL ->
               T_Exp ->
               T_Exp
sem_Exp_Lam args_ body_ =
    (\ _lhsIstkoff ->
         (let _lhsOcrexpl :: ([CR.Exp])
              _bodyOstkoff :: Int
              _lhsOself :: Exp
              _lhsOcrexp :: (CR.Exp)
              _argsIcrrefl :: ([CR.RRef])
              _argsIself :: RefL
              _bodyIcrexp :: (CR.Exp)
              _bodyIcrexpl :: ([CR.Exp])
              _bodyIself :: Exp
              _crexp =
                  ({-# LINE 31 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   CR.mkLam (length _argsIcrrefl) 100 _bodyIcrexp
                   {-# LINE 256 "src/CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 40 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 261 "src/CCO/Core/AG.hs" #-}
                   )
              _bodyOstkoff =
                  ({-# LINE 74 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   length _argsIcrrefl
                   {-# LINE 266 "src/CCO/Core/AG.hs" #-}
                   )
              _self =
                  Lam _argsIself _bodyIself
              _lhsOself =
                  _self
              _lhsOcrexp =
                  ({-# LINE 25 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 275 "src/CCO/Core/AG.hs" #-}
                   )
              ( _argsIcrrefl,_argsIself) =
                  args_
              ( _bodyIcrexp,_bodyIcrexpl,_bodyIself) =
                  body_ _bodyOstkoff
          in  ( _lhsOcrexp,_lhsOcrexpl,_lhsOself)))
sem_Exp_App :: T_Exp ->
               T_SExpL ->
               T_Exp
sem_Exp_App func_ args_ =
    (\ _lhsIstkoff ->
         (let _lhsOcrexpl :: ([CR.Exp])
              _lhsOself :: Exp
              _lhsOcrexp :: (CR.Exp)
              _funcOstkoff :: Int
              _funcIcrexp :: (CR.Exp)
              _funcIcrexpl :: ([CR.Exp])
              _funcIself :: Exp
              _argsIcrsexpl :: ([CR.SExp])
              _argsIself :: SExpL
              _crexp =
                  ({-# LINE 32 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   CR.mkApp _funcIcrexp _argsIcrsexpl
                   {-# LINE 299 "src/CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 40 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 304 "src/CCO/Core/AG.hs" #-}
                   )
              _self =
                  App _funcIself _argsIself
              _lhsOself =
                  _self
              _lhsOcrexp =
                  ({-# LINE 25 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 313 "src/CCO/Core/AG.hs" #-}
                   )
              _funcOstkoff =
                  ({-# LINE 61 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   _lhsIstkoff
                   {-# LINE 318 "src/CCO/Core/AG.hs" #-}
                   )
              ( _funcIcrexp,_funcIcrexpl,_funcIself) =
                  func_ _funcOstkoff
              ( _argsIcrsexpl,_argsIself) =
                  args_
          in  ( _lhsOcrexp,_lhsOcrexpl,_lhsOself)))
sem_Exp_Prim :: String ->
                T_SExpL ->
                T_Exp
sem_Exp_Prim func_ args_ =
    (\ _lhsIstkoff ->
         (let _lhsOcrexpl :: ([CR.Exp])
              _lhsOself :: Exp
              _lhsOcrexp :: (CR.Exp)
              _argsIcrsexpl :: ([CR.SExp])
              _argsIself :: SExpL
              _crexp =
                  ({-# LINE 33 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   CR.mkFFI func_       _argsIcrsexpl
                   {-# LINE 338 "src/CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 40 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 343 "src/CCO/Core/AG.hs" #-}
                   )
              _self =
                  Prim func_ _argsIself
              _lhsOself =
                  _self
              _lhsOcrexp =
                  ({-# LINE 25 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 352 "src/CCO/Core/AG.hs" #-}
                   )
              ( _argsIcrsexpl,_argsIself) =
                  args_
          in  ( _lhsOcrexp,_lhsOcrexpl,_lhsOself)))
sem_Exp_Node :: Int ->
                T_SExpL ->
                T_Exp
sem_Exp_Node tag_ args_ =
    (\ _lhsIstkoff ->
         (let _lhsOcrexpl :: ([CR.Exp])
              _lhsOself :: Exp
              _lhsOcrexp :: (CR.Exp)
              _argsIcrsexpl :: ([CR.SExp])
              _argsIself :: SExpL
              _crexp =
                  ({-# LINE 34 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   CR.mkTup tag_        _argsIcrsexpl
                   {-# LINE 370 "src/CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 40 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 375 "src/CCO/Core/AG.hs" #-}
                   )
              _self =
                  Node tag_ _argsIself
              _lhsOself =
                  _self
              _lhsOcrexp =
                  ({-# LINE 25 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 384 "src/CCO/Core/AG.hs" #-}
                   )
              ( _argsIcrsexpl,_argsIself) =
                  args_
          in  ( _lhsOcrexp,_lhsOcrexpl,_lhsOself)))
sem_Exp_Case :: T_SExp ->
                T_ExpL ->
                T_Exp
sem_Exp_Case sexp_ alts_ =
    (\ _lhsIstkoff ->
         (let _lhsOcrexpl :: ([CR.Exp])
              _lhsOself :: Exp
              _lhsOcrexp :: (CR.Exp)
              _altsOstkoff :: Int
              _sexpIcrsexpl :: ([CR.SExp])
              _sexpIself :: SExp
              _altsIcrexpl :: ([CR.Exp])
              _altsIself :: ExpL
              _crexp =
                  ({-# LINE 35 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   CR.mkCase (head _sexpIcrsexpl) _altsIcrexpl
                   {-# LINE 405 "src/CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 40 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 410 "src/CCO/Core/AG.hs" #-}
                   )
              _self =
                  Case _sexpIself _altsIself
              _lhsOself =
                  _self
              _lhsOcrexp =
                  ({-# LINE 25 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 419 "src/CCO/Core/AG.hs" #-}
                   )
              _altsOstkoff =
                  ({-# LINE 61 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   _lhsIstkoff
                   {-# LINE 424 "src/CCO/Core/AG.hs" #-}
                   )
              ( _sexpIcrsexpl,_sexpIself) =
                  sexp_
              ( _altsIcrexpl,_altsIself) =
                  alts_ _altsOstkoff
          in  ( _lhsOcrexp,_lhsOcrexpl,_lhsOself)))
sem_Exp_Let :: T_Bind ->
               T_Exp ->
               T_Exp
sem_Exp_Let bind_ body_ =
    (\ _lhsIstkoff ->
         (let _lhsOcrexpl :: ([CR.Exp])
              _lhsOself :: Exp
              _lhsOcrexp :: (CR.Exp)
              _bindOstkoff :: Int
              _bodyOstkoff :: Int
              _bindIcrbindl :: ([CR.Bind])
              _bindIself :: Bind
              _bindIstkoff :: Int
              _bodyIcrexp :: (CR.Exp)
              _bodyIcrexpl :: ([CR.Exp])
              _bodyIself :: Exp
              _crexp =
                  ({-# LINE 36 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   CR.mkLet _lhsIstkoff _bindIcrbindl _bodyIcrexp
                   {-# LINE 450 "src/CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 40 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 455 "src/CCO/Core/AG.hs" #-}
                   )
              _self =
                  Let _bindIself _bodyIself
              _lhsOself =
                  _self
              _lhsOcrexp =
                  ({-# LINE 25 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 464 "src/CCO/Core/AG.hs" #-}
                   )
              _bindOstkoff =
                  ({-# LINE 63 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   _lhsIstkoff
                   {-# LINE 469 "src/CCO/Core/AG.hs" #-}
                   )
              _bodyOstkoff =
                  ({-# LINE 61 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   _bindIstkoff
                   {-# LINE 474 "src/CCO/Core/AG.hs" #-}
                   )
              ( _bindIcrbindl,_bindIself,_bindIstkoff) =
                  bind_ _bindOstkoff
              ( _bodyIcrexp,_bodyIcrexpl,_bodyIself) =
                  body_ _bodyOstkoff
          in  ( _lhsOcrexp,_lhsOcrexpl,_lhsOself)))
sem_Exp_Dbg :: String ->
               T_Exp
sem_Exp_Dbg info_ =
    (\ _lhsIstkoff ->
         (let _lhsOcrexpl :: ([CR.Exp])
              _lhsOself :: Exp
              _lhsOcrexp :: (CR.Exp)
              _crexp =
                  ({-# LINE 37 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   CR.mkDbg info_
                   {-# LINE 491 "src/CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 40 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 496 "src/CCO/Core/AG.hs" #-}
                   )
              _self =
                  Dbg info_
              _lhsOself =
                  _self
              _lhsOcrexp =
                  ({-# LINE 25 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 505 "src/CCO/Core/AG.hs" #-}
                   )
          in  ( _lhsOcrexp,_lhsOcrexpl,_lhsOself)))
-- ExpL --------------------------------------------------------
type ExpL = [Exp]
-- cata
sem_ExpL :: ExpL ->
            T_ExpL
sem_ExpL list =
    (Prelude.foldr sem_ExpL_Cons sem_ExpL_Nil (Prelude.map sem_Exp list))
-- semantic domain
type T_ExpL = Int ->
              ( ([CR.Exp]),ExpL)
data Inh_ExpL = Inh_ExpL {stkoff_Inh_ExpL :: Int}
data Syn_ExpL = Syn_ExpL {crexpl_Syn_ExpL :: ([CR.Exp]),self_Syn_ExpL :: ExpL}
wrap_ExpL :: T_ExpL ->
             Inh_ExpL ->
             Syn_ExpL
wrap_ExpL sem (Inh_ExpL _lhsIstkoff) =
    (let ( _lhsOcrexpl,_lhsOself) = sem _lhsIstkoff
     in  (Syn_ExpL _lhsOcrexpl _lhsOself))
sem_ExpL_Cons :: T_Exp ->
                 T_ExpL ->
                 T_ExpL
sem_ExpL_Cons hd_ tl_ =
    (\ _lhsIstkoff ->
         (let _lhsOcrexpl :: ([CR.Exp])
              _lhsOself :: ExpL
              _hdOstkoff :: Int
              _tlOstkoff :: Int
              _hdIcrexp :: (CR.Exp)
              _hdIcrexpl :: ([CR.Exp])
              _hdIself :: Exp
              _tlIcrexpl :: ([CR.Exp])
              _tlIself :: ExpL
              _lhsOcrexpl =
                  ({-# LINE 27 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   _hdIcrexpl ++ _tlIcrexpl
                   {-# LINE 543 "src/CCO/Core/AG.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _hdOstkoff =
                  ({-# LINE 61 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   _lhsIstkoff
                   {-# LINE 552 "src/CCO/Core/AG.hs" #-}
                   )
              _tlOstkoff =
                  ({-# LINE 61 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   _lhsIstkoff
                   {-# LINE 557 "src/CCO/Core/AG.hs" #-}
                   )
              ( _hdIcrexp,_hdIcrexpl,_hdIself) =
                  hd_ _hdOstkoff
              ( _tlIcrexpl,_tlIself) =
                  tl_ _tlOstkoff
          in  ( _lhsOcrexpl,_lhsOself)))
sem_ExpL_Nil :: T_ExpL
sem_ExpL_Nil =
    (\ _lhsIstkoff ->
         (let _lhsOcrexpl :: ([CR.Exp])
              _lhsOself :: ExpL
              _lhsOcrexpl =
                  ({-# LINE 27 "src/CCO/Core/AG/ToCoreRun.ag" #-}
                   []
                   {-# LINE 572 "src/CCO/Core/AG.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
          in  ( _lhsOcrexpl,_lhsOself)))
-- Mod ---------------------------------------------------------
data Mod = Mod (Exp) (BindL)
-- cata
sem_Mod :: Mod ->
           T_Mod
sem_Mod (Mod _main _binds) =
    (sem_Mod_Mod (sem_Exp _main) (sem_BindL _binds))
-- semantic domain
type T_Mod = ( (CR.Mod),Mod)
data Inh_Mod = Inh_Mod {}
data Syn_Mod = Syn_Mod {crmod_Syn_Mod :: (CR.Mod),self_Syn_Mod :: Mod}
wrap_Mod :: T_Mod ->
            Inh_Mod ->
            Syn_Mod
wrap_Mod sem (Inh_Mod) =
    (let ( _lhsOcrmod,_lhsOself) = sem
     in  (Syn_Mod _lhsOcrmod _lhsOself))
sem_Mod_Mod :: T_Exp ->
               T_BindL ->
               T_Mod
sem_Mod_Mod main_ binds_ =
    (let _lhsOcrmod :: (CR.Mod)
         _bindsOstkoff :: Int
         _mainOstkoff :: Int
         _lhsOself :: Mod
         _mainIcrexp :: (CR.Exp)
         _mainIcrexpl :: ([CR.Exp])
         _mainIself :: Exp
         _bindsIcrbindl :: ([CR.Bind])
         _bindsIself :: BindL
         _bindsIstkoff :: Int
         _lhsOcrmod =
             ({-# LINE 15 "src/CCO/Core/AG/ToCoreRun.ag" #-}
              CR.mkMod (mkHNm "Main") Nothing (length _bindsIcrbindl + 100) _bindsIcrbindl _mainIcrexp
              {-# LINE 613 "src/CCO/Core/AG.hs" #-}
              )
         _bindsOstkoff =
             ({-# LINE 66 "src/CCO/Core/AG/ToCoreRun.ag" #-}
              0
              {-# LINE 618 "src/CCO/Core/AG.hs" #-}
              )
         _mainOstkoff =
             ({-# LINE 67 "src/CCO/Core/AG/ToCoreRun.ag" #-}
              _bindsIstkoff
              {-# LINE 623 "src/CCO/Core/AG.hs" #-}
              )
         _self =
             Mod _mainIself _bindsIself
         _lhsOself =
             _self
         ( _mainIcrexp,_mainIcrexpl,_mainIself) =
             main_ _mainOstkoff
         ( _bindsIcrbindl,_bindsIself,_bindsIstkoff) =
             binds_ _bindsOstkoff
     in  ( _lhsOcrmod,_lhsOself))
-- Ref ---------------------------------------------------------
data Ref = Glob (Int)
         | Loc (Int) (Int)
-- cata
sem_Ref :: Ref ->
           T_Ref
sem_Ref (Glob _offset) =
    (sem_Ref_Glob _offset)
sem_Ref (Loc _levdiff _offset) =
    (sem_Ref_Loc _levdiff _offset)
-- semantic domain
type T_Ref = ( ([CR.RRef]),Ref)
data Inh_Ref = Inh_Ref {}
data Syn_Ref = Syn_Ref {crrefl_Syn_Ref :: ([CR.RRef]),self_Syn_Ref :: Ref}
wrap_Ref :: T_Ref ->
            Inh_Ref ->
            Syn_Ref
wrap_Ref sem (Inh_Ref) =
    (let ( _lhsOcrrefl,_lhsOself) = sem
     in  (Syn_Ref _lhsOcrrefl _lhsOself))
sem_Ref_Glob :: Int ->
                T_Ref
sem_Ref_Glob offset_ =
    (let _lhsOcrrefl :: ([CR.RRef])
         _lhsOself :: Ref
         _lhsOcrrefl =
             ({-# LINE 52 "src/CCO/Core/AG/ToCoreRun.ag" #-}
              [CR.mkModRef offset_]
              {-# LINE 662 "src/CCO/Core/AG.hs" #-}
              )
         _self =
             Glob offset_
         _lhsOself =
             _self
     in  ( _lhsOcrrefl,_lhsOself))
sem_Ref_Loc :: Int ->
               Int ->
               T_Ref
sem_Ref_Loc levdiff_ offset_ =
    (let _lhsOcrrefl :: ([CR.RRef])
         _lhsOself :: Ref
         _lhsOcrrefl =
             ({-# LINE 53 "src/CCO/Core/AG/ToCoreRun.ag" #-}
              [CR.mkLocDifRef levdiff_ offset_]
              {-# LINE 678 "src/CCO/Core/AG.hs" #-}
              )
         _self =
             Loc levdiff_ offset_
         _lhsOself =
             _self
     in  ( _lhsOcrrefl,_lhsOself))
-- RefL --------------------------------------------------------
type RefL = [Ref]
-- cata
sem_RefL :: RefL ->
            T_RefL
sem_RefL list =
    (Prelude.foldr sem_RefL_Cons sem_RefL_Nil (Prelude.map sem_Ref list))
-- semantic domain
type T_RefL = ( ([CR.RRef]),RefL)
data Inh_RefL = Inh_RefL {}
data Syn_RefL = Syn_RefL {crrefl_Syn_RefL :: ([CR.RRef]),self_Syn_RefL :: RefL}
wrap_RefL :: T_RefL ->
             Inh_RefL ->
             Syn_RefL
wrap_RefL sem (Inh_RefL) =
    (let ( _lhsOcrrefl,_lhsOself) = sem
     in  (Syn_RefL _lhsOcrrefl _lhsOself))
sem_RefL_Cons :: T_Ref ->
                 T_RefL ->
                 T_RefL
sem_RefL_Cons hd_ tl_ =
    (let _lhsOcrrefl :: ([CR.RRef])
         _lhsOself :: RefL
         _hdIcrrefl :: ([CR.RRef])
         _hdIself :: Ref
         _tlIcrrefl :: ([CR.RRef])
         _tlIself :: RefL
         _lhsOcrrefl =
             ({-# LINE 49 "src/CCO/Core/AG/ToCoreRun.ag" #-}
              _hdIcrrefl ++ _tlIcrrefl
              {-# LINE 715 "src/CCO/Core/AG.hs" #-}
              )
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIcrrefl,_hdIself) =
             hd_
         ( _tlIcrrefl,_tlIself) =
             tl_
     in  ( _lhsOcrrefl,_lhsOself))
sem_RefL_Nil :: T_RefL
sem_RefL_Nil =
    (let _lhsOcrrefl :: ([CR.RRef])
         _lhsOself :: RefL
         _lhsOcrrefl =
             ({-# LINE 49 "src/CCO/Core/AG/ToCoreRun.ag" #-}
              []
              {-# LINE 733 "src/CCO/Core/AG.hs" #-}
              )
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOcrrefl,_lhsOself))
-- SExp --------------------------------------------------------
data SExp = Int (Int)
          | Var (Ref)
-- cata
sem_SExp :: SExp ->
            T_SExp
sem_SExp (Int _i) =
    (sem_SExp_Int _i)
sem_SExp (Var _x) =
    (sem_SExp_Var (sem_Ref _x))
-- semantic domain
type T_SExp = ( ([CR.SExp]),SExp)
data Inh_SExp = Inh_SExp {}
data Syn_SExp = Syn_SExp {crsexpl_Syn_SExp :: ([CR.SExp]),self_Syn_SExp :: SExp}
wrap_SExp :: T_SExp ->
             Inh_SExp ->
             Syn_SExp
wrap_SExp sem (Inh_SExp) =
    (let ( _lhsOcrsexpl,_lhsOself) = sem
     in  (Syn_SExp _lhsOcrsexpl _lhsOself))
sem_SExp_Int :: Int ->
                T_SExp
sem_SExp_Int i_ =
    (let _lhsOcrsexpl :: ([CR.SExp])
         _lhsOself :: SExp
         _lhsOcrsexpl =
             ({-# LINE 21 "src/CCO/Core/AG/ToCoreRun.ag" #-}
              [CR.mkInt' i_]
              {-# LINE 768 "src/CCO/Core/AG.hs" #-}
              )
         _self =
             Int i_
         _lhsOself =
             _self
     in  ( _lhsOcrsexpl,_lhsOself))
sem_SExp_Var :: T_Ref ->
                T_SExp
sem_SExp_Var x_ =
    (let _lhsOcrsexpl :: ([CR.SExp])
         _lhsOself :: SExp
         _xIcrrefl :: ([CR.RRef])
         _xIself :: Ref
         _lhsOcrsexpl =
             ({-# LINE 22 "src/CCO/Core/AG/ToCoreRun.ag" #-}
              [CR.mkVar' $ head _xIcrrefl]
              {-# LINE 785 "src/CCO/Core/AG.hs" #-}
              )
         _self =
             Var _xIself
         _lhsOself =
             _self
         ( _xIcrrefl,_xIself) =
             x_
     in  ( _lhsOcrsexpl,_lhsOself))
-- SExpL -------------------------------------------------------
type SExpL = [SExp]
-- cata
sem_SExpL :: SExpL ->
             T_SExpL
sem_SExpL list =
    (Prelude.foldr sem_SExpL_Cons sem_SExpL_Nil (Prelude.map sem_SExp list))
-- semantic domain
type T_SExpL = ( ([CR.SExp]),SExpL)
data Inh_SExpL = Inh_SExpL {}
data Syn_SExpL = Syn_SExpL {crsexpl_Syn_SExpL :: ([CR.SExp]),self_Syn_SExpL :: SExpL}
wrap_SExpL :: T_SExpL ->
              Inh_SExpL ->
              Syn_SExpL
wrap_SExpL sem (Inh_SExpL) =
    (let ( _lhsOcrsexpl,_lhsOself) = sem
     in  (Syn_SExpL _lhsOcrsexpl _lhsOself))
sem_SExpL_Cons :: T_SExp ->
                  T_SExpL ->
                  T_SExpL
sem_SExpL_Cons hd_ tl_ =
    (let _lhsOcrsexpl :: ([CR.SExp])
         _lhsOself :: SExpL
         _hdIcrsexpl :: ([CR.SExp])
         _hdIself :: SExp
         _tlIcrsexpl :: ([CR.SExp])
         _tlIself :: SExpL
         _lhsOcrsexpl =
             ({-# LINE 18 "src/CCO/Core/AG/ToCoreRun.ag" #-}
              _hdIcrsexpl ++ _tlIcrsexpl
              {-# LINE 824 "src/CCO/Core/AG.hs" #-}
              )
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIcrsexpl,_hdIself) =
             hd_
         ( _tlIcrsexpl,_tlIself) =
             tl_
     in  ( _lhsOcrsexpl,_lhsOself))
sem_SExpL_Nil :: T_SExpL
sem_SExpL_Nil =
    (let _lhsOcrsexpl :: ([CR.SExp])
         _lhsOself :: SExpL
         _lhsOcrsexpl =
             ({-# LINE 18 "src/CCO/Core/AG/ToCoreRun.ag" #-}
              []
              {-# LINE 842 "src/CCO/Core/AG.hs" #-}
              )
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOcrsexpl,_lhsOself))