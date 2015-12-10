

-- UUAGC 0.9.52.1 (src/lib/CCO/HM/AG.ag)
module CCO.HM.AG where

{-# LINE 5 "src/lib/CCO/HM/AG/HMToCR.ag" #-}

import qualified CCO.Core.AG as CR
import qualified Data.Map as Map

import Debug.Trace
{-# LINE 13 "src/lib/CCO/HM/AG.hs" #-}

{-# LINE 2 "src/lib/CCO/HM/AG/Base.ag" #-}

import CCO.SourcePos
{-# LINE 18 "src/lib/CCO/HM/AG.hs" #-}
{-# LINE 12 "src/lib/CCO/HM/AG/HMToCR.ag" #-}

data DeclRef
  = DeclLoc { declLocLevel :: Int, declLocOffset :: Int}
  | DeclGlob { declGlobOffset :: Int }
  deriving (Eq, Ord, Show, Read)

type Environment = Map.Map Var DeclRef
{-# LINE 27 "src/lib/CCO/HM/AG.hs" #-}

{-# LINE 102 "src/lib/CCO/HM/AG/HMToCR.ag" #-}

expToSExp :: CR.Exp -> CR.SExp
expToSExp (CR.SExp se) = se
expToSExp _            = error "Exp not a SExp"

varToRef :: Int -> Environment -> Var -> CR.Ref
varToRef currentLevel env x = case env Map.! x of
    DeclLoc bindLevel offset -> CR.Loc (currentLevel - bindLevel) offset
    DeclGlob offset -> CR.Glob offset
{-# LINE 39 "src/lib/CCO/HM/AG.hs" #-}

{-# LINE 10 "src/lib/CCO/HM/AG/Base.ag" #-}

type Var = String    -- ^ Type of variables.
{-# LINE 44 "src/lib/CCO/HM/AG.hs" #-}
-- HMToCR ------------------------------------------------------
data HMToCR = HMToCR (Tm)
-- cata
sem_HMToCR :: HMToCR ->
              T_HMToCR
sem_HMToCR (HMToCR _root) =
    (sem_HMToCR_HMToCR (sem_Tm _root))
-- semantic domain
type T_HMToCR = ( (CR.Mod),HMToCR)
data Inh_HMToCR = Inh_HMToCR {}
data Syn_HMToCR = Syn_HMToCR {mod_Syn_HMToCR :: (CR.Mod),self_Syn_HMToCR :: HMToCR}
wrap_HMToCR :: T_HMToCR ->
               Inh_HMToCR ->
               Syn_HMToCR
wrap_HMToCR sem (Inh_HMToCR) =
    (let ( _lhsOmod,_lhsOself) = sem
     in  (Syn_HMToCR _lhsOmod _lhsOself))
sem_HMToCR_HMToCR :: T_Tm ->
                     T_HMToCR
sem_HMToCR_HMToCR root_ =
    (let _lhsOmod :: (CR.Mod)
         _rootOlevel :: Int
         _rootOglobalBinds :: (Map.Map Int CR.Exp)
         _rootOenv :: Environment
         _rootOoffset :: Int
         _lhsOself :: HMToCR
         _rootIexp :: (CR.Exp)
         _rootIglobalBinds :: (Map.Map Int CR.Exp)
         _rootIoffset :: Int
         _rootIself :: Tm
         _modBinds =
             ({-# LINE 28 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
              map (\(o,e) -> CR.Bind (CR.Glob o) e) $ Map.toAscList _rootIglobalBinds
              {-# LINE 78 "src/lib/CCO/HM/AG.hs" #-}
              )
         _lhsOmod =
             ({-# LINE 29 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
              CR.Mod _rootIexp _modBinds
              {-# LINE 83 "src/lib/CCO/HM/AG.hs" #-}
              )
         _rootOlevel =
             ({-# LINE 31 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
              0
              {-# LINE 88 "src/lib/CCO/HM/AG.hs" #-}
              )
         _rootOglobalBinds =
             ({-# LINE 32 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
              Map.empty
              {-# LINE 93 "src/lib/CCO/HM/AG.hs" #-}
              )
         _rootOenv =
             ({-# LINE 33 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
              Map.empty
              {-# LINE 98 "src/lib/CCO/HM/AG.hs" #-}
              )
         _rootOoffset =
             ({-# LINE 34 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
              0
              {-# LINE 103 "src/lib/CCO/HM/AG.hs" #-}
              )
         _self =
             HMToCR _rootIself
         _lhsOself =
             _self
         ( _rootIexp,_rootIglobalBinds,_rootIoffset,_rootIself) =
             root_ _rootOenv _rootOglobalBinds _rootOlevel _rootOoffset
     in  ( _lhsOmod,_lhsOself))
-- Tm ----------------------------------------------------------
data Tm = Tm (SourcePos) (Tm_)
-- cata
sem_Tm :: Tm ->
          T_Tm
sem_Tm (Tm _pos _t) =
    (sem_Tm_Tm _pos (sem_Tm_ _t))
-- semantic domain
type T_Tm = Environment ->
            (Map.Map Int CR.Exp) ->
            Int ->
            Int ->
            ( (CR.Exp),(Map.Map Int CR.Exp),Int,Tm)
data Inh_Tm = Inh_Tm {env_Inh_Tm :: Environment,globalBinds_Inh_Tm :: (Map.Map Int CR.Exp),level_Inh_Tm :: Int,offset_Inh_Tm :: Int}
data Syn_Tm = Syn_Tm {exp_Syn_Tm :: (CR.Exp),globalBinds_Syn_Tm :: (Map.Map Int CR.Exp),offset_Syn_Tm :: Int,self_Syn_Tm :: Tm}
wrap_Tm :: T_Tm ->
           Inh_Tm ->
           Syn_Tm
wrap_Tm sem (Inh_Tm _lhsIenv _lhsIglobalBinds _lhsIlevel _lhsIoffset) =
    (let ( _lhsOexp,_lhsOglobalBinds,_lhsOoffset,_lhsOself) = sem _lhsIenv _lhsIglobalBinds _lhsIlevel _lhsIoffset
     in  (Syn_Tm _lhsOexp _lhsOglobalBinds _lhsOoffset _lhsOself))
sem_Tm_Tm :: SourcePos ->
             T_Tm_ ->
             T_Tm
sem_Tm_Tm pos_ t_ =
    (\ _lhsIenv
       _lhsIglobalBinds
       _lhsIlevel
       _lhsIoffset ->
         (let _lhsOself :: Tm
              _lhsOexp :: (CR.Exp)
              _lhsOglobalBinds :: (Map.Map Int CR.Exp)
              _lhsOoffset :: Int
              _tOenv :: Environment
              _tOglobalBinds :: (Map.Map Int CR.Exp)
              _tOlevel :: Int
              _tOoffset :: Int
              _tIexp :: (CR.Exp)
              _tIglobalBinds :: (Map.Map Int CR.Exp)
              _tIoffset :: Int
              _tIself :: Tm_
              _self =
                  Tm pos_ _tIself
              _lhsOself =
                  _self
              _lhsOexp =
                  ({-# LINE 50 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _tIexp
                   {-# LINE 160 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _lhsOglobalBinds =
                  ({-# LINE 44 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _tIglobalBinds
                   {-# LINE 165 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _lhsOoffset =
                  ({-# LINE 48 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _tIoffset
                   {-# LINE 170 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _tOenv =
                  ({-# LINE 38 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIenv
                   {-# LINE 175 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _tOglobalBinds =
                  ({-# LINE 45 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIglobalBinds
                   {-# LINE 180 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _tOlevel =
                  ({-# LINE 41 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIlevel
                   {-# LINE 185 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _tOoffset =
                  ({-# LINE 47 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIoffset
                   {-# LINE 190 "src/lib/CCO/HM/AG.hs" #-}
                   )
              ( _tIexp,_tIglobalBinds,_tIoffset,_tIself) =
                  t_ _tOenv _tOglobalBinds _tOlevel _tOoffset
          in  ( _lhsOexp,_lhsOglobalBinds,_lhsOoffset,_lhsOself)))
-- Tm_ ---------------------------------------------------------
data Tm_ = Nat (Int)
         | Var (Var)
         | Lam (Var) (Tm)
         | App (Tm) (Tm)
         | Let (Var) (Tm) (Tm)
-- cata
sem_Tm_ :: Tm_ ->
           T_Tm_
sem_Tm_ (Nat _i) =
    (sem_Tm__Nat _i)
sem_Tm_ (Var _x) =
    (sem_Tm__Var _x)
sem_Tm_ (Lam _x _t1) =
    (sem_Tm__Lam _x (sem_Tm _t1))
sem_Tm_ (App _t1 _t2) =
    (sem_Tm__App (sem_Tm _t1) (sem_Tm _t2))
sem_Tm_ (Let _x _t1 _t2) =
    (sem_Tm__Let _x (sem_Tm _t1) (sem_Tm _t2))
-- semantic domain
type T_Tm_ = Environment ->
             (Map.Map Int CR.Exp) ->
             Int ->
             Int ->
             ( (CR.Exp),(Map.Map Int CR.Exp),Int,Tm_)
data Inh_Tm_ = Inh_Tm_ {env_Inh_Tm_ :: Environment,globalBinds_Inh_Tm_ :: (Map.Map Int CR.Exp),level_Inh_Tm_ :: Int,offset_Inh_Tm_ :: Int}
data Syn_Tm_ = Syn_Tm_ {exp_Syn_Tm_ :: (CR.Exp),globalBinds_Syn_Tm_ :: (Map.Map Int CR.Exp),offset_Syn_Tm_ :: Int,self_Syn_Tm_ :: Tm_}
wrap_Tm_ :: T_Tm_ ->
            Inh_Tm_ ->
            Syn_Tm_
wrap_Tm_ sem (Inh_Tm_ _lhsIenv _lhsIglobalBinds _lhsIlevel _lhsIoffset) =
    (let ( _lhsOexp,_lhsOglobalBinds,_lhsOoffset,_lhsOself) = sem _lhsIenv _lhsIglobalBinds _lhsIlevel _lhsIoffset
     in  (Syn_Tm_ _lhsOexp _lhsOglobalBinds _lhsOoffset _lhsOself))
sem_Tm__Nat :: Int ->
               T_Tm_
sem_Tm__Nat i_ =
    (\ _lhsIenv
       _lhsIglobalBinds
       _lhsIlevel
       _lhsIoffset ->
         (let _lhsOexp :: (CR.Exp)
              _lhsOoffset :: Int
              _lhsOself :: Tm_
              _lhsOglobalBinds :: (Map.Map Int CR.Exp)
              _lhsOexp =
                  ({-# LINE 53 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   CR.SExp (CR.Int i_)
                   {-# LINE 242 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _lhsOoffset =
                  ({-# LINE 54 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIoffset + 1
                   {-# LINE 247 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _self =
                  Nat i_
              _lhsOself =
                  _self
              _lhsOglobalBinds =
                  ({-# LINE 44 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIglobalBinds
                   {-# LINE 256 "src/lib/CCO/HM/AG.hs" #-}
                   )
          in  ( _lhsOexp,_lhsOglobalBinds,_lhsOoffset,_lhsOself)))
sem_Tm__Var :: Var ->
               T_Tm_
sem_Tm__Var x_ =
    (\ _lhsIenv
       _lhsIglobalBinds
       _lhsIlevel
       _lhsIoffset ->
         (let _lhsOexp :: (CR.Exp)
              _lhsOoffset :: Int
              _lhsOself :: Tm_
              _lhsOglobalBinds :: (Map.Map Int CR.Exp)
              _lhsOexp =
                  ({-# LINE 56 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   CR.SExp (CR.Var (varToRef _lhsIlevel _lhsIenv x_))
                   {-# LINE 273 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _lhsOoffset =
                  ({-# LINE 57 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIoffset + 1
                   {-# LINE 278 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _self =
                  Var x_
              _lhsOself =
                  _self
              _lhsOglobalBinds =
                  ({-# LINE 44 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIglobalBinds
                   {-# LINE 287 "src/lib/CCO/HM/AG.hs" #-}
                   )
          in  ( _lhsOexp,_lhsOglobalBinds,_lhsOoffset,_lhsOself)))
sem_Tm__Lam :: Var ->
               T_Tm ->
               T_Tm_
sem_Tm__Lam x_ t1_ =
    (\ _lhsIenv
       _lhsIglobalBinds
       _lhsIlevel
       _lhsIoffset ->
         (let _lhsOexp :: (CR.Exp)
              _lhsOoffset :: Int
              _t1Oenv :: Environment
              _t1Olevel :: Int
              _t1Ooffset :: Int
              _lhsOself :: Tm_
              _lhsOglobalBinds :: (Map.Map Int CR.Exp)
              _t1OglobalBinds :: (Map.Map Int CR.Exp)
              _t1Iexp :: (CR.Exp)
              _t1IglobalBinds :: (Map.Map Int CR.Exp)
              _t1Ioffset :: Int
              _t1Iself :: Tm
              _lhsOexp =
                  ({-# LINE 59 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   CR.Lam [CR.Loc 0 0] _t1Iexp
                   {-# LINE 313 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _lhsOoffset =
                  ({-# LINE 60 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIoffset + 1
                   {-# LINE 318 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _ref =
                  ({-# LINE 61 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   DeclLoc (_lhsIlevel + 1) 0
                   {-# LINE 323 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t1Oenv =
                  ({-# LINE 62 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   Map.insert x_ _ref _lhsIenv
                   {-# LINE 328 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t1Olevel =
                  ({-# LINE 63 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIlevel + 1
                   {-# LINE 333 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t1Ooffset =
                  ({-# LINE 64 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   1
                   {-# LINE 338 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _self =
                  Lam x_ _t1Iself
              _lhsOself =
                  _self
              _lhsOglobalBinds =
                  ({-# LINE 44 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _t1IglobalBinds
                   {-# LINE 347 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t1OglobalBinds =
                  ({-# LINE 45 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIglobalBinds
                   {-# LINE 352 "src/lib/CCO/HM/AG.hs" #-}
                   )
              ( _t1Iexp,_t1IglobalBinds,_t1Ioffset,_t1Iself) =
                  t1_ _t1Oenv _t1OglobalBinds _t1Olevel _t1Ooffset
          in  ( _lhsOexp,_lhsOglobalBinds,_lhsOoffset,_lhsOself)))
sem_Tm__App :: T_Tm ->
               T_Tm ->
               T_Tm_
sem_Tm__App t1_ t2_ =
    (\ _lhsIenv
       _lhsIglobalBinds
       _lhsIlevel
       _lhsIoffset ->
         (let _lhsOexp :: (CR.Exp)
              _t1Ooffset :: Int
              _t2Ooffset :: Int
              _lhsOself :: Tm_
              _lhsOglobalBinds :: (Map.Map Int CR.Exp)
              _lhsOoffset :: Int
              _t1Oenv :: Environment
              _t1OglobalBinds :: (Map.Map Int CR.Exp)
              _t1Olevel :: Int
              _t2Oenv :: Environment
              _t2OglobalBinds :: (Map.Map Int CR.Exp)
              _t2Olevel :: Int
              _t1Iexp :: (CR.Exp)
              _t1IglobalBinds :: (Map.Map Int CR.Exp)
              _t1Ioffset :: Int
              _t1Iself :: Tm
              _t2Iexp :: (CR.Exp)
              _t2IglobalBinds :: (Map.Map Int CR.Exp)
              _t2Ioffset :: Int
              _t2Iself :: Tm
              _lhsOexp =
                  ({-# LINE 66 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   CR.App _t1Iexp [expToSExp _t2Iexp]
                   {-# LINE 388 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t1Ooffset =
                  ({-# LINE 67 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   1
                   {-# LINE 393 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t2Ooffset =
                  ({-# LINE 68 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIoffset
                   {-# LINE 398 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _self =
                  App _t1Iself _t2Iself
              _lhsOself =
                  _self
              _lhsOglobalBinds =
                  ({-# LINE 44 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _t2IglobalBinds
                   {-# LINE 407 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _lhsOoffset =
                  ({-# LINE 48 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _t2Ioffset
                   {-# LINE 412 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t1Oenv =
                  ({-# LINE 38 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIenv
                   {-# LINE 417 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t1OglobalBinds =
                  ({-# LINE 45 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIglobalBinds
                   {-# LINE 422 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t1Olevel =
                  ({-# LINE 41 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIlevel
                   {-# LINE 427 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t2Oenv =
                  ({-# LINE 38 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIenv
                   {-# LINE 432 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t2OglobalBinds =
                  ({-# LINE 45 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _t1IglobalBinds
                   {-# LINE 437 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t2Olevel =
                  ({-# LINE 41 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIlevel
                   {-# LINE 442 "src/lib/CCO/HM/AG.hs" #-}
                   )
              ( _t1Iexp,_t1IglobalBinds,_t1Ioffset,_t1Iself) =
                  t1_ _t1Oenv _t1OglobalBinds _t1Olevel _t1Ooffset
              ( _t2Iexp,_t2IglobalBinds,_t2Ioffset,_t2Iself) =
                  t2_ _t2Oenv _t2OglobalBinds _t2Olevel _t2Ooffset
          in  ( _lhsOexp,_lhsOglobalBinds,_lhsOoffset,_lhsOself)))
sem_Tm__Let :: Var ->
               T_Tm ->
               T_Tm ->
               T_Tm_
sem_Tm__Let x_ t1_ t2_ =
    (\ _lhsIenv
       _lhsIglobalBinds
       _lhsIlevel
       _lhsIoffset ->
         (let _lhsOexp :: (CR.Exp)
              _t1OglobalBinds :: (Map.Map Int CR.Exp)
              _t2OglobalBinds :: (Map.Map Int CR.Exp)
              _lhsOglobalBinds :: (Map.Map Int CR.Exp)
              _t1Oenv :: Environment
              _t2Oenv :: Environment
              _t1Ooffset :: Int
              _t2Ooffset :: Int
              _lhsOoffset :: Int
              _lhsOself :: Tm_
              _t1Olevel :: Int
              _t2Olevel :: Int
              _t1Iexp :: (CR.Exp)
              _t1IglobalBinds :: (Map.Map Int CR.Exp)
              _t1Ioffset :: Int
              _t1Iself :: Tm
              _t2Iexp :: (CR.Exp)
              _t2IglobalBinds :: (Map.Map Int CR.Exp)
              _t2Ioffset :: Int
              _t2Iself :: Tm
              _isGlobal =
                  ({-# LINE 70 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIlevel == 0
                   {-# LINE 481 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _lhsOexp =
                  ({-# LINE 71 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   if _isGlobal
                       then _t2Iexp
                       else CR.Let (CR.Bind (CR.Loc 0 _lhsIoffset) _t1Iexp) _t2Iexp
                   {-# LINE 488 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _globalOffset =
                  ({-# LINE 78 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   Map.size _lhsIglobalBinds
                   {-# LINE 493 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _ref =
                  ({-# LINE 80 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   if _isGlobal
                     then DeclGlob _globalOffset
                     else DeclLoc _lhsIlevel _lhsIoffset
                   {-# LINE 500 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _newGlobalBinds =
                  ({-# LINE 85 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   if _isGlobal
                     then Map.insert _globalOffset _t1Iexp _lhsIglobalBinds
                     else _lhsIglobalBinds
                   {-# LINE 507 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t1OglobalBinds =
                  ({-# LINE 89 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _newGlobalBinds
                   {-# LINE 512 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t2OglobalBinds =
                  ({-# LINE 90 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _t1IglobalBinds
                   {-# LINE 517 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _lhsOglobalBinds =
                  ({-# LINE 91 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _t2IglobalBinds
                   {-# LINE 522 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _newEnv =
                  ({-# LINE 93 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   Map.insert x_ _ref _lhsIenv
                   {-# LINE 527 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t1Oenv =
                  ({-# LINE 94 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _newEnv
                   {-# LINE 532 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t2Oenv =
                  ({-# LINE 95 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _newEnv
                   {-# LINE 537 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t1Ooffset =
                  ({-# LINE 97 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIoffset
                   {-# LINE 542 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t2Ooffset =
                  ({-# LINE 98 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _t1Ioffset
                   {-# LINE 547 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _lhsOoffset =
                  ({-# LINE 99 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _t2Ioffset
                   {-# LINE 552 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _self =
                  Let x_ _t1Iself _t2Iself
              _lhsOself =
                  _self
              _t1Olevel =
                  ({-# LINE 41 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIlevel
                   {-# LINE 561 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t2Olevel =
                  ({-# LINE 41 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIlevel
                   {-# LINE 566 "src/lib/CCO/HM/AG.hs" #-}
                   )
              ( _t1Iexp,_t1IglobalBinds,_t1Ioffset,_t1Iself) =
                  t1_ _t1Oenv _t1OglobalBinds _t1Olevel _t1Ooffset
              ( _t2Iexp,_t2IglobalBinds,_t2Ioffset,_t2Iself) =
                  t2_ _t2Oenv _t2OglobalBinds _t2Olevel _t2Ooffset
          in  ( _lhsOexp,_lhsOglobalBinds,_lhsOoffset,_lhsOself)))