

-- UUAGC 0.9.52.1 (src/lib/CCO/HM/AG.ag)
module CCO.HM.AG where

{-# LINE 5 "src/lib/CCO/HM/AG/HMToCR.ag" #-}

import qualified CCO.Core.AG as CR
import qualified Data.Map as Map
{-# LINE 11 "src/lib/CCO/HM/AG.hs" #-}

{-# LINE 2 "src/lib/CCO/HM/AG/Base.ag" #-}

import CCO.SourcePos
{-# LINE 16 "src/lib/CCO/HM/AG.hs" #-}
{-# LINE 10 "src/lib/CCO/HM/AG/HMToCR.ag" #-}

type Environment = Map.Map Var CR.Ref
{-# LINE 20 "src/lib/CCO/HM/AG.hs" #-}

{-# LINE 69 "src/lib/CCO/HM/AG/HMToCR.ag" #-}

expToSExp :: CR.Exp -> CR.SExp
expToSExp (CR.SExp se) = se
expToSExp _            = error "Exp not a SExp"

varToRef :: Int -> Environment -> Var -> CR.Ref
varToRef currentLevel env x = case env Map.! x of
    CR.Loc bindLevel offset -> CR.Loc (currentLevel - bindLevel) offset
    g@(CR.Glob _) -> g
{-# LINE 32 "src/lib/CCO/HM/AG.hs" #-}

{-# LINE 10 "src/lib/CCO/HM/AG/Base.ag" #-}

type Var = String    -- ^ Type of variables.
{-# LINE 37 "src/lib/CCO/HM/AG.hs" #-}
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
             ({-# LINE 21 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
              map (\(o,e) -> CR.Bind (CR.Glob o) e) $ Map.toAscList _rootIglobalBinds
              {-# LINE 71 "src/lib/CCO/HM/AG.hs" #-}
              )
         _lhsOmod =
             ({-# LINE 22 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
              CR.Mod _rootIexp _modBinds
              {-# LINE 76 "src/lib/CCO/HM/AG.hs" #-}
              )
         _rootOlevel =
             ({-# LINE 24 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
              0
              {-# LINE 81 "src/lib/CCO/HM/AG.hs" #-}
              )
         _rootOglobalBinds =
             ({-# LINE 25 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
              Map.empty
              {-# LINE 86 "src/lib/CCO/HM/AG.hs" #-}
              )
         _rootOenv =
             ({-# LINE 26 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
              Map.empty
              {-# LINE 91 "src/lib/CCO/HM/AG.hs" #-}
              )
         _rootOoffset =
             ({-# LINE 27 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
              0
              {-# LINE 96 "src/lib/CCO/HM/AG.hs" #-}
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
                  ({-# LINE 38 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _tIexp
                   {-# LINE 153 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _lhsOglobalBinds =
                  ({-# LINE 33 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _tIglobalBinds
                   {-# LINE 158 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _lhsOoffset =
                  ({-# LINE 37 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _tIoffset
                   {-# LINE 163 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _tOenv =
                  ({-# LINE 30 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIenv
                   {-# LINE 168 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _tOglobalBinds =
                  ({-# LINE 34 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIglobalBinds
                   {-# LINE 173 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _tOlevel =
                  ({-# LINE 31 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIlevel
                   {-# LINE 178 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _tOoffset =
                  ({-# LINE 36 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIoffset
                   {-# LINE 183 "src/lib/CCO/HM/AG.hs" #-}
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
                  ({-# LINE 41 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   CR.SExp (CR.Int i_)
                   {-# LINE 235 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _lhsOoffset =
                  ({-# LINE 42 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIoffset + 1
                   {-# LINE 240 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _self =
                  Nat i_
              _lhsOself =
                  _self
              _lhsOglobalBinds =
                  ({-# LINE 33 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIglobalBinds
                   {-# LINE 249 "src/lib/CCO/HM/AG.hs" #-}
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
                  ({-# LINE 44 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   CR.SExp (CR.Var (varToRef _lhsIlevel _lhsIenv x_))
                   {-# LINE 266 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _lhsOoffset =
                  ({-# LINE 45 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIoffset + 1
                   {-# LINE 271 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _self =
                  Var x_
              _lhsOself =
                  _self
              _lhsOglobalBinds =
                  ({-# LINE 33 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIglobalBinds
                   {-# LINE 280 "src/lib/CCO/HM/AG.hs" #-}
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
                  ({-# LINE 47 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   CR.Lam [_ref] _t1Iexp
                   {-# LINE 306 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _lhsOoffset =
                  ({-# LINE 48 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIoffset + 1
                   {-# LINE 311 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _ref =
                  ({-# LINE 49 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   CR.Loc (_lhsIlevel + 1) 0 :: CR.Ref
                   {-# LINE 316 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t1Oenv =
                  ({-# LINE 50 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   Map.insert x_ _ref _lhsIenv
                   {-# LINE 321 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t1Olevel =
                  ({-# LINE 51 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIlevel + 1
                   {-# LINE 326 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t1Ooffset =
                  ({-# LINE 52 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   1
                   {-# LINE 331 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _self =
                  Lam x_ _t1Iself
              _lhsOself =
                  _self
              _lhsOglobalBinds =
                  ({-# LINE 33 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _t1IglobalBinds
                   {-# LINE 340 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t1OglobalBinds =
                  ({-# LINE 34 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIglobalBinds
                   {-# LINE 345 "src/lib/CCO/HM/AG.hs" #-}
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
                  ({-# LINE 54 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   CR.App _t1Iexp [expToSExp _t2Iexp]
                   {-# LINE 381 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t1Ooffset =
                  ({-# LINE 55 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   1
                   {-# LINE 386 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t2Ooffset =
                  ({-# LINE 56 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIoffset
                   {-# LINE 391 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _self =
                  App _t1Iself _t2Iself
              _lhsOself =
                  _self
              _lhsOglobalBinds =
                  ({-# LINE 33 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _t2IglobalBinds
                   {-# LINE 400 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _lhsOoffset =
                  ({-# LINE 37 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _t2Ioffset
                   {-# LINE 405 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t1Oenv =
                  ({-# LINE 30 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIenv
                   {-# LINE 410 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t1OglobalBinds =
                  ({-# LINE 34 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIglobalBinds
                   {-# LINE 415 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t1Olevel =
                  ({-# LINE 31 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIlevel
                   {-# LINE 420 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t2Oenv =
                  ({-# LINE 30 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIenv
                   {-# LINE 425 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t2OglobalBinds =
                  ({-# LINE 34 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _t1IglobalBinds
                   {-# LINE 430 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t2Olevel =
                  ({-# LINE 31 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIlevel
                   {-# LINE 435 "src/lib/CCO/HM/AG.hs" #-}
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
              _lhsOoffset :: Int
              _t2OglobalBinds :: (Map.Map Int CR.Exp)
              _lhsOglobalBinds :: (Map.Map Int CR.Exp)
              _t1Ooffset :: Int
              _t2Ooffset :: Int
              _t2Oenv :: Environment
              _lhsOself :: Tm_
              _t1Oenv :: Environment
              _t1OglobalBinds :: (Map.Map Int CR.Exp)
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
              _lhsOexp =
                  ({-# LINE 58 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   if _lhsIlevel == 0 then _t2Iexp else CR.Let (CR.Bind _ref _t1Iexp) _t2Iexp
                   {-# LINE 474 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _lhsOoffset =
                  ({-# LINE 59 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _t2Ioffset
                   {-# LINE 479 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _globalOffset =
                  ({-# LINE 60 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   Map.size _lhsIglobalBinds
                   {-# LINE 484 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _ref =
                  ({-# LINE 61 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   if _lhsIlevel == 0 then CR.Glob _globalOffset else CR.Loc _lhsIlevel _lhsIoffset :: CR.Ref
                   {-# LINE 489 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t2OglobalBinds =
                  ({-# LINE 62 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   if _lhsIlevel == 0 then Map.insert _globalOffset _t1Iexp _lhsIglobalBinds else _lhsIglobalBinds
                   {-# LINE 494 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _lhsOglobalBinds =
                  ({-# LINE 63 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _t2IglobalBinds
                   {-# LINE 499 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t1Ooffset =
                  ({-# LINE 64 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIoffset
                   {-# LINE 504 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t2Ooffset =
                  ({-# LINE 65 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _t1Ioffset
                   {-# LINE 509 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t2Oenv =
                  ({-# LINE 66 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   Map.insert x_ _ref _lhsIenv
                   {-# LINE 514 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _self =
                  Let x_ _t1Iself _t2Iself
              _lhsOself =
                  _self
              _t1Oenv =
                  ({-# LINE 30 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIenv
                   {-# LINE 523 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t1OglobalBinds =
                  ({-# LINE 34 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIglobalBinds
                   {-# LINE 528 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t1Olevel =
                  ({-# LINE 31 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIlevel
                   {-# LINE 533 "src/lib/CCO/HM/AG.hs" #-}
                   )
              _t2Olevel =
                  ({-# LINE 31 "src/lib/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIlevel
                   {-# LINE 538 "src/lib/CCO/HM/AG.hs" #-}
                   )
              ( _t1Iexp,_t1IglobalBinds,_t1Ioffset,_t1Iself) =
                  t1_ _t1Oenv _t1OglobalBinds _t1Olevel _t1Ooffset
              ( _t2Iexp,_t2IglobalBinds,_t2Ioffset,_t2Iself) =
                  t2_ _t2Oenv _t2OglobalBinds _t2Olevel _t2Ooffset
          in  ( _lhsOexp,_lhsOglobalBinds,_lhsOoffset,_lhsOself)))