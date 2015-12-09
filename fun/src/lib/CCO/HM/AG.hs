

-- UUAGC 0.9.52.1 (src/CCO/HM/AG.ag)
module CCO.HM.AG where

{-# LINE 5 "src/CCO/HM/AG/HMToCR.ag" #-}

import qualified CCO.Core.AG as CR
{-# LINE 10 "src/CCO/HM/AG.hs" #-}

{-# LINE 2 "src/CCO/HM/AG/Base.ag" #-}

import CCO.SourcePos
{-# LINE 15 "src/CCO/HM/AG.hs" #-}
{-# LINE 10 "src/CCO/HM/AG/Base.ag" #-}

type Var = String    -- ^ Type of variables.
{-# LINE 19 "src/CCO/HM/AG.hs" #-}
-- HMToCR ------------------------------------------------------
data HMToCR = HMToCR (Tm)
-- cata
sem_HMToCR :: HMToCR ->
              T_HMToCR
sem_HMToCR (HMToCR _tm) =
    (sem_HMToCR_HMToCR (sem_Tm _tm))
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
sem_HMToCR_HMToCR tm_ =
    (let _lhsOmod :: (CR.Mod)
         _lhsOself :: HMToCR
         _tmOlevDiff :: Int
         _tmOoffset :: Int
         _tmIexp :: (CR.Exp)
         _tmIoffset :: Int
         _tmIself :: Tm
         _lhsOmod =
             ({-# LINE 16 "src/CCO/HM/AG/HMToCR.ag" #-}
              undefined
              {-# LINE 50 "src/CCO/HM/AG.hs" #-}
              )
         _self =
             HMToCR _tmIself
         _lhsOself =
             _self
         _tmOlevDiff =
             ({-# LINE 19 "src/CCO/HM/AG/HMToCR.ag" #-}
              error "missing rule: HMToCR.HMToCR.tm.levDiff"
              {-# LINE 59 "src/CCO/HM/AG.hs" #-}
              )
         _tmOoffset =
             ({-# LINE 20 "src/CCO/HM/AG/HMToCR.ag" #-}
              error "missing rule: HMToCR.HMToCR.tm.offset"
              {-# LINE 64 "src/CCO/HM/AG.hs" #-}
              )
         ( _tmIexp,_tmIoffset,_tmIself) =
             tm_ _tmOlevDiff _tmOoffset
     in  ( _lhsOmod,_lhsOself))
-- Tm ----------------------------------------------------------
data Tm = Tm (SourcePos) (Tm_)
-- cata
sem_Tm :: Tm ->
          T_Tm
sem_Tm (Tm _pos _t) =
    (sem_Tm_Tm _pos (sem_Tm_ _t))
-- semantic domain
type T_Tm = Int ->
            Int ->
            ( (CR.Exp),Int,Tm)
data Inh_Tm = Inh_Tm {levDiff_Inh_Tm :: Int,offset_Inh_Tm :: Int}
data Syn_Tm = Syn_Tm {exp_Syn_Tm :: (CR.Exp),offset_Syn_Tm :: Int,self_Syn_Tm :: Tm}
wrap_Tm :: T_Tm ->
           Inh_Tm ->
           Syn_Tm
wrap_Tm sem (Inh_Tm _lhsIlevDiff _lhsIoffset) =
    (let ( _lhsOexp,_lhsOoffset,_lhsOself) = sem _lhsIlevDiff _lhsIoffset
     in  (Syn_Tm _lhsOexp _lhsOoffset _lhsOself))
sem_Tm_Tm :: SourcePos ->
             T_Tm_ ->
             T_Tm
sem_Tm_Tm pos_ t_ =
    (\ _lhsIlevDiff
       _lhsIoffset ->
         (let _lhsOself :: Tm
              _lhsOexp :: (CR.Exp)
              _lhsOoffset :: Int
              _tOlevDiff :: Int
              _tOoffset :: Int
              _tIexp :: (CR.Exp)
              _tIoffset :: Int
              _tIself :: Tm_
              _self =
                  Tm pos_ _tIself
              _lhsOself =
                  _self
              _lhsOexp =
                  ({-# LINE 22 "src/CCO/HM/AG/HMToCR.ag" #-}
                   _tIexp
                   {-# LINE 109 "src/CCO/HM/AG.hs" #-}
                   )
              _lhsOoffset =
                  ({-# LINE 21 "src/CCO/HM/AG/HMToCR.ag" #-}
                   _tIoffset
                   {-# LINE 114 "src/CCO/HM/AG.hs" #-}
                   )
              _tOlevDiff =
                  ({-# LINE 19 "src/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIlevDiff
                   {-# LINE 119 "src/CCO/HM/AG.hs" #-}
                   )
              _tOoffset =
                  ({-# LINE 20 "src/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIoffset
                   {-# LINE 124 "src/CCO/HM/AG.hs" #-}
                   )
              ( _tIexp,_tIoffset,_tIself) =
                  t_ _tOlevDiff _tOoffset
          in  ( _lhsOexp,_lhsOoffset,_lhsOself)))
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
type T_Tm_ = Int ->
             Int ->
             ( (CR.Exp),Int,Tm_)
data Inh_Tm_ = Inh_Tm_ {levDiff_Inh_Tm_ :: Int,offset_Inh_Tm_ :: Int}
data Syn_Tm_ = Syn_Tm_ {exp_Syn_Tm_ :: (CR.Exp),offset_Syn_Tm_ :: Int,self_Syn_Tm_ :: Tm_}
wrap_Tm_ :: T_Tm_ ->
            Inh_Tm_ ->
            Syn_Tm_
wrap_Tm_ sem (Inh_Tm_ _lhsIlevDiff _lhsIoffset) =
    (let ( _lhsOexp,_lhsOoffset,_lhsOself) = sem _lhsIlevDiff _lhsIoffset
     in  (Syn_Tm_ _lhsOexp _lhsOoffset _lhsOself))
sem_Tm__Nat :: Int ->
               T_Tm_
sem_Tm__Nat i_ =
    (\ _lhsIlevDiff
       _lhsIoffset ->
         (let _lhsOself :: Tm_
              _lhsOexp :: (CR.Exp)
              _lhsOoffset :: Int
              _self =
                  Nat i_
              _lhsOself =
                  _self
              _lhsOexp =
                  ({-# LINE 22 "src/CCO/HM/AG/HMToCR.ag" #-}
                   error "missing rule: Tm_.Nat.lhs.exp"
                   {-# LINE 175 "src/CCO/HM/AG.hs" #-}
                   )
              _lhsOoffset =
                  ({-# LINE 21 "src/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIoffset
                   {-# LINE 180 "src/CCO/HM/AG.hs" #-}
                   )
          in  ( _lhsOexp,_lhsOoffset,_lhsOself)))
sem_Tm__Var :: Var ->
               T_Tm_
sem_Tm__Var x_ =
    (\ _lhsIlevDiff
       _lhsIoffset ->
         (let _lhsOself :: Tm_
              _lhsOexp :: (CR.Exp)
              _lhsOoffset :: Int
              _self =
                  Var x_
              _lhsOself =
                  _self
              _lhsOexp =
                  ({-# LINE 22 "src/CCO/HM/AG/HMToCR.ag" #-}
                   error "missing rule: Tm_.Var.lhs.exp"
                   {-# LINE 198 "src/CCO/HM/AG.hs" #-}
                   )
              _lhsOoffset =
                  ({-# LINE 21 "src/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIoffset
                   {-# LINE 203 "src/CCO/HM/AG.hs" #-}
                   )
          in  ( _lhsOexp,_lhsOoffset,_lhsOself)))
sem_Tm__Lam :: Var ->
               T_Tm ->
               T_Tm_
sem_Tm__Lam x_ t1_ =
    (\ _lhsIlevDiff
       _lhsIoffset ->
         (let _lhsOself :: Tm_
              _lhsOexp :: (CR.Exp)
              _lhsOoffset :: Int
              _t1OlevDiff :: Int
              _t1Ooffset :: Int
              _t1Iexp :: (CR.Exp)
              _t1Ioffset :: Int
              _t1Iself :: Tm
              _self =
                  Lam x_ _t1Iself
              _lhsOself =
                  _self
              _lhsOexp =
                  ({-# LINE 22 "src/CCO/HM/AG/HMToCR.ag" #-}
                   _t1Iexp
                   {-# LINE 227 "src/CCO/HM/AG.hs" #-}
                   )
              _lhsOoffset =
                  ({-# LINE 21 "src/CCO/HM/AG/HMToCR.ag" #-}
                   _t1Ioffset
                   {-# LINE 232 "src/CCO/HM/AG.hs" #-}
                   )
              _t1OlevDiff =
                  ({-# LINE 19 "src/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIlevDiff
                   {-# LINE 237 "src/CCO/HM/AG.hs" #-}
                   )
              _t1Ooffset =
                  ({-# LINE 20 "src/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIoffset
                   {-# LINE 242 "src/CCO/HM/AG.hs" #-}
                   )
              ( _t1Iexp,_t1Ioffset,_t1Iself) =
                  t1_ _t1OlevDiff _t1Ooffset
          in  ( _lhsOexp,_lhsOoffset,_lhsOself)))
sem_Tm__App :: T_Tm ->
               T_Tm ->
               T_Tm_
sem_Tm__App t1_ t2_ =
    (\ _lhsIlevDiff
       _lhsIoffset ->
         (let _lhsOself :: Tm_
              _lhsOexp :: (CR.Exp)
              _lhsOoffset :: Int
              _t1OlevDiff :: Int
              _t1Ooffset :: Int
              _t2OlevDiff :: Int
              _t2Ooffset :: Int
              _t1Iexp :: (CR.Exp)
              _t1Ioffset :: Int
              _t1Iself :: Tm
              _t2Iexp :: (CR.Exp)
              _t2Ioffset :: Int
              _t2Iself :: Tm
              _self =
                  App _t1Iself _t2Iself
              _lhsOself =
                  _self
              _lhsOexp =
                  ({-# LINE 22 "src/CCO/HM/AG/HMToCR.ag" #-}
                   _t2Iexp
                   {-# LINE 273 "src/CCO/HM/AG.hs" #-}
                   )
              _lhsOoffset =
                  ({-# LINE 21 "src/CCO/HM/AG/HMToCR.ag" #-}
                   _t2Ioffset
                   {-# LINE 278 "src/CCO/HM/AG.hs" #-}
                   )
              _t1OlevDiff =
                  ({-# LINE 19 "src/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIlevDiff
                   {-# LINE 283 "src/CCO/HM/AG.hs" #-}
                   )
              _t1Ooffset =
                  ({-# LINE 20 "src/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIoffset
                   {-# LINE 288 "src/CCO/HM/AG.hs" #-}
                   )
              _t2OlevDiff =
                  ({-# LINE 19 "src/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIlevDiff
                   {-# LINE 293 "src/CCO/HM/AG.hs" #-}
                   )
              _t2Ooffset =
                  ({-# LINE 20 "src/CCO/HM/AG/HMToCR.ag" #-}
                   _t1Ioffset
                   {-# LINE 298 "src/CCO/HM/AG.hs" #-}
                   )
              ( _t1Iexp,_t1Ioffset,_t1Iself) =
                  t1_ _t1OlevDiff _t1Ooffset
              ( _t2Iexp,_t2Ioffset,_t2Iself) =
                  t2_ _t2OlevDiff _t2Ooffset
          in  ( _lhsOexp,_lhsOoffset,_lhsOself)))
sem_Tm__Let :: Var ->
               T_Tm ->
               T_Tm ->
               T_Tm_
sem_Tm__Let x_ t1_ t2_ =
    (\ _lhsIlevDiff
       _lhsIoffset ->
         (let _lhsOself :: Tm_
              _lhsOexp :: (CR.Exp)
              _lhsOoffset :: Int
              _t1OlevDiff :: Int
              _t1Ooffset :: Int
              _t2OlevDiff :: Int
              _t2Ooffset :: Int
              _t1Iexp :: (CR.Exp)
              _t1Ioffset :: Int
              _t1Iself :: Tm
              _t2Iexp :: (CR.Exp)
              _t2Ioffset :: Int
              _t2Iself :: Tm
              _self =
                  Let x_ _t1Iself _t2Iself
              _lhsOself =
                  _self
              _lhsOexp =
                  ({-# LINE 22 "src/CCO/HM/AG/HMToCR.ag" #-}
                   _t2Iexp
                   {-# LINE 332 "src/CCO/HM/AG.hs" #-}
                   )
              _lhsOoffset =
                  ({-# LINE 21 "src/CCO/HM/AG/HMToCR.ag" #-}
                   _t2Ioffset
                   {-# LINE 337 "src/CCO/HM/AG.hs" #-}
                   )
              _t1OlevDiff =
                  ({-# LINE 19 "src/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIlevDiff
                   {-# LINE 342 "src/CCO/HM/AG.hs" #-}
                   )
              _t1Ooffset =
                  ({-# LINE 20 "src/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIoffset
                   {-# LINE 347 "src/CCO/HM/AG.hs" #-}
                   )
              _t2OlevDiff =
                  ({-# LINE 19 "src/CCO/HM/AG/HMToCR.ag" #-}
                   _lhsIlevDiff
                   {-# LINE 352 "src/CCO/HM/AG.hs" #-}
                   )
              _t2Ooffset =
                  ({-# LINE 20 "src/CCO/HM/AG/HMToCR.ag" #-}
                   _t1Ioffset
                   {-# LINE 357 "src/CCO/HM/AG.hs" #-}
                   )
              ( _t1Iexp,_t1Ioffset,_t1Iself) =
                  t1_ _t1OlevDiff _t1Ooffset
              ( _t2Iexp,_t2Ioffset,_t2Iself) =
                  t2_ _t2OlevDiff _t2Ooffset
          in  ( _lhsOexp,_lhsOoffset,_lhsOself)))