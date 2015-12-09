

-- UUAGC 0.9.52.1 (src/lib/CCO/SystemF/AG.ag)
module CCO.SystemF.AG where

{-# LINE 2 "src/lib/CCO/SystemF/AG/Printing.ag" #-}

import CCO.Printing
{-# LINE 10 "src/lib/CCO/SystemF/AG.hs" #-}

{-# LINE 2 "src/lib/CCO/SystemF/AG/Base.ag" #-}

import CCO.SourcePos
{-# LINE 15 "src/lib/CCO/SystemF/AG.hs" #-}
{-# LINE 53 "src/lib/CCO/SystemF/AG/Printing.ag" #-}

-- | Type of precedence levels.
type Prec = Int
{-# LINE 20 "src/lib/CCO/SystemF/AG.hs" #-}

{-# LINE 75 "src/lib/CCO/SystemF/AG/Printing.ag" #-}

-- | Pretty prints in single-line mode, given the precedence level of its
-- immediate context, a term constructed from a binary operator of a specified
-- precedence level.
-- 
-- A term is enclosed in parentheses if the precedence level of its operator 
-- is less than the precedence level of the enclosing context.

ppInfixSL :: Prec -> (String, Prec) -> Doc -> Doc -> Doc
ppInfixSL ctx (op, prec) l r = modifier $ l >#< ppOp >#< r
  where
    modifier = if prec < ctx then parens else id
    ppOp     = text op

-- | Pretty prints in multiline mode, given the precedence level of its
-- immediate context, a term constructed from a binary operator of a specified
-- precedence level.
-- 
-- A term is enclosed in parentheses if the precedence level of its operator 
-- is less than the precedence level of the enclosing context.

ppInfixML :: Prec -> (String, Prec) -> Doc -> Doc -> Doc
ppInfixML ctx (op, prec) l r = modifier $ l >#< ppOp >-< r
  where
    modifier doc = if prec < ctx then (lparen >#< doc >-< rparen) else doc
    ppOp         = text op
{-# LINE 49 "src/lib/CCO/SystemF/AG.hs" #-}

{-# LINE 107 "src/lib/CCO/SystemF/AG/Printing.ag" #-}

-- | Pretty prints in single-line mode, a construct involving a binder.
ppBinderSL :: String -> Doc -> Doc -> Doc
ppBinderSL binder arg body = text binder >|< arg >|< period >#< body

-- | Pretty prints in multiline mode, a construct involving a binder.
ppBinderML :: String -> Doc -> Doc -> Doc
ppBinderML binder arg body = text binder >|< arg >|< period >-< indent 2 body
{-# LINE 60 "src/lib/CCO/SystemF/AG.hs" #-}

{-# LINE 10 "src/lib/CCO/SystemF/AG/Base.ag" #-}

type TyVar = String    -- ^ Type of type variables. 
type Var   = String    -- ^ Type of variables.
{-# LINE 66 "src/lib/CCO/SystemF/AG.hs" #-}
-- Tm ----------------------------------------------------------
data Tm = Nat (Int)
        | Var (Var)
        | Lam (Var) (Ty) (Tm)
        | App (Tm) (Tm)
        | TyLam (TyVar) (Tm)
        | TyApp (Tm) (Ty)
-- cata
sem_Tm :: Tm ->
          T_Tm
sem_Tm (Nat _x) =
    (sem_Tm_Nat _x)
sem_Tm (Var _x) =
    (sem_Tm_Var _x)
sem_Tm (Lam _x _ty _t1) =
    (sem_Tm_Lam _x (sem_Ty _ty) (sem_Tm _t1))
sem_Tm (App _t1 _t2) =
    (sem_Tm_App (sem_Tm _t1) (sem_Tm _t2))
sem_Tm (TyLam _a _t1) =
    (sem_Tm_TyLam _a (sem_Tm _t1))
sem_Tm (TyApp _t1 _ty) =
    (sem_Tm_TyApp (sem_Tm _t1) (sem_Ty _ty))
-- semantic domain
type T_Tm = Prec ->
            ( Doc,Doc,Tm)
data Inh_Tm = Inh_Tm {prec_Inh_Tm :: Prec}
data Syn_Tm = Syn_Tm {ppML_Syn_Tm :: Doc,ppSL_Syn_Tm :: Doc,self_Syn_Tm :: Tm}
wrap_Tm :: T_Tm ->
           Inh_Tm ->
           Syn_Tm
wrap_Tm sem (Inh_Tm _lhsIprec) =
    (let ( _lhsOppML,_lhsOppSL,_lhsOself) = sem _lhsIprec
     in  (Syn_Tm _lhsOppML _lhsOppSL _lhsOself))
sem_Tm_Nat :: Int ->
              T_Tm
sem_Tm_Nat x_ =
    (\ _lhsIprec ->
         (let _lhsOppML :: Doc
              _lhsOself :: Tm
              _lhsOppSL :: Doc
              _ppSL =
                  ({-# LINE 28 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   text $ show x_
                   {-# LINE 110 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
              _lhsOppML =
                  ({-# LINE 36 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   _ppSL
                   {-# LINE 115 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
              _self =
                  Nat x_
              _lhsOself =
                  _self
              _lhsOppSL =
                  ({-# LINE 11 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   _ppSL
                   {-# LINE 124 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
          in  ( _lhsOppML,_lhsOppSL,_lhsOself)))
sem_Tm_Var :: Var ->
              T_Tm
sem_Tm_Var x_ =
    (\ _lhsIprec ->
         (let _lhsOppML :: Doc
              _lhsOself :: Tm
              _lhsOppSL :: Doc
              _ppSL =
                  ({-# LINE 29 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   text x_
                   {-# LINE 137 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
              _lhsOppML =
                  ({-# LINE 37 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   _ppSL
                   {-# LINE 142 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
              _self =
                  Var x_
              _lhsOself =
                  _self
              _lhsOppSL =
                  ({-# LINE 11 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   _ppSL
                   {-# LINE 151 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
          in  ( _lhsOppML,_lhsOppSL,_lhsOself)))
sem_Tm_Lam :: Var ->
              T_Ty ->
              T_Tm ->
              T_Tm
sem_Tm_Lam x_ ty_ t1_ =
    (\ _lhsIprec ->
         (let _lhsOppML :: Doc
              _tyOprec :: Prec
              _t1Oprec :: Prec
              _lhsOself :: Tm
              _lhsOppSL :: Doc
              _tyIppML :: Doc
              _tyIppSL :: Doc
              _tyIself :: Ty
              _t1IppML :: Doc
              _t1IppSL :: Doc
              _t1Iself :: Tm
              _ppSL =
                  ({-# LINE 30 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   ppBinderSL "\\" (text x_ >#< text ":" >#< _tyIppSL)
                     _t1IppSL
                   {-# LINE 175 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
              _lhsOppML =
                  ({-# LINE 38 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   _ppSL     >^<
                   ppBinderML "\\" (text x_ >#< text ":" >#< _tyIppML)
                     _t1IppML
                   {-# LINE 182 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
              _tyOprec =
                  ({-# LINE 67 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   0
                   {-# LINE 187 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
              _t1Oprec =
                  ({-# LINE 68 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   0
                   {-# LINE 192 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
              _self =
                  Lam x_ _tyIself _t1Iself
              _lhsOself =
                  _self
              _lhsOppSL =
                  ({-# LINE 11 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   _ppSL
                   {-# LINE 201 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
              ( _tyIppML,_tyIppSL,_tyIself) =
                  ty_ _tyOprec
              ( _t1IppML,_t1IppSL,_t1Iself) =
                  t1_ _t1Oprec
          in  ( _lhsOppML,_lhsOppSL,_lhsOself)))
sem_Tm_App :: T_Tm ->
              T_Tm ->
              T_Tm
sem_Tm_App t1_ t2_ =
    (\ _lhsIprec ->
         (let _lhsOppML :: Doc
              _t1Oprec :: Prec
              _t2Oprec :: Prec
              _lhsOself :: Tm
              _lhsOppSL :: Doc
              _t1IppML :: Doc
              _t1IppSL :: Doc
              _t1Iself :: Tm
              _t2IppML :: Doc
              _t2IppSL :: Doc
              _t2Iself :: Tm
              _ppSL =
                  ({-# LINE 32 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   ppInfixSL _lhsIprec ("", 10) _t1IppSL _t2IppSL
                   {-# LINE 227 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
              _lhsOppML =
                  ({-# LINE 41 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   _ppSL     >^<
                   ppInfixML _lhsIprec ("", 10) _t1IppML _t2IppML
                   {-# LINE 233 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
              _t1Oprec =
                  ({-# LINE 69 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   10
                   {-# LINE 238 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
              _t2Oprec =
                  ({-# LINE 70 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   11
                   {-# LINE 243 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
              _self =
                  App _t1Iself _t2Iself
              _lhsOself =
                  _self
              _lhsOppSL =
                  ({-# LINE 11 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   _ppSL
                   {-# LINE 252 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
              ( _t1IppML,_t1IppSL,_t1Iself) =
                  t1_ _t1Oprec
              ( _t2IppML,_t2IppSL,_t2Iself) =
                  t2_ _t2Oprec
          in  ( _lhsOppML,_lhsOppSL,_lhsOself)))
sem_Tm_TyLam :: TyVar ->
                T_Tm ->
                T_Tm
sem_Tm_TyLam a_ t1_ =
    (\ _lhsIprec ->
         (let _lhsOppML :: Doc
              _t1Oprec :: Prec
              _lhsOself :: Tm
              _lhsOppSL :: Doc
              _t1IppML :: Doc
              _t1IppSL :: Doc
              _t1Iself :: Tm
              _ppSL =
                  ({-# LINE 33 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   ppBinderSL "/\\" (text a_) _t1IppSL
                   {-# LINE 274 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
              _lhsOppML =
                  ({-# LINE 43 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   _ppSL     >^<
                   ppBinderML "/\\" (text a_) _t1IppML
                   {-# LINE 280 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
              _t1Oprec =
                  ({-# LINE 71 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   0
                   {-# LINE 285 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
              _self =
                  TyLam a_ _t1Iself
              _lhsOself =
                  _self
              _lhsOppSL =
                  ({-# LINE 11 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   _ppSL
                   {-# LINE 294 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
              ( _t1IppML,_t1IppSL,_t1Iself) =
                  t1_ _t1Oprec
          in  ( _lhsOppML,_lhsOppSL,_lhsOself)))
sem_Tm_TyApp :: T_Tm ->
                T_Ty ->
                T_Tm
sem_Tm_TyApp t1_ ty_ =
    (\ _lhsIprec ->
         (let _lhsOppML :: Doc
              _t1Oprec :: Prec
              _tyOprec :: Prec
              _lhsOself :: Tm
              _lhsOppSL :: Doc
              _t1IppML :: Doc
              _t1IppSL :: Doc
              _t1Iself :: Tm
              _tyIppML :: Doc
              _tyIppSL :: Doc
              _tyIself :: Ty
              _ppSL =
                  ({-# LINE 34 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   ppInfixSL _lhsIprec ("", 10) _t1IppSL (brackets _tyIppSL)
                   {-# LINE 318 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
              _lhsOppML =
                  ({-# LINE 45 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   _ppSL     >^<
                   ppInfixML _lhsIprec ("", 10) _t1IppML
                     (lbracket >#< _tyIppML >-< rbracket)
                   {-# LINE 325 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
              _t1Oprec =
                  ({-# LINE 72 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   9
                   {-# LINE 330 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
              _tyOprec =
                  ({-# LINE 73 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   0
                   {-# LINE 335 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
              _self =
                  TyApp _t1Iself _tyIself
              _lhsOself =
                  _self
              _lhsOppSL =
                  ({-# LINE 11 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   _ppSL
                   {-# LINE 344 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
              ( _t1IppML,_t1IppSL,_t1Iself) =
                  t1_ _t1Oprec
              ( _tyIppML,_tyIppSL,_tyIself) =
                  ty_ _tyOprec
          in  ( _lhsOppML,_lhsOppSL,_lhsOself)))
-- Ty ----------------------------------------------------------
data Ty = TyNat
        | TyVar (TyVar)
        | Arr (Ty) (Ty)
        | Forall (TyVar) (Ty)
-- cata
sem_Ty :: Ty ->
          T_Ty
sem_Ty (TyNat) =
    (sem_Ty_TyNat)
sem_Ty (TyVar _a) =
    (sem_Ty_TyVar _a)
sem_Ty (Arr _ty1 _ty2) =
    (sem_Ty_Arr (sem_Ty _ty1) (sem_Ty _ty2))
sem_Ty (Forall _a _ty1) =
    (sem_Ty_Forall _a (sem_Ty _ty1))
-- semantic domain
type T_Ty = Prec ->
            ( Doc,Doc,Ty)
data Inh_Ty = Inh_Ty {prec_Inh_Ty :: Prec}
data Syn_Ty = Syn_Ty {ppML_Syn_Ty :: Doc,ppSL_Syn_Ty :: Doc,self_Syn_Ty :: Ty}
wrap_Ty :: T_Ty ->
           Inh_Ty ->
           Syn_Ty
wrap_Ty sem (Inh_Ty _lhsIprec) =
    (let ( _lhsOppML,_lhsOppSL,_lhsOself) = sem _lhsIprec
     in  (Syn_Ty _lhsOppML _lhsOppSL _lhsOself))
sem_Ty_TyNat :: T_Ty
sem_Ty_TyNat =
    (\ _lhsIprec ->
         (let _lhsOppML :: Doc
              _lhsOself :: Ty
              _lhsOppSL :: Doc
              _ppSL =
                  ({-# LINE 15 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   text "Nat"
                   {-# LINE 387 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
              _lhsOppML =
                  ({-# LINE 20 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   _ppSL
                   {-# LINE 392 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
              _self =
                  TyNat
              _lhsOself =
                  _self
              _lhsOppSL =
                  ({-# LINE 11 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   _ppSL
                   {-# LINE 401 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
          in  ( _lhsOppML,_lhsOppSL,_lhsOself)))
sem_Ty_TyVar :: TyVar ->
                T_Ty
sem_Ty_TyVar a_ =
    (\ _lhsIprec ->
         (let _lhsOppML :: Doc
              _lhsOself :: Ty
              _lhsOppSL :: Doc
              _ppSL =
                  ({-# LINE 16 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   text a_
                   {-# LINE 414 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
              _lhsOppML =
                  ({-# LINE 21 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   _ppSL
                   {-# LINE 419 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
              _self =
                  TyVar a_
              _lhsOself =
                  _self
              _lhsOppSL =
                  ({-# LINE 11 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   _ppSL
                   {-# LINE 428 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
          in  ( _lhsOppML,_lhsOppSL,_lhsOself)))
sem_Ty_Arr :: T_Ty ->
              T_Ty ->
              T_Ty
sem_Ty_Arr ty1_ ty2_ =
    (\ _lhsIprec ->
         (let _lhsOppML :: Doc
              _ty1Oprec :: Prec
              _ty2Oprec :: Prec
              _lhsOself :: Ty
              _lhsOppSL :: Doc
              _ty1IppML :: Doc
              _ty1IppSL :: Doc
              _ty1Iself :: Ty
              _ty2IppML :: Doc
              _ty2IppSL :: Doc
              _ty2Iself :: Ty
              _ppSL =
                  ({-# LINE 17 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   ppInfixSL _lhsIprec ("->", 0) _ty1IppSL _ty2IppSL
                   {-# LINE 450 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
              _lhsOppML =
                  ({-# LINE 22 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   _ppSL     >^<
                   ppInfixML _lhsIprec ("->", 0) _ty1IppML _ty2IppML
                   {-# LINE 456 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
              _ty1Oprec =
                  ({-# LINE 62 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   1
                   {-# LINE 461 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
              _ty2Oprec =
                  ({-# LINE 63 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   0
                   {-# LINE 466 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
              _self =
                  Arr _ty1Iself _ty2Iself
              _lhsOself =
                  _self
              _lhsOppSL =
                  ({-# LINE 11 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   _ppSL
                   {-# LINE 475 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
              ( _ty1IppML,_ty1IppSL,_ty1Iself) =
                  ty1_ _ty1Oprec
              ( _ty2IppML,_ty2IppSL,_ty2Iself) =
                  ty2_ _ty2Oprec
          in  ( _lhsOppML,_lhsOppSL,_lhsOself)))
sem_Ty_Forall :: TyVar ->
                 T_Ty ->
                 T_Ty
sem_Ty_Forall a_ ty1_ =
    (\ _lhsIprec ->
         (let _lhsOppML :: Doc
              _ty1Oprec :: Prec
              _lhsOself :: Ty
              _lhsOppSL :: Doc
              _ty1IppML :: Doc
              _ty1IppSL :: Doc
              _ty1Iself :: Ty
              _ppSL =
                  ({-# LINE 18 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   ppBinderSL "forall " (text a_) _ty1IppSL
                   {-# LINE 497 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
              _lhsOppML =
                  ({-# LINE 24 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   _ppSL     >^<
                   ppBinderML "forall " (text a_) _ty1IppML
                   {-# LINE 503 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
              _ty1Oprec =
                  ({-# LINE 64 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   0
                   {-# LINE 508 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
              _self =
                  Forall a_ _ty1Iself
              _lhsOself =
                  _self
              _lhsOppSL =
                  ({-# LINE 11 "src/lib/CCO/SystemF/AG/Printing.ag" #-}
                   _ppSL
                   {-# LINE 517 "src/lib/CCO/SystemF/AG.hs" #-}
                   )
              ( _ty1IppML,_ty1IppSL,_ty1Iself) =
                  ty1_ _ty1Oprec
          in  ( _lhsOppML,_lhsOppSL,_lhsOself)))