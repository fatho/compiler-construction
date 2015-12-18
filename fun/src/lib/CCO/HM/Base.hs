-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.HM.Base
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- A simple, implicitly typed functional language.
--
-------------------------------------------------------------------------------

module CCO.HM.Base (
    -- * Syntax
    Var                                 -- = String
  , Mod (..)
  , Tm (Tm)                             -- instances: Tree
  , Tm_ (..)                            -- instances: Tree
  , DataTy (..)
  , DataTy_ (..)
  , DataCon (..)
  , DataCon_ (..)
  , Alt (..)
) where

import CCO.HM.AG
import CCO.Tree                   (Tree (fromTree, toTree))
import qualified CCO.Tree as T    (ATerm (App))
import CCO.Tree.Parser            (parseTree, app, arg)
import Control.Applicative        (Applicative ((<*>)), (<$>))

-------------------------------------------------------------------------------
-- Tree instances
-------------------------------------------------------------------------------

instance Tree Mod where
  fromTree (Mod dat tm) = T.App "Mod" [fromTree dat, fromTree tm]
  toTree = parseTree [app "Mod" (Mod <$> arg <*> arg)]

instance Tree DataCon where
  fromTree (DataCon pos t) = T.App "DataCon" [fromTree pos, fromTree t]
  toTree = parseTree [app "DataCon" (DataCon <$> arg <*> arg)]

instance Tree DataTy where
  fromTree (DataTy pos t) = T.App "DataTy" [fromTree pos, fromTree t]
  toTree = parseTree [app "DataTy" (DataTy <$> arg <*> arg)]

instance Tree DataCon_ where
  fromTree (DataCon_ name arity) = T.App "DataCon_" [fromTree name, fromTree arity]
  toTree = parseTree [app "DataCon_" (DataCon_ <$> arg <*> arg)]

instance Tree DataTy_ where
  fromTree (DataTy_ pos dcons) = T.App "DataTy_" [fromTree pos, fromTree dcons]
  toTree = parseTree [app "DataTy_" (DataTy_ <$> arg <*> arg)]

instance Tree Tm where
  fromTree (Tm pos t) = T.App "Tm" [fromTree pos, fromTree t]
  toTree = parseTree [app "Tm" (Tm <$> arg <*> arg)]

instance Tree Alt where
  fromTree (Alt con vars tm) = T.App "Alt" [fromTree con, fromTree vars, fromTree tm]
  toTree = parseTree [app "Alt" (Alt <$> arg <*> arg <*> arg)]

instance Tree Tm_ where
  fromTree (Nat x)       = T.App "Nat" [fromTree x]
  fromTree (Var x)       = T.App "Var" [fromTree x]
  fromTree (Lam x t1)    = T.App "Lam" [fromTree x, fromTree t1]
  fromTree (App t1 t2)   = T.App "App" [fromTree t1, fromTree t2]
  fromTree (Let x t1 t2) = T.App "Let" [fromTree x, fromTree t1, fromTree t2]
  fromTree (Prim fn as)  = T.App "Prim" [fromTree fn, fromTree as]
  --fromTree (Prim fn)     = T.App "Prim" [fromTree fn]
  fromTree (If c t1 t2)  = T.App "If" [fromTree c, fromTree t1, fromTree t2]
  fromTree (Case s a)  = T.App "Case" [fromTree s, fromTree a]

  toTree = parseTree [ app "Nat" (Nat <$> arg                )
                     , app "Var" (Var <$> arg                )
                     , app "Lam" (Lam <$> arg <*> arg        )
                     , app "App" (App <$> arg <*> arg        )
                     , app "Let" (Let <$> arg <*> arg <*> arg)
                     , app "Prim" (Prim <$> arg <*> arg      )
                     --, app "Prim" (Prim <$> arg              )
                     , app "If" (If <$> arg <*> arg <*> arg)
                     , app "Case" (Case <$> arg <*> arg)
                     ]
