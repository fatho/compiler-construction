-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.Core.Base
-- Copyright   :  (c) 2014 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  atze@uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- Simple wrapper for/around CoreRun from uhc-light
--
-------------------------------------------------------------------------------

module CCO.Core.Base (
    -- * Syntax
    Ref (Glob, Loc)                     
  , RefL
  , SExp (Var, Int)                     
  , SExpL
  , Exp (SExp, Lam, App, Let, Prim, Node, Case, Dbg)
  , ExpL
  , Bind (Bind)                         
  , BindL
  , Mod (Mod)                           
) where

import CCO.Core.AG
import CCO.Tree                   (Tree (fromTree, toTree))
import qualified CCO.Tree as T    (ATerm (App))
import CCO.Tree.Parser            (parseTree, app, arg)
import Control.Applicative        (Applicative ((<*>)), (<$>))

-------------------------------------------------------------------------------
-- Tree instances
-------------------------------------------------------------------------------

instance Tree Mod where
  fromTree (Mod mn bs) = T.App "Mod" [fromTree mn, fromTree bs]
  toTree = parseTree [app "Mod" (Mod <$> arg <*> arg)]

instance Tree Bind where
  fromTree (Bind x e) = T.App "Bind" [fromTree x, fromTree e]
  toTree = parseTree [app "Bind" (Bind <$> arg <*> arg)]

instance Tree SExp where
  fromTree (Int i)   = T.App "Int" [fromTree i]
  fromTree (Var r)   = T.App "Var" [fromTree r]

  toTree = parseTree [ app "Int" (Int <$> arg)
                     , app "Var" (Var <$> arg)
                     ]

instance Tree Ref where
  fromTree (Glob  o)  = T.App "Glob" [fromTree o]
  fromTree (Loc d o)  = T.App "Loc" [fromTree d, fromTree o]

  toTree = parseTree [ app "Glob" (Glob <$> arg)
                     , app "Loc"  (Loc  <$> arg <*> arg)
                     ]

instance Tree Exp where
  fromTree (SExp se   )   = T.App "SExp" [fromTree se]
  fromTree (Lam  as bd)   = T.App "Lam"  [fromTree as, fromTree bd]
  fromTree (App  fn as)   = T.App "App"  [fromTree fn, fromTree as]
  fromTree (Prim fn as)   = T.App "Prim" [fromTree fn, fromTree as]
  fromTree (Node tg as)   = T.App "Node" [fromTree tg, fromTree as]
  fromTree (Case se as)   = T.App "Case" [fromTree se, fromTree as]
  fromTree (Let  bn bd)   = T.App "Let"  [fromTree bn, fromTree bd]
  fromTree (Dbg  i    )   = T.App "Dbg"  [fromTree i]

  toTree = parseTree [ app "SExp" (SExp <$> arg        )
                     , app "Lam"  (Lam  <$> arg <*> arg)
                     , app "App"  (App  <$> arg <*> arg)
                     , app "Prim" (Prim <$> arg <*> arg)
                     , app "Node" (Node <$> arg <*> arg)
                     , app "Case" (Case <$> arg <*> arg)
                     , app "Let"  (Let  <$> arg <*> arg)
                     , app "Dbg"  (Dbg  <$> arg        )
                     ]

