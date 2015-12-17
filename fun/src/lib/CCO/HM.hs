-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.HM
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

module CCO.HM (
    -- * Syntax
    Var                                 -- = String
  , Mod (..)
  , Tm (Tm)                             -- instances: Tree
  , Tm_ (..)                            -- instances: Tree
  , DataTy (..)
  , DataTy_ (..)
  , DataCon (..)
  , DataCon_ (..)
    -- * Parser
  , parser                      -- :: Component String Tm
) where

import CCO.HM.Base
import CCO.HM.Parser    (parser)