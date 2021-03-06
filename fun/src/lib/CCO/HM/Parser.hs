-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.HM.Parser
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- A 'Parser' for a simple, implicitly typed functional language.
--
-------------------------------------------------------------------------------

module CCO.HM.Parser (
    -- * Parser
    parser    -- :: Component String Tm
) where

import CCO.HM.Base --                     (Var, Mod(..), DataTy(..), DataTy_(..), DataCon(..), Tm (Tm), Tm_ (..))
import CCO.HM.Lexer                    (Token, lexer, keyword, var, nat, spec, strLit)
import CCO.Component                   (Component)
import qualified CCO.Component as C    (parser)
import CCO.Parsing
import Control.Applicative

-------------------------------------------------------------------------------
-- Token parsers
-------------------------------------------------------------------------------

-- | Type of 'Parser's that consume symbols described by 'Token's.
type TokenParser = Parser Token

-------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------

-- A 'Component' for parsing terms.
parser :: Component String Mod
parser = C.parser lexer (pMod <* eof)

-- | Parses a 'Mod'.
pMod :: TokenParser Mod
pMod = Mod <$> many pDataTy <*> pTm

pDataTy :: TokenParser DataTy
pDataTy = (\pos name alts -> DataTy pos (DataTy_ name alts))
  <$> sourcePos
  <*  keyword "data"
  <*> var
  <*  spec '='
  <*> someSepBy (spec '|') pDataCon 

pDataCon :: TokenParser DataCon
pDataCon = (\pos name arity -> DataCon pos (DataCon_ name arity))
  <$> sourcePos
  <*> var
  <*> opt (spec '(' *> nat <* spec ')') 0

-- | Parses a 'Tm'.
pTm :: TokenParser Tm
pTm = pLam <|> pPrim <|> pApp
-- pTm = pLam <|> pApp


pLam :: TokenParser Tm
pLam = (\pos x t1 -> Tm pos (Lam x t1)) 
        <$> sourcePos 
        <*  spec '\\' 
        <*> var 
        <*  spec '.' 
        <*> pTm

pApp :: TokenParser Tm
pApp = (\pos ts -> foldl1 (\t1 t2 -> Tm pos (App t1 t2)) ts) 
        <$> sourcePos 
        <*> some
          (  pLet  <|>
             pIf   <|>
             pCase <|>
             pNat  <|>
             pVar  <|>
             pPar
          )

{-
pPrim :: TokenParser Tm
pPrim = (\pos fn -> Tm pos (Prim fn))
  <$> sourcePos
  <*  keyword "prim"
  <*> strLit
-}
pPrim :: TokenParser Tm
pPrim = (\pos fn args -> Tm pos (Prim fn args))
  <$> sourcePos
  <*  keyword "prim"
  <*> strLit
  <*> many
      (  
         pLet <|>
         pIf  <|>
         pNat <|>
         pVar <|>
         pPar
      )
pNat :: TokenParser Tm
pNat = (\pos x -> Tm pos (Nat x)) <$> sourcePos <*> nat

pVar :: TokenParser Tm
pVar = (\pos x -> Tm pos (Var x)) <$> sourcePos <*> var

pLet :: TokenParser Tm
pLet = (\pos x t1 t2 -> Tm pos (Let x t1 t2)) 
        <$> sourcePos 
        <*  keyword "let" 
        <*> var 
        <*  spec '=' 
        <*> pTm 
        <*  keyword "in" 
        <*> pTm 
        <*  keyword "ni"

pPar :: TokenParser Tm
pPar = spec '(' *> pTm <* spec ')'

pIf :: TokenParser Tm
pIf = (\pos cond t1 t2 -> Tm pos (If cond t1 t2))
  <$> sourcePos
  <*  keyword "if"
  <*> pTm
  <*  keyword "then"
  <*> pTm
  <*  keyword "else"
  <*> pTm
  <*  keyword "fi"


pCase :: TokenParser Tm
pCase = (\pos scrut alts -> Tm pos (Case scrut alts))
  <$> sourcePos
  <*  keyword "case"
  <*> pTm
  <*  keyword "of"
  <*> someSepBy (spec ';') pAlt
  <*  keyword "esac"
  
pAlt :: TokenParser Alt
pAlt = Alt <$> var <*> many var <* spec '=' <*> pTm
