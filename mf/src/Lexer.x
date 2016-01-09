{
module Lexer where
}

%wrapper "basic"

$digit = 0-9      -- digits
$alpha = [a-zA-Z]   -- alphabetic characters


tokens :-

  $white+                          ;
  "--".*                           ;

  true                             { \s -> TBool True }
  false                            { \s -> TBool False }

  if                               { \s -> TIf }
  then                             { \s -> TThen }
  else                             { \s -> TElse }
  while                            { \s -> TWhile }
  do                               { \s -> TDo }
  skip                             { \s -> TSkip }
  not                              { \s -> TNot }
  continue                         { \s -> TContinue}
  break                            { \s -> TBreak}
  
  \:\=                             { \s -> TAssign }
  [\+\-\/]                         { \s -> TArithmeticOp s }
  \*                               { \s -> TStar }
  and                              { \s -> TBoolOp s }
  or                               { \s -> TBoolOp s }
  (\<|\>|\<\=|\>\=|\=\=)           { \s -> TRelOp s }
  
  \;                               { \s -> TSemicolon }
  \[                               { \s -> TBlockOpen }
  \]                               { \s -> TBlockClose }
  \(                               { \s -> TParenOpen }
  \)                               { \s -> TParenClose }
  \{                               { \s -> TBraceOpen }
  \}                               { \s -> TBraceClose }

  begin                            { \s -> TBegin }
  end                              { \s -> TEnd }
  proc                             { \s -> TProc }
  is                               { \s -> TIs }
  val                              { \s -> TVal }
  res                              { \s -> TRes }
  call                             { \s -> TCall }
  \,                               { \s -> TComma }

  malloc                           { \s -> TMalloc }
  free                             { \s -> TFree }

  int                              { \s -> TTyInt }

  $alpha [$alpha $digit \_ \']*    { \s -> TIdent s }
  $digit+                          { \s -> TInt (read s) }

{
-- Each action has type :: String -> Token

-- The token type:
data Token  = TIdent String
            | TInt Int
            | TBool Bool
            | TIf
            | TThen
            | TElse
            | TWhile
            | TDo
            | TSkip
            | TNot
            | TAssign
            | TArithmeticOp String
            | TStar
            | TBoolOp String
            | TRelOp String
            | TSemicolon
            | TBlockOpen
            | TBlockClose
            | TParenOpen
            | TParenClose
            | TBraceOpen
            | TBraceClose
            | TBegin
            | TEnd
            | TProc
            | TIs
            | TVal
            | TRes
            | TCall
            | TComma
            | TMalloc
            | TFree
            | TContinue
            | TBreak
            | TTyInt
            deriving (Eq,Show)

alex :: String -> [Token]
alex = alexScanTokens
}