{
module Parser where
import Lexer
import AttributeGrammar
}

%name happy
%tokentype { Token }
%error { parseError }

%token 
      
      ident            { TIdent $$ }
      int              { TInt $$ }
      bool             { TBool $$ }
      if               { TIf }
      then             { TThen }
      else             { TElse }
      while            { TWhile }
      do               { TDo }
      skip             { TSkip }
      not              { TNot }
      ":="             { TAssign }
      "+"              { TArithmeticOp "+" }
      "-"              { TArithmeticOp "-" }
      "*"              { TStar }
      "/"              { TArithmeticOp "/" }
      and              { TBoolOp "and" }
      or               { TBoolOp "or" }
      "=="             { TRelOp "==" }
      "<"              { TRelOp "<" }
      ">"              { TRelOp ">" }
      "<="             { TRelOp "<=" }
      ">="             { TRelOp ">=" }
      ";"              { TSemicolon }
      "["              { TBlockOpen  }
      "]"              { TBlockClose }
      "("              { TParenOpen  }
      ")"              { TParenClose }
      "{"              { TBraceOpen  }
      "}"              { TBraceClose }
      begin            { TBegin }
      end              { TEnd }
      proc             { TProc }
      is               { TIs }
      val              { TVal }
      res              { TRes }
      call             { TCall }
      ","              { TComma }
      malloc           { TMalloc }
      free             { TFree }
      continue         { TContinue }
      break            { TBreak }
      tyint            { TTyInt }


%left or
%left and
%nonassoc "==" "<" ">" "<=" ">="
%left "+" "-"
%left "*" "/"
%left not

%%

Start : Program { $1 }

Program : begin Procs Stats end  { Program $2 $3 }

Procs : Procs Proc   { $1 ++ [ $2 ] }
      | {- empty -}  { [ ] }
Proc  : proc ident "(" ValArgs ResArg ")" is Stats end  { Proc $2 $4 $5 $8 }

ValArgs : val Args ","  { $2 }
        | {- empty -}   { [] }
ResArg : res Arg        { $2 }

Args : Args "," Arg  { $1 ++ [ $3 ] }
     | Arg           { [ $1 ] }
Arg : ident          { $1 }

Stats : Stats Stat      { Seq $1 $2 }
      | Stat            { $1 }
Stat  : if BExpr then Stat0 else Stat0 { IfThenElse $2 $4 $6 }
      | if BExpr then Stat0            { IfThenElse $2 $4 Skip}
      | while BExpr do Stat0           { While $2 $4 }
      | Stat0                          { $1 }
Stat0 : skip ";"                         { Skip }
      | ident ":=" AExpr ";"             { IAssign $1 $3 }
      | ident ":=" BExpr ";"             { BAssign $1 $3 }
      | "*" AExpr0 ":=" AExpr0 ";"       { RefAssign $2 $4 }
      | call ident "(" CallArgs "," ident ")" ";"  { Call $2 $4 $6 }
      | malloc "(" ident "," AExpr ")" ";"      { Malloc $3 $5 }
      | free "(" AExpr0 ")" ";"          { Free $3 }
      | "(" Stats ")"                    { $2 }
      | "{" Stats "}"                    { $2 }
      | continue ";"                     { Continue }
      | break ";"                        { Break }
      | tyint ident "[" AExpr "]" ";"      { Malloc $2 $4 }
      | ident "[" AExpr "]" ":=" AExpr0 ";"          { RefAssign (Plus (Var $1) $3) $6 }

CallArgs : CallArgs "," EitherExpr   { $1 ++ [ $3 ] }
         | EitherExpr                { [ $1 ] }
         | {- empty -}               { [] }
EitherExpr : AExpr { I $1 }
           | BExpr { B $1 }

AExpr  : AExpr "+" AExpr   { Plus $1 $3 }
       | AExpr "-" AExpr   { Minus $1 $3 }
       | AExpr "*" AExpr   { Times $1 $3 }
       | AExpr "/" AExpr   { Divide $1 $3 }
       | AExpr0            { $1 }
AExpr0 : int               { IConst $1 }
       | ident             { Var $1 }
       | "(" AExpr ")"     { $2 }
       | "[" AExpr "]"     { $2 }
       | "*" AExpr0        { Deref $2 }
       | ident "[" AExpr "]" { Deref (Plus (Var $1) $3) }

BExpr  : not BExpr         { Not $2 }
       | BExpr and BExpr   { And $1 $3 }
       | BExpr or  BExpr   { Or $1 $3 }
       | BExpr "==" BExpr  { BEqual $1 $3 }
       | AExpr "==" AExpr  { IEqual $1 $3 }
       | AExpr "<"  AExpr  { LessThan $1 $3 }
       | AExpr ">"  AExpr  { GreaterThan $1 $3 }
       | AExpr ">=" AExpr  { GreaterEqual $1 $3 }
       | AExpr "<=" AExpr  { LessEqual $1 $3 }
       | BExpr0            { $1 }
BExpr0 : bool              { BConst $1 }
       | "(" BExpr ")"     { $2 }


{
parseError :: [Token] -> a
parseError (x:xs) = error ("Parse error: " ++ show xs)
}

