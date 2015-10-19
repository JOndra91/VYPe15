{
module VYPe15.Internal.Parser where

import Data.Char

import VYPe15.Types.AST
import VYPe15.Types.Tokens
}

%name      parseVYPe15
%tokentype { Token }
%error     { parseError }

%token 
  if          {TokenIf}
  else        {TokenElse}
  return      {TokenReturn}
  while       {TokenWhile}
  ';'         {TokenSemicolon}
  '{'         {TokenOCB}
  '}'         {TokenCCB}
  numConst    { TokenNumConst $$ }
  charConst   { TokenCharConst $$ }
  stringConst { TokenStringConst $$ }
  identifier  { TokenID $$ }
  '='         { TokenAssign }
  '+'         { TokenPlus }
  '-'         { TokenMinus }
  '*'         { TokenTimes }
  '/'         { TokenDiv }
  '%'         { TokenMod }
  '<'         { TokenLess }
  '>'         { TokenGreater }
  '<='        { TokenLEQ }
  '>='        { TokenGEQ }
  '=='        { TokenEQ }
  '!='        { TokenNEQ }
  '&&'        { TokenAND }
  '||'        { TokenOR } 
  '!'         { TokenNEG }
  '('         { TokenOB }
  ')'         { TokenCB }
  string      { TokenString }
  char        { TokenChar }
  int         { TokenInt }
  void	      { TokenVoid }
  ','         { TokenComma }

%left '||'
%left '&&'
%left '==' '!='
%left '<' '>' '<=' '>='
%left '*' '/' '%'
%left '+' '-'
%left '!'
%left CAST

%%
Program : Program FuncDef           { reverse $ $2:$1 }
        | Program FuncDeclr         { reverse $ $2:$1 }
        | FuncDef                   { [$1] }
        | FuncDeclr                 { [$1] }

FuncDef : Type Identifier '(' Identifier ')' '{' Stats '}'   { FunDef $1 $2 $4 $7 }

FuncDeclr : Type Identifier '(' Identifier ')' ';'           { FunDeclr $1 $2 $4 }


Stats : {- empty -}    { [] }
      | Stats Stat     { $2 : $1 }

Stat  : Identifier '=' Exp ';'                            { Assign $1 $3 }
      | if '(' Exp ')' '{' Stats '}' else '{' Stats '}'   { If $3 (reverse $6) (reverse $10) }
      | return Exp ';'                                    { Return (Just $2) }
      | return ';'                                        { Return Nothing }
      | while '(' Exp ')' '{' Stats '}'                   { While $3 (reverse $6) }
      | DataType Identifier IdList ';'                    { VarDef ($2:$3) }
      | Identifier '(' ExpList ')' ';'                    { FuncCall $1 (reverse $3) }

ExpList : {- empty -}     { [] }
        | ExpList ',' Exp { $3:$1 }
        | Exp             { [$1] }

Exp : Exp '||' Exp                  { OR $1 $3 }
    | Exp '&&' Exp                  { AND $1 $3 }
    | Exp '==' Exp                  { Eq $1 $3 }
    | Exp '!=' Exp                  { NonEq $1 $3 }
    | Exp '<' Exp                   { Less $1 $3}
    | Exp '>' Exp                   { Greater $1 $3}
    | Exp '<=' Exp                  { LessEq $1 $3}
    | Exp '>=' Exp                  { GreaterEq $1 $3}
    | Exp '+' Exp                   { Plus $1 $3 }
    | Exp '-' Exp                   { Minus $1 $3}
    | Exp '*' Exp                   { Times $1 $3}
    | Exp '/' Exp                   { Div $1 $3}
    | Exp '%' Exp                   { Mod $1 $3}
    | '!'Exp                        { NOT $2}
    | '(' DataType ')' Exp %prec CAST   { Cast $2 $4}
    | numConst                      { ConsNum $1}
    | stringConst                   { ConsString $1}
    | charConst                     { ConsChar $1}
    | '(' Exp ')'                   { Bracket $2}
         
DataType : int    { Int }
         | char   { Char }
         | string { String }

Type : void	{ Void }
     | DataType { Type $1 }

IdList : {- empty -}          	       { [] }
       | IdList ',' Identifier 	       { $3 : $1 }

Identifier : identifier { Identifier $1 }

{
parseError :: [Token] -> a
parseError a = error $ "Parse error near : " ++ show a
}
