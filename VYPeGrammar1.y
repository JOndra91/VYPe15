{
module Main where

import Data.Char
}

%name      calc
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

type Program
  = [ FunDeclrOrDef ]

data FunDeclrOrDef
  = FunDeclr Type Identifier Identifier
  | FunDef Type Identifier Identifier [Stat]
  deriving (Show)

data Identifier
  = Identifier String
  deriving (Show)

data Stat 
  = Assign Identifier Exp
  | If Exp [Stat] [Stat]
  | Return (Maybe Exp)
  | While Exp [Stat]
  | VarDef [Identifier]
  | FuncCall Identifier [Exp]
  deriving Show

data Exp
  = OR Exp Exp
  | AND Exp Exp
  | Eq Exp Exp
  | NonEq Exp Exp
  | Less Exp Exp
  | Greater Exp Exp
  | LessEq Exp Exp
  | GreaterEq Exp Exp
  | Plus Exp Exp
  | Minus Exp Exp
  | Times Exp Exp
  | Div Exp Exp
  | Mod Exp Exp
  | NOT Exp
  | Cast DataType Exp
  | ConsNum Int
  | ConsString String
  | ConsChar Char
  | Bracket Exp
  deriving Show

data DataType 
  = Int
  | Char
  | String
  deriving Show

data Type
  = Type DataType
  | Void
  deriving (Show)

data Token 
  = TokenNumConst Int
  | TokenCharConst Char
  | TokenStringConst String
  | TokenID String
  | TokenAssign 
  | TokenPlus 
  | TokenMinus 
  | TokenTimes 
  | TokenDiv 
  | TokenMod 
  | TokenLess 
  | TokenGreater
  | TokenLEQ 
  | TokenGEQ 
  | TokenEQ 
  | TokenNEQ 
  | TokenAND 
  | TokenOR  
  | TokenNEG 
  | TokenOB 
  | TokenCB 
  | TokenOCB
  | TokenCCB
  | TokenIf
  | TokenElse
  | TokenReturn
  | TokenWhile
  | TokenSemicolon
  | TokenString 
  | TokenChar 
  | TokenInt
  | TokenVoid
  | TokenComma
  deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
      | isDigit c = lexNum (c:cs)
      | ('"' == c) = lexStrConst cs
      | ('\'' == c) = lexCharConst cs
lexer ('=':cs) = TokenAssign : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('/':cs) = TokenDiv : lexer cs
lexer ('%':cs) = TokenMod : lexer cs
lexer ('<':'=':cs) = TokenLEQ : lexer cs
lexer ('>':'=':cs) = TokenGEQ : lexer cs
lexer ('=':'=':cs) = TokenEQ : lexer cs
lexer ('!':'=':cs) = TokenNEQ : lexer cs
lexer ('|':'|':cs) = TokenOR : lexer cs
lexer ('&':'&':cs) = TokenAND : lexer cs
lexer ('<':cs) = TokenLess : lexer cs
lexer ('>':cs) = TokenGreater: lexer cs
lexer ('!':cs) = TokenNEG : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs
lexer ('{':cs) = TokenOCB : lexer cs
lexer ('}':cs) = TokenCCB : lexer cs
lexer (';':cs) = TokenSemicolon : lexer cs
lexer (',':cs) = TokenComma : lexer cs

lexCharConst cs = TokenCharConst char : lexer rest
  where (char, rest) = (head cs, drop 2 cs)

lexStrConst cs = TokenStringConst str : lexer (drop 1 rest)
  where (str,rest) = span (/= '"') cs

lexNum cs = TokenNumConst (read num) : lexer rest
  where (num,rest) = span isDigit cs

lexVar cs = case var of
    "string" -> TokenString
    "char"   -> TokenChar
    "int"    -> TokenInt
    "while"  -> TokenWhile
    "return" -> TokenReturn
    "if"     -> TokenIf
    "else"   -> TokenElse
    "void"   -> TokenVoid
    _        -> TokenID var
    : lexer rest
 where (var, rest) = span isAlpha cs

parseError :: [Token] -> a
parseError a = error $ "Parse error near : " ++ show a

main = getContents >>= print . calc . lexer
}
