{
module Main where
}

%name      calc
%tokentype { Token }
%error     { parseError }

%token 
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

%%

Stat  : identifier '=' OR_Expr { Assign $1 $3 }

OR_Expr    : OR_Expr '||' AND_Expr { OR $1 $3 } 
           | AND_Expr              { AND_Expr $1 }

AND_Expr : EQ_Expr { EQ_Expr $1 }
         | AND_Expr '&&' EQ_Expr { AND $1 $3 }

EQ_Expr : Rel_Expr { Rel_Expr $1 }
        | EQ_Expr '==' Rel_Expr { Equals $1 $3 }
        | EQ_Expr '!=' Rel_Expr { NEquals $1 $3 }

Rel_Expr : ADD_Expr { ADD_Expr $1 }
         | Rel_Expr '>' ADD_Expr { Greater $1 $3 }
         | Rel_Expr '<' ADD_Expr { Less $1 $3 }
         | Rel_Expr '>=' ADD_Expr { GreaterEq $1 $3 }
         | Rel_Expr '<=' ADD_Expr { LessEq $1 $3 }
         
ADD_Expr : Mul_Expr { Mul_Expr $1 }
         | ADD_Expr '+' Mul_Expr { Plus $1 $3 }
         | ADD_Expr '-' Mul_Expr { Minus $1 $3 }

Mul_Expr : Unary_Expr { Unary_Expr $1 }
         | Mul_Expr '*' Unary_Expr { Times $1 $3 }
         | Mul_Expr '/' Unary_Expr { Div $1 $3 }
         | Mul_Expr '%' Unary_Expr { Mod $1 $3 }

Unary_Expr : Cast_Expr  { Cast_Expr $1 }
           | '!'Cast_Expr { Neg $2 }

Cast_Expr  : Factor { Factor $1 }
           | '(' Type ')' Factor { Cast $2 $4 }

Factor : numConst { NumConst $1 }
       | stringConst { StrConst $1 }
       | charConst { CharConst $1 }
       | '(' OR_Expr ')' { Bracket $2 }

Type : int    { Int }
     | char   { Char }
     | string { String }

{
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
  | TokenString 
  | TokenChar 
  | TokenInt
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
    _        -> TokenID var
    : lexer rest
 where (var, rest) = span isAlpha cs

parseError :: [Token] -> a
parseError a = error $ "Parse error near : " ++ show a

data Type 
  = Int
  | Char
  | String
  deriving Show

data Id 
  = Identifier String
  deriving Show

data Stat 
  = Assign String OR_Expr
  deriving Show

data OR_Expr
  = OR OR_Expr AND_Expr
  | AND_Expr AND_Expr
  deriving Show

data AND_Expr
  = AND AND_Expr EQ_Expr
  | EQ_Expr EQ_Expr
  deriving Show

data EQ_Expr
  = Equals EQ_Expr Rel_Expr
  | NEquals EQ_Expr Rel_Expr
  | Rel_Expr Rel_Expr
  deriving Show

data Rel_Expr
 = Greater Rel_Expr ADD_Expr
 | Less Rel_Expr ADD_Expr
 | GreaterEq Rel_Expr ADD_Expr
 | LessEq Rel_Expr ADD_Expr
 | ADD_Expr ADD_Expr
 deriving Show

data ADD_Expr 
 = Plus ADD_Expr Mul_Expr
 | Minus ADD_Expr Mul_Expr
 | Mul_Expr Mul_Expr
 deriving Show

data Mul_Expr
  = Times Mul_Expr Unary_Expr
  | Div Mul_Expr Unary_Expr
  | Mod Mul_Expr Unary_Expr 
  | Unary_Expr Unary_Expr
 deriving Show

data Unary_Expr
 = Neg Cast_Expr
 | Cast_Expr Cast_Expr
 deriving Show

data Cast_Expr
  = Cast Type Factor
  | Factor Factor
 deriving Show

data Factor 
  = NumConst Int
  | StrConst String
  | CharConst Char
  | Bracket OR_Expr
  deriving Show

main = getContents >>= print . calc . lexer
}
