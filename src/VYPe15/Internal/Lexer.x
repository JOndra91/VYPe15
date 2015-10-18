{
Module VYPe15.Internal.Lexer

import VYPe15.Types.Tokens
}

%wrapper "basic"

$digit            = 0-9
$alpha            = [a-zA-Z]
$onelineComment   = "//".*
$multilineComment = "/*".*"*/"

toknes :-
   $white        ;
   $onelineComment ; 
   $multilineComment ;
   if            { TokenIf}
   else          { TokenElse}
   return        { TokenReturn}
   while         { TokenWhile}
   ';'           { TokenSemicolon}
   '{'           { TokenOCB}
   '}'           { TokenCCB}
   numConst      { TokenNumConst $$ }
   charConst     { TokenCharConst $$ }
   stringConst   { TokenStringConst $$ }
   identifier    { TokenID $$ }
   '='           { TokenAssign }
   '+'           { TokenPlus }
   '-'           { TokenMinus }
   '*'           { TokenTimes }
   '/'           { TokenDiv }
   '%'           { TokenMod }
   '<'           { TokenLess }
   '>'           { TokenGreater }
   '<='          { TokenLEQ }
   '>='          { TokenGEQ }
   '=='          { TokenEQ }
   '!='          { TokenNEQ }
   '&&'          { TokenAND }
   '||'          { TokenOR } 
   '!'           { TokenNEG }
   '('           { TokenOB }
   ')'           { TokenCB }
   string        { TokenString }
   char          { TokenChar }
   int           { TokenInt }
   void	         { TokenVoid }
   ','           { TokenComma }
