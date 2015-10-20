{
module VYPe15.Internal.Lexer
where

import VYPe15.Types.Tokens
}

%wrapper "basic"

$digit            = 0-9
$alpha            = [a-zA-Z]
$ascii            = [\x00-\x7f]
$char             = [\x20\x21\x23-\x26\x28-\x7f]
$new_line         = \n
$any              = [.\n]


@numConst         = $digit+
@charConst        = "'"$char"'"
@stringConst      = "\""$ascii*"\"" -- Needs state based matching.
@onelineComment   = "//".*$new_line
@multilineComment = "/*".*"*/" -- Needs state based matching.
@identifier       = $alpha[_$digit$alpha]*



tokens :-
   $white        ;
   @onelineComment ;
   @multilineComment ;
   "if"          {\s -> TokenIf }
   "else"        {\s -> TokenElse }
   "return"      {\s -> TokenReturn }
   "while"       {\s -> TokenWhile }
   "string"      {\s -> TokenString }
   "char"        {\s -> TokenChar }
   "int"         {\s -> TokenInt }
   "void"        {\s -> TokenVoid }
   ";"           {\s -> TokenSemicolon }
   "{"           {\s -> TokenOCB }
   "}"           {\s -> TokenCCB }
   "="           {\s -> TokenAssign }
   "+"           {\s -> TokenPlus }
   "-"           {\s -> TokenMinus }
   "*"           {\s -> TokenTimes }
   "/"           {\s -> TokenDiv }
   "%"           {\s -> TokenMod }
   "<"           {\s -> TokenLess }
   ">"           {\s -> TokenGreater }
   "<="          {\s -> TokenLEQ }
   ">="          {\s -> TokenGEQ }
   "=="          {\s -> TokenEQ }
   "!="          {\s -> TokenNEQ }
   "&&"          {\s -> TokenAND }
   "||"          {\s -> TokenOR }
   "!"           {\s -> TokenNEG }
   "("           {\s -> TokenOB }
   ")"           {\s -> TokenCB }
   ","           {\s -> TokenComma }
   @numConst     {\s -> TokenNumConst (read s) }
   @charConst    {\s -> TokenCharConst (head s) }
   @stringConst  {\s -> TokenStringConst s }
   @identifier   {\s -> TokenID s }
