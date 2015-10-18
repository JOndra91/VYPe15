module VYPe15.Types.Tokens 
where


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
