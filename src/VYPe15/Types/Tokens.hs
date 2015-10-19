{-# LANGUAGE NoImplicitPrelude #-}

module VYPe15.Types.Tokens 
where

import Data.Char (Char)
import Data.Int (Int)
import Data.String (String)
import Text.Show (Show)

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
