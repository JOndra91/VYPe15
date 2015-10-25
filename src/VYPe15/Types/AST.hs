{-# LANGUAGE NoImplicitPrelude #-}

module VYPe15.Types.AST
where

import Data.Maybe (Maybe)
import Text.Show (Show)
import Data.Char (Char)
import Data.String (String)

import Prelude (Integer)

type Program
  = [FunDeclrOrDef]

data FunDeclrOrDef
  = FunDeclr (Maybe DataType) Identifier (Maybe [DataType])
  | FunDef (Maybe DataType) Identifier (Maybe [Param]) [Stat]
  deriving (Show)

data Identifier
  = Identifier String
  deriving (Show)

data Stat 
  = Assign Identifier Exp
  | If Exp [Stat] [Stat]
  | Return (Maybe Exp)
  | While Exp [Stat]
  | VarDef DataType [Identifier]
  | FuncCall Identifier [Exp]
  deriving (Show)

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
  | ConsNum Integer
  | ConsString String
  | ConsChar Char
  | Bracket Exp
  | FuncCallExp Identifier [Exp]
  | IdentifierExp String
  deriving (Show)

data DataType 
  = DInt
  | DChar
  | DString
  deriving (Show)

data Param 
    = Param DataType Identifier
    deriving (Show)
