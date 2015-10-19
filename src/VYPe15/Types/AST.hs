module VYPe15.Types.AST
where

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
