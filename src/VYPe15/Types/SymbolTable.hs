{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module VYPe15.Types.SymbolTable
  where

import Prelude (Enum, Bounded)

import Data.Eq (Eq)
import Data.Word (Word64)
import Data.Map.Lazy as M (Map, fromList)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Ord (Ord)
import Data.Text (Text)
import Text.Show (Show)

import VYPe15.Types.AST
    (DataType(DChar, DInt, DString), Identifier, Param(AnonymousParam))

newtype Id (a :: IdType) = Id { idWord :: Word64 }
  deriving (Show, Ord, Eq, Enum, Bounded)

data IdType
    = Var
    | Data
    | Label

type VarId = Id 'Var
type DataId = Id 'Data
type LabelId = Id 'Label

data Variable = Variable
    { varId :: VarId
    , varType :: DataType
    }
  deriving (Show)

type VariableTable = M.Map Identifier Variable

data FunctionState
    = FuncDefined
    | FuncDeclared
  deriving (Show)

data Function = Function
    { isDefined :: FunctionState
    , functionReturn :: Maybe DataType
    , functionParams :: Maybe [Param]
    }
  deriving (Show)

type FunctionTable = M.Map Identifier Function

data ProgramData = String Text
  deriving (Show)

type DataTable = M.Map DataId ProgramData

builtInFunctions :: FunctionTable
builtInFunctions = M.fromList
    [ ("print",
        Function
          FuncDefined
          Nothing
          (Just [AnonymousParam DString])
      )
    , ("read_char",
        Function
          FuncDefined
          (Just DChar)
          Nothing
      )
    , ("read_int",
        Function
          FuncDefined
          (Just DInt)
          Nothing
      )
    , ("read_string",
        Function
          FuncDefined
          (Just DString)
          Nothing
      )
    , ("get_at",
        Function
          FuncDefined
          (Just DChar)
          (Just
            [ AnonymousParam DString
            , AnonymousParam DInt
            ]
          )
      )
    , ("set_at",
        Function
          FuncDefined
          (Just DString)
          (Just
            [ AnonymousParam DString
            , AnonymousParam DInt
            , AnonymousParam DChar
            ]
          )
      )
    , ("strcat",
        Function
          FuncDefined
          (Just DString)
          (Just
            [ AnonymousParam DString
            , AnonymousParam DString
            ]
          )
      )
    ]
