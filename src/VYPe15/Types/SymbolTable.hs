{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module VYPe15.Types.SymbolTable
where

import Data.Map.Lazy as M (Map, fromList)
import Data.Maybe (Maybe(Just, Nothing))

import VYPe15.Types.AST
    ( DataType(DChar, DInt, DString)
    , Param(AnonymousParam)
    , Identifier
    )

type VariableTable = M.Map Identifier DataType

data FunctionState
    = FuncDefined
    | FuncDeclared

data Function = Function
    { isDefined :: FunctionState
    , functionReturn :: Maybe DataType
    , functionParams :: Maybe [Param]
    }

type FunctionTable = M.Map Identifier Function

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
