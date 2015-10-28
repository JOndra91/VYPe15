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

data Function
    = FunctionDef
        { functionReturn :: Maybe DataType
        , functionParams :: Maybe [Param]
        }
    | FunctionDec
        { functionReturn :: Maybe DataType
        , functionParams :: Maybe [Param]
        }

type FunctionTable = M.Map Identifier Function

builtInFunctions :: FunctionTable
builtInFunctions = M.fromList
    [ ("print",
        FunctionDef
          Nothing
          (Just [AnonymousParam DString])
      )
    , ("read_char",
        FunctionDef
          (Just DChar)
          Nothing
      )
    , ("read_int",
        FunctionDef
          (Just DInt)
          Nothing
      )
    , ("read_string",
        FunctionDef
          (Just DString)
          Nothing
      )
    , ("get_at",
        FunctionDef
          (Just DChar)
          (Just
            [ AnonymousParam DString
            , AnonymousParam DInt
            ]
          )
      )
    , ("set_at",
        FunctionDef
          (Just DString)
          (Just
            [ AnonymousParam DString
            , AnonymousParam DInt
            , AnonymousParam DChar
            ]
          )
      )
    , ("strcat",
        FunctionDef
          (Just DString)
          (Just
            [ AnonymousParam DString
            , AnonymousParam DString
            ]
          )
      )
    ]
