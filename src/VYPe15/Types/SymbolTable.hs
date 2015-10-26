{-# LANGUAGE NoImplicitPrelude #-}

module VYPe15.Types.SymbolTable
where

import Data.Map.Lazy as M (Map, fromList)
import Data.Maybe(Maybe(Just, Nothing))
import Data.String (String)

import VYPe15.Types.AST (DataType(DInt, DString, DChar), Identifier(Identifier), Param(Param))

type VariableTable = M.Map String DataType

type Function = (Maybe DataType, Maybe [Param])
type FunctionTable = M.Map String Function

builtInFunctions :: FunctionTable
builtInFunctions = M.fromList 
    [ ("print", 
        ( Nothing
        , Just 
            [ Param DString (Identifier "")
            ]
        )
      )
    , ("read_char", 
        ( Just DChar
        , Nothing
        )
      )
    , ("read_int", 
        ( Just DInt
        , Nothing
        )
      )
    , ("read_string", 
        ( Just DString
        , Nothing
        )
      )
    , ("get_at", 
        ( Just DChar
        , Just 
            [ Param DString (Identifier "")
            , Param DInt (Identifier "")
            ]
        )
      )
    , ("set_at", 
        ( Just DString
        , Just 
            [ Param DString (Identifier "")
            , Param DInt  (Identifier "")
            , Param DChar (Identifier "")
            ]
        )
      )
    , ("strcat", 
        ( Just DString
        , Just 
            [ Param DString (Identifier "")
            , Param DString (Identifier "")
            ]
        )
      )
    ]
