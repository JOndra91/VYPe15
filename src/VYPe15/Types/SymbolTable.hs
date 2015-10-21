{-# LANGUAGE NoImplicitPrelude #-}

module VYPe15.Types.SymbolTable
where

import Data.Default (Default(def))
import Data.Map.Lazy as M (Map, empty)
import Data.Maybe(Maybe)
import Data.String (String)
import Data.Vector as V (Vector, empty)

import VYPe15.Types.AST (DataType)

-- This should contain also type of the function.
-- Think before you type asshole..
data SymbolState a
    = Defined a
    | Declared a

type VariableTable = M.Map String DataType
type FunctionTable = Map String (SymbolState Function)
-- | (type, arguments)
type Function = (Maybe DataType, Maybe [DataType])

data SymbolTable = SymbolTable
    { identifierTable :: V.Vector VariableTable
    , functionTable :: FunctionTable
    }

instance Default SymbolTable where
    def = SymbolTable V.empty M.empty
