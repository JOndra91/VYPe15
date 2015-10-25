{-# LANGUAGE NoImplicitPrelude #-}

module VYPe15.Types.SymbolTable
where

import Data.Map.Lazy as M (Map)
import Data.Maybe(Maybe)
import Data.String (String)

import VYPe15.Types.AST (DataType, Param)

type VariableTable = M.Map String DataType

type Function = (Maybe DataType, Maybe [Param])
type FunctionTable = M.Map String Function
