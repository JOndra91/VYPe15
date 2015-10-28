{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}

module VYPe15.Types.Semantics
where

import Control.Applicative (Applicative, (<$>))
import Control.Monad (Monad, (>>=))
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (MonadState, State, evalState, get, modify)
import Control.Monad.Writer (MonadWriter, WriterT, runWriterT)
import Data.Either (Either(Left, Right))
import Data.Function (($), (.))
import Data.Functor (Functor)
import Data.String (String)
import Text.Show (Show)
import Data.Maybe (Maybe)

import VYPe15.Types.SymbolTable (FunctionTable, VariableTable)
import VYPe15.Types.AST (DataType)

newtype SError
    = SError String
  deriving (Show)

data AnalyzerState = AnalyzerState
    { functionTable :: FunctionTable
    , variableTables :: [VariableTable]
    , returnType :: Maybe DataType
    -- ^ Actual return type of function is needed during it's processing so it's
    -- possible to check when type in return statements matches.
    }

newtype SemanticAnalyzer a
    = SemanticAnalyzer { runSemAnalyzer ::
        ExceptT SError (WriterT  [String] (State AnalyzerState)) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError SError
    , MonadState AnalyzerState
    , MonadWriter [String]
    )

evalSemAnalyzer
  :: AnalyzerState
  -> SemanticAnalyzer a
  -> Either SError [String]
evalSemAnalyzer s m =
    case (`evalState` s) . runWriterT . runExceptT $ runSemAnalyzer m of
        (Left e, _) -> Left e
        (Right _, w) -> Right w

getVars :: SemanticAnalyzer [VariableTable]
getVars = variableTables <$> get

getFunc :: SemanticAnalyzer FunctionTable
getFunc = functionTable <$> get

getReturnType :: SemanticAnalyzer (Maybe DataType)
getReturnType = returnType <$> get

putVars :: [VariableTable] -> SemanticAnalyzer ()
putVars vars = modify (\s -> s {variableTables = vars})

putFunc :: FunctionTable -> SemanticAnalyzer ()
putFunc func = modify (\s -> s {functionTable = func})

putReturnType :: Maybe DataType -> SemanticAnalyzer ()
putReturnType t = modify (\s -> s {returnType = t})

modifyVars :: ([VariableTable] -> [VariableTable]) -> SemanticAnalyzer ()
modifyVars f = modify
    $ \s -> s {variableTables = f $ variableTables s}

modifyFunc :: (FunctionTable -> FunctionTable) -> SemanticAnalyzer ()
modifyFunc f = modify
    $ \s -> s {functionTable = f $ functionTable s}

withVars :: ([VariableTable] -> SemanticAnalyzer a) -> SemanticAnalyzer a
withVars = (getVars >>=)

withFunc :: (FunctionTable -> SemanticAnalyzer a) -> SemanticAnalyzer a
withFunc = (getFunc >>=)

withVars' :: ([VariableTable] -> a) -> SemanticAnalyzer a
withVars' = (<$> getVars)

withFunc' :: (FunctionTable -> a) -> SemanticAnalyzer a
withFunc' = (<$> getFunc)
