{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module VYPe15.Types.Semantics
where

import Control.Applicative (Applicative)
import Control.Monad (Monad)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State (State)
import Data.Functor (Functor)
import Data.String (String)
import Text.Show (Show)

import VYPe15.Types.SymbolTable (FunctionTable, VariableTable)

newtype SError
    = SError String
  deriving (Show)

newtype SemanticAnalyzer a
    = SemanticAnalyzer { runSemAnalyzer ::
        ExceptT SError (ReaderT FunctionTable (State [VariableTable])) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError SError
    , MonadReader FunctionTable
    , MonadState [VariableTable]
    )
