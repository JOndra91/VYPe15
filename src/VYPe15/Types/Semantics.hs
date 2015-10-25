{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module VYPe15.Types.Semantics
where

import Control.Applicative(Applicative(pure))
import Control.Monad(Monad(return, (>>=), fail))
import Control.Monad.Except (ExceptT(ExceptT), runExceptT)
import Control.Monad.Reader.Class(MonadReader, ask)
import Control.Monad.State.Class(MonadState, get, put)
import Control.Monad.Trans.Reader (ReaderT(ReaderT), runReaderT)
import Control.Monad.Trans.State (State, state, runState)
import Data.Either (Either(Right,Left))
import Data.Function (($))
import Data.Functor(Functor(fmap))
import Data.String (String)
import Text.Show(Show)


import VYPe15.Types.SymbolTable (FunctionTable, VariableTable)


data SError
    = SError String
    deriving (Show)

-- \r -> \s -> (Either SError a, s)

newtype SemanticAnalyzer a 
    = SemanticAnalyzer { runSemAnalyzer :: ExceptT SError (ReaderT FunctionTable (State [VariableTable])) a}

-- Could have used generalized newtype deriving but this is a good
-- execise anyway
instance Functor SemanticAnalyzer where
    fmap f b = SemanticAnalyzer $ ExceptT $ ReaderT $ \r -> state $ \s ->
        case runState (runReaderT (runExceptT $ runSemAnalyzer b) r) s of
            (Left a, s') -> (Left a, s')
            (Right a, s') -> (Right (f a), s')

instance Applicative SemanticAnalyzer where
    pure a = SemanticAnalyzer $ ExceptT $ ReaderT $ \_ -> state $ \s ->
        (Right a,s)

instance Monad SemanticAnalyzer where
    return = pure
    b >>= f = SemanticAnalyzer $ ExceptT $ ReaderT $ \r -> state $ \s ->
        case runState (runReaderT (runExceptT $ runSemAnalyzer b) r) s of
            (Left a, s') -> (Left a, s')
            (Right a, s') -> runState (runReaderT (runExceptT $ runSemAnalyzer $ f a) r) s'
    fail reason = SemanticAnalyzer $ ExceptT $ ReaderT $ \_ -> state $ \s ->
        (Left $ SError reason,s)

instance MonadReader FunctionTable SemanticAnalyzer where
   ask = SemanticAnalyzer $ ExceptT $ ReaderT $ \r -> state $ \s -> (Right r,s)

instance MonadState [VariableTable] SemanticAnalyzer where
   get = SemanticAnalyzer $ ExceptT $ ReaderT $ \_ -> state $ \s -> (Right s,s)
   put s = SemanticAnalyzer $ ExceptT $ ReaderT $ \_ -> state $ \_ -> (Right (),s)


