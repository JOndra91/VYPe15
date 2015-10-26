{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module VYPe15.Types.Semantics
where

import Control.Applicative (Applicative(pure, (<*>)))
import Control.Monad (Monad(fail, return, (>>=)))
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader.Class (MonadReader, ask, local)
import Control.Monad.State.Class (MonadState, get, put)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State (State)
import Data.Function (($), (.))
import Data.Functor (Functor(fmap), (<$>))
import Data.String (String)
import Text.Show (Show)


import VYPe15.Types.SymbolTable (FunctionTable, VariableTable)


data SError
    = SError String
    deriving (Show)

-- \r -> \s -> (Either SError a, s)

newtype SemanticAnalyzer a
    = SemanticAnalyzer { runSemAnalyzer :: ExceptT SError (ReaderT FunctionTable (State [VariableTable])) a}

-- Could have used generalized newtype deriving but this is a good
-- exercise anyway
instance Functor SemanticAnalyzer where
    fmap f fa = SemanticAnalyzer $ f <$> runSemAnalyzer fa

instance Applicative SemanticAnalyzer where
    pure = SemanticAnalyzer . pure
    a <*> b = SemanticAnalyzer $ runSemAnalyzer a <*> runSemAnalyzer b

instance Monad SemanticAnalyzer where
    return = pure
    a >>= f = SemanticAnalyzer (runSemAnalyzer a >>= runSemAnalyzer . f)
    fail = SemanticAnalyzer . fail

instance MonadReader FunctionTable SemanticAnalyzer where
   ask = SemanticAnalyzer ask
   local f = SemanticAnalyzer . local f . runSemAnalyzer

instance MonadState [VariableTable] SemanticAnalyzer where
   get = SemanticAnalyzer get
   put = SemanticAnalyzer . put
