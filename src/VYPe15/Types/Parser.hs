{-# LANGUAGE NoImplicitPrelude #-}

module VYPe15.Types.Parser
where

import Control.Applicative (Applicative(pure, (<*>)))
import Control.Monad (Monad((>>=)))
import Data.Functor (Functor(fmap))
import Data.Function (($))
import Data.Int(Int)
import Data.String(String)
import Text.Show ( Show )

import VYPe15.Types.SymbolTable (SymbolTable)

data ParserState
    = ParserState
    { currentLine :: Int
    } deriving (Show)

data ParserResult a
    = ParseOK a
    | ParseFail String
    deriving (Show)

newtype Parser a = Parser {runParser :: (ParserState -> SymbolTable -> ParserResult a)}

instance Functor Parser where
    fmap f (Parser x) = Parser $ \s ->
        case x s of
            ParseOK b -> ParseOK $ f b
            ParseFail r -> ParseFail r

instance Applicative Parser where
    pure v = Parser $ \_  -> ParseOK v
    (Parser f) <*> (Parser x) = Parser $ \s ->
        case x s of
            ParseOK b -> case f s of
                ParseOK f' -> ParseOK $ f' b
                ParseFail r -> ParseFail r
            ParseFail r -> ParseFail r

instance Monad Parser where
    (Parser x) >>= f = Parser $ \s ->
        case x s of
            ParseOK a -> runParser (f a) s
            ParseFail r -> ParseFail r
