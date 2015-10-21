{-# LANGUAGE NoImplicitPrelude #-}

module VYPe15.Types.Parser
where

import Control.Applicative (Applicative(pure, (<*>)))
import Control.Monad (Monad((>>=)))
import Data.Default (Default(def))
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

instance Default ParserState where
    def = ParserState 1

instance Functor Parser where
    fmap f (Parser x) = Parser $ \s t ->
        case x s t of
            ParseOK b -> ParseOK $ f b
            ParseFail r -> ParseFail r

instance Applicative Parser where
    pure v = Parser $ \_ _ -> ParseOK v
    (Parser f) <*> (Parser x) = Parser $ \s t ->
        case x s t of
            ParseOK b -> case f s t of
                ParseOK f' -> ParseOK $ f' b
                ParseFail r -> ParseFail r
            ParseFail r -> ParseFail r

instance Monad Parser where
    (Parser x) >>= f = Parser $ \s t ->
        case x s t of
            ParseOK a -> runParser (f a) s t
            ParseFail r -> ParseFail r
