{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Default (Default(def))
import Data.Function (($))
import GHC.Err(error)
import System.IO (IO, print, getContents)

import VYPe15.Internal.Parser (parseVYPe15)
import VYPe15.Internal.Lexer (alexScanTokens)
import VYPe15.Types.Parser 
    (Parser(runParser), ParserResult(ParseOK, ParseFail), ParserState(ParserState))

main :: IO ()
main = do
    c <- getContents
    case (runParser $ parseVYPe15 (alexScanTokens c)) def def of
        ParseOK a -> print a
        ParseFail s -> error s
