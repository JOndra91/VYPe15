{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Data.Function (($))
import           GHC.Err(error)
import           System.IO (IO, print, getContents)

import           VYPe15.Internal.Parser (parseVYPe15)
import           VYPe15.Internal.Lexer (alexScanTokens)
import           VYPe15.Types.Parser 
    (Parser(runParser), ParserResult(ParseOK, ParseFail), ParserState(ParserState))

main :: IO ()
main = do
    c <- getContents
    case (runParser $ parseVYPe15 (alexScanTokens c)) (ParserState 1) of
        ParseOK a -> print a
        ParseFail s -> error s
