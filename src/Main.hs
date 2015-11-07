{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>))
import Control.Monad ((>=>), (>>=))
import Data.Either (either)
import Data.Function ((.))
import Data.List (head)
import Data.Text (unlines)
import Data.Text.IO (putStrLn)
import System.Environment (getArgs)
import System.IO (IO, print, readFile)
import Text.Parsec (parse)

import VYPe15.Internal.Parser (parseVYPe15)
import VYPe15.Internal.Semantics (semanticAnalysis)
import VYPe15.Types.TAC (TAC, strTac)

main :: IO ()
main = head <$> getArgs >>=
    ( readFile >=>
        either print (either print prettyPrintTac . semanticAnalysis)
          . parse parseVYPe15 ""
    )

prettyPrintTac :: [TAC] -> IO ()
prettyPrintTac = putStrLn . unlines . (strTac <$>)
