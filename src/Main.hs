{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative ((<$>))
import Control.Monad ((>>=), (>=>))
import Data.Either (either)
import Data.Function ((.))
import Data.List (head)
import System.IO (IO, print, readFile)
import System.Environment (getArgs)
import Text.Parsec (parse)

import VYPe15.Internal.Parser (parseVYPe15)
import VYPe15.Internal.Semantics (semanticAnalysis)

main :: IO ()
main = head <$> getArgs >>=
    ( readFile >=>
      either print (print . semanticAnalysis) . parse parseVYPe15 ""
    )
