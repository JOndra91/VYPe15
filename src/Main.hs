{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad ((>>=))
import Data.Either (either)
import Data.Function ((.))
import System.IO (IO, print, getContents)
import Text.Parsec (parse)

import VYPe15.Internal.Parser (parseVYPe15)
import VYPe15.Internal.Semantics (semanticAnalysis)

main :: IO ()
main = getContents >>= either print (print . semanticAnalysis) . parse parseVYPe15 ""


