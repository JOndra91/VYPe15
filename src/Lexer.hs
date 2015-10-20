{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Data.Function (($))
import System.IO (IO, getContents, print)

import VYPe15.Internal.Lexer (alexScanTokens)

main :: IO ()
main = do
    c <- getContents
    print $ alexScanTokens c
