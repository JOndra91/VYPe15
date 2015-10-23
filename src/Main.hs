{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad ((>>=))
import Data.Function ((.))
import System.IO (IO, print, getContents)
import Text.Parsec (parse)

import VYPe15.Internal.Parser (parseVYPe15)

main :: IO ()
main = getContents >>= print . parse parseVYPe15 "" 

