{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad ((>>=))
import           Control.Monad.Trans.State ( evalStateT )
import           Data.Attoparsec.ByteString ( parse )
import           Data.ByteString ( ByteString )
import           Data.Function ( ($), (.) )
import           System.IO ( IO, print, getContents )

import           VYPe15.Internal.Parser ( parseVYPe15 )
import           VYPe15.Internal.Lexer ( alexScanTokens )
import           VYPe15.Types.Parser ()

sourceCode :: ByteString
sourceCode = "identifier = 3 + identifier'"

main :: IO ()
main = getContents >>= print . parseVYPe15 . alexScanTokens
