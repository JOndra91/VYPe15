{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Trans.State ( evalStateT )
import           Data.Attoparsec.ByteString ( parse )
import           Data.ByteString ( ByteString )
import           Data.Function ( ($) )
import           System.IO ( IO, print )

import           VYPe15.Internal.Parser ()
import           VYPe15.Types.Parser ()

sourceCode :: ByteString
sourceCode = "identifier = 3 + identifier'"

main :: IO ()
main = print "123"
