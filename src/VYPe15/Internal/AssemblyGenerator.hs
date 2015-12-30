{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module VYPe15.Internal.AssemblyGenerator
  where

import Data.Text (Text)

import VYPe15.Types.TAC (TAC)


generateAssembly :: [TAC] -> Text
generateAssembly _ = "Ta-Da!"
