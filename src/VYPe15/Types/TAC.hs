{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module:       $HEADER$
-- Description:  Type for three-adress code (TAC)
-- Copyright:    (c) 2015 Ixperta, s.r.o.
-- License:      AllRightsReserved
--
-- Maintainer:   Ixcom development team <ixcom-dev@ixperta.com>
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Type for three-adress code (TAC)
module VYPe15.Types.TAC
  where

import Data.Eq (Eq)
import Text.Show (Show)
import Data.String (IsString)

import Data.Text (Text)

import VYPe15.Types.SymbolTable (Variable)

data Operator
    = Mul Variable Variable
    | Div Variable Variable
    | Mod Variable Variable
    | Sub Variable Variable
    | Add Variable Variable
    | Set Variable
    | And Variable Variable
    | Or Variable Variable
    | Neg Variable
    | Eq  Variable Variable
    | Neq Variable Variable
    | LT Variable Variable
    | LE Variable Variable
    | GT Variable Variable
    | GE Variable Variable
    | Label Label
    | Call Label
    | Begin
    | End
    | JmpZ Variable Label
    | Goto Label
    | Return Variable
  deriving (Show)

newtype Label = Label' Text
  deriving (Show, Eq, IsString)

data TAC = TAC Variable Operator
  deriving (Show)
