{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
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

import Data.Char (Char)
import Data.Eq (Eq)
import Data.Int (Int32)
import Data.String (IsString)
import Text.Show (Show)

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
    | Not Variable
    | Eq  Variable Variable
    | Neq Variable Variable
    | LT Variable Variable
    | LE Variable Variable
    | GT Variable Variable
    | GE Variable Variable
    | Const Constant
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

data Constant
    = Int Int32
    | Char Char
    | String Text
  deriving (Show)

data TAC = TAC Variable Operator
  deriving (Show)
