{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Data.Function (($), (.))
import Data.Int (Int32)
import Data.Maybe (Maybe, maybe)
import Data.Monoid ((<>))
import Data.String (IsString(fromString))
import Text.Show (Show(show))

import Data.Text (Text)

import VYPe15.Internal.Util (showText)
import VYPe15.Types.SymbolTable (Variable(varId))

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
  deriving (Show)

newtype Label = Label' { label' :: Text }
  deriving (Show, Eq, IsString)

data Constant
    = Int Int32
    | Char Char
    | String Text
  deriving (Show)

data TAC
    = Assign Variable Operator
    | Call (Maybe Variable) Label
    | PushParam Variable
    | PopParams Int32
    -- ^ Parameter is number of bytes
    | Label Label
    | Begin
    | JmpZ Variable Label
    | Goto Label
    | Return (Maybe Variable)
    | Print (Variable)
  deriving (Show)

strTac :: TAC -> Text
strTac = \case
    Assign v op -> indent $ strVar v <> " := " <> strOp op
    Call v l -> indent $ maybe "" ((<> " := ") . strVar) v <> label' l <> "()"
    PushParam v -> indent $ "Push " <> strVar v
    PopParams n -> indent $ "Pop " <> showText n
    Label l -> label' l <> ":"
    Begin -> "Begin"
    JmpZ v l -> indent $ "JmpZ " <> strVar v <> ": " <> label' l
    Goto l -> indent $ "GoTo: " <> label' l
    Return v -> indent $ "Return: " <> maybe "()" strVar v
    Print v -> indent $ "Print " <> strVar v

  where
    indent :: Text -> Text
    indent = ("  " <>)

    strVar :: Variable -> Text
    strVar = ("$" <>) . varId

    strOp :: Operator -> Text
    strOp = \case
        Mul a b -> op2 a b "*"
        Div a b -> op2 a b "/"
        Mod a b -> op2 a b "%"
        Sub a b -> op2 a b "-"
        Add a b -> op2 a b "+"
        Set a -> strVar a
        And a b -> op2 a b "&&"
        Or a b -> op2 a b "||"
        Not a -> op1 a "!"
        Eq a b -> op2 a b "=="
        Neq a b -> op2 a b "!="
        LT a b -> op2 a b "<"
        LE a b -> op2 a b "<="
        GT a b -> op2 a b ">"
        GE a b -> op2 a b ">="
        Const c -> fromString $ show c
      where
        op1 :: Variable -> Text -> Text
        op1 a op = op <> strVar a

        op2 :: Variable -> Variable -> Text -> Text
        op2 a b op = strVar a <> " " <> op <> " " <> strVar b
