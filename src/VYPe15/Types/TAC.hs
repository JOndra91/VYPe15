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
module VYPe15.Type.TAC
  where

data Operator
    = Mul  -- 2 Params
    | Div  -- 2 Params
    | Mod  -- 2 Params
    | Sub  -- 2 Params
    | Add  -- 2 Params
    | MovB -- 1 Param (Move byte)
    | MovW -- 1 Param (Move word)
    | SetB -- 1 Param
    | SetW -- 1 Param
    | SetS -- 2 Param (Set string, probably string value and length)
    | And  -- 2 Params
    | Or   -- 2 Params
    | Neg  -- 1 Param
    | Eq   -- 2 Params
    | Neq  -- 2 Params
    | LT   -- 2 Params
    | LE   -- 2 Params
    | GT   -- 2 Params
    | GE   -- 2 Params
    | JmpZ -- Value, Label
    | Goto -- Label
    | Push -- 1 Param
    | Pop  -- 1 Param
    | Call -- Label (Function name)
    | Begin -- 1 Param (Stack size)
    | End  -- No Param
    | Return -- 1 Param
