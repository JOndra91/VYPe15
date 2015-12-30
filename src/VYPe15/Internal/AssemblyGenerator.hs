{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module VYPe15.Internal.AssemblyGenerator
    ( generateAssembly
    )
  where

import Prelude (error)

import Control.Applicative (pure)
import Control.Monad (mapM_)
import Control.Monad.State (modify)
import Control.Monad.Writer (tell)
import Data.Bool (Bool(False, True))
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.List (groupBy)
import qualified Data.Map as M (empty)
import Data.Text (Text)

import VYPe15.Internal.Util (showText)
import VYPe15.Types.Assembly
    ( ASM(Label)
    , Assembly
    , AssemblyState(AssemblyState, paramCounter, stringCounter, stringTable, variableCounter, variableTable)
    , addParam
    , evalAssembly
    )
import VYPe15.Types.AST (Identifier(getId), Param(AnonymousParam, Param))
import VYPe15.Types.SymbolTable (Function(functionParams), Variable(Variable))
import VYPe15.Types.TAC (Label, Operator, TAC(Assign, Begin))


generateAssembly :: [TAC] -> Text
generateAssembly tac =
    showText $ fmap (evalAssembly initialState . generateAssembly') $ functions tac
    -- showText $ evalAssembly initialState $ mapM_ generateAssembly' $ functions tac
  where
    functions = groupBy (\_ b -> isBegin b)

    isBegin = \case
        Begin _ _ -> False
        _ -> True

    initialState = AssemblyState
      { variableTable = M.empty
      , stringTable = []
      , stringCounter = 0
      , paramCounter = 0
      , variableCounter = 0
      }

    generateAssembly' :: [TAC] -> Assembly ()
    generateAssembly' = mapM_ handleTAC

handleTAC :: TAC -> Assembly ()
handleTAC = \case
    Assign var op -> handleAssign var op
    -- Call (Maybe Variable) Label
    -- PushParam Variable
    -- PopParams Word32
    -- Label Label
    Begin l fn -> handleBegin l fn
    -- JmpZ Variable Label
    -- Goto Label
    -- Return (Maybe Variable)
    -- Print (Variable)
    _ -> pure ()

handleAssign :: Variable -> Operator -> Assembly ()
handleAssign _var _op = pure ()

handleBegin ::  Label -> Function -> Assembly ()
handleBegin l fn = do
    tell [Label l]
    modify (\s -> s
        { variableTable = M.empty
        , variableCounter = 0
        , paramCounter = 0
        })
    mapM_ (addParam . paramToVar)  $ functionParams fn
  where
    paramToVar (Param dt id) = Variable (getId id) dt
    paramToVar (AnonymousParam _) = error "This should not happen."
