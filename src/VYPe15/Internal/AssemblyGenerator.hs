{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module VYPe15.Internal.AssemblyGenerator
    ( generateAssembly
    )
  where

import Prelude (Integral, error, fromIntegral)

import Control.Applicative (pure)
import Control.Monad (mapM_, return, (>>), (>>=))
import Control.Monad.State (modify)
import Control.Monad.Writer (tell)
import Data.Bool (Bool(False, True))
import Data.Char (ord)
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.List (groupBy)
import qualified Data.Map as M (empty)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>))
import Data.Text (Text)

import VYPe15.Internal.Util (showText)
import VYPe15.Types.Assembly
    ( ASM(ADD, AND, B, BEQ, BGE, BGT, BLE, BLT, BNE, DIV, JAL, JR, LB, LI, LW, Label, MFHi, MFLo, MOV, MUL, OR, SB, SRL, SUB, SW, XOR)
    , Address(Data, RAM, Reg)
    , Assembly
    , AssemblyState(AssemblyState, labelCounter, paramCounter, returnLabel, stringCounter, stringTable, variableCounter, variableTable)
    , Register(A0, A1, A2, A3, FP, RA, SP, T0, T1, T2, T3, T4, T5, T6, T7, V0, V1, Zero)
    , addParam
    , addVariable
    , evalAssembly
    , getReturnLabel
    , getVarAddr
    , lookupVarAddr
    , mkLabel
    )
import VYPe15.Types.AST
    ( DataType(DChar, DInt, DString)
    , Identifier(getId)
    , Param(AnonymousParam, Param)
    )
import VYPe15.Types.SymbolTable (Function(functionParams), Variable(Variable))
import VYPe15.Types.TAC (Constant, Label, Operator, TAC)
import qualified VYPe15.Types.TAC as C (Constant(Char, Int, String))
import qualified VYPe15.Types.TAC as TAC
    (TAC(Assign, Begin, Call, Goto, Label, Return))
import qualified VYPe15.Types.TAC as Op
    ( Operator(Add, And, Const, Div, Eq, GE, GT, LE, LT, Mod, Mul, Neq, Not, Or, Set, Sub)
    )


generateAssembly :: [TAC] -> Text
generateAssembly tac =
    showText $ fmap (evalAssembly initialState . generateAssembly') $ functions tac
    -- showText $ evalAssembly initialState $ mapM_ generateAssembly' $ functions tac
  where
    functions = groupBy (\_ b -> isBegin b)

    isBegin = \case
        TAC.Begin _ _ -> False
        _ -> True

    initialState = AssemblyState
      { variableTable = M.empty
      , stringTable = []
      , stringCounter = 0
      , paramCounter = 0
      , variableCounter = 0
      , labelCounter = 0
      , returnLabel = "__quit_program_"
      }

    generateAssembly' :: [TAC] -> Assembly ()
    generateAssembly' = mapM_ handleTAC

handleTAC :: TAC -> Assembly ()
handleTAC = \case
    TAC.Assign var op -> handleAssign var op
    TAC.Call mvar l -> handleCall mvar l
    -- TAC.PushParam Variable
    -- TAC.PopParams Word32
    TAC.Label l -> tell [Label l]
    TAC.Begin l fn -> handleBegin l fn
    -- TAC.JmpZ Variable Label
    TAC.Goto l -> tell [B l]
    TAC.Return mvar -> handleReturn mvar
    -- TAC.Print (Variable)
    _ -> pure ()

handleAssign :: Variable -> Operator -> Assembly ()
handleAssign dst = \case
    Op.Mul v1 v2 -> binaryOpMFReg MUL MFLo v1 v2
    Op.Div v1 v2 -> binaryOpMFReg DIV MFLo v1 v2
    Op.Mod v1 v2 -> binaryOpMFReg DIV MFHi v1 v2
    Op.Sub v1 v2 -> binaryOp SUB v1 v2
    Op.Add v1 v2 -> binaryOp ADD v1 v2
    Op.Set v -> loadVar T0 v >> storeVar T0 dst
    Op.And v1 v2 -> binaryOp AND v1 v2
    Op.Or  v1 v2 -> binaryOp OR v1 v2
    Op.Not v -> do
        loadVar T0 v
        tell
          [ LI T1 1
          , XOR T0 T1 T2
          ]
        storeVar T2 dst
    Op.Eq v1 v2 -> binaryOpLogic BEQ v1 v2 "Eq"
    Op.Neq v1 v2 -> binaryOpLogic BNE v1 v2 "Neq"
    Op.LT v1 v2 -> binaryOpLogic BLT v1 v2 "LT"
    Op.LE v1 v2 -> binaryOpLogic BLE v1 v2 "LE"
    Op.GT v1 v2 -> binaryOpLogic BGT v1 v2 "GT"
    Op.GE v1 v2 -> binaryOpLogic BGE v1 v2 "GE"
    Op.Const c -> loadConstant c
  where
    loadVar :: Register -> Variable -> Assembly ()
    loadVar r v = do
        v' <- getVarAddr v
        tell [lv v r v']

    storeVar :: Register -> Variable -> Assembly ()
    storeVar r v = do
        v' <- lookupVarAddr v >>= \case
            Just addr -> return addr
            Nothing -> addVariable v
        tell [sv v r v']

    binaryOp
      :: (Register -> Register -> Register -> ASM)
      -> Variable
      -> Variable
      -> Assembly ()
    binaryOp op v1 v2 = do
        loadVar T0 v1
        loadVar T1 v2
        tell [op T0 T1 T2]
        storeVar T2 dst

    binaryOpMFReg
      :: (Register -> Register -> ASM)
      -> (Register -> ASM)
      -> Variable
      -> Variable
      -> Assembly ()
    binaryOpMFReg op mf v1 v2 = do
        loadVar T0 v1
        loadVar T1 v2
        tell
          [ op T0 T1
          , mf T2
          ]
        storeVar T2 dst

    binaryOpLogic
      :: (Register -> Register -> Label -> ASM)
      -> Variable
      -> Variable
      -> Text
      -> Assembly ()
    binaryOpLogic branch v1 v2 labelName = do
        loadVar T0 v1
        loadVar T1 v2
        l <- mkLabel labelName
        tell
          [ LI T2 1
          , branch T0 T1 l
          , LI T2 0
          , Label l
          ]
        storeVar T2 dst

    loadConstant :: Constant -> Assembly ()
    loadConstant = \case
        C.Int n -> loadVal dst n
        C.Char n -> loadVal dst $ ord n
        C.String _s -> pure () -- Memory allocation strategy is needed.

    loadVal :: (Integral a) => Variable -> a -> Assembly ()
    loadVal v n = do
       tell [LI T0 $ fromIntegral n]
       storeVar T0 v

handleBegin ::  Label -> Function -> Assembly ()
handleBegin l fn = do
    tell [Label l]
    modify (\s -> s
        { variableTable = M.empty
        , returnLabel = "__" <> l <> "_return_"
        , variableCounter = 0
        , paramCounter = 0
        })
    mapM_ (addParam . paramToVar)  $ functionParams fn
  where
    paramToVar (Param dt id) = Variable (getId id) dt
    paramToVar (AnonymousParam _) = error "BUG: Unexpected anonymous param."

handleCall :: Maybe Variable -> Label -> Assembly ()
handleCall mvar l = do
    assign <- case mvar of
        Just v -> do
          v' <- addVariable v
          return [sv v V0 v']
        Nothing -> pure []
    tell assign

handleReturn :: Maybe Variable -> Assembly ()
handleReturn mvar = do
    assign <- case mvar of
        Just v -> do
            v' <- getVarAddr v
            return [lv v V0 v']
        Nothing -> pure []
    retLabel <- getReturnLabel
    tell $ assign <> [B retLabel]

lv :: Variable -> Register -> Address -> ASM
lv (Variable _ vType) = case vType of
    DInt -> LW
    DChar -> LB
    DString -> LW

sv :: Variable -> Register -> Address -> ASM
sv (Variable _ vType) = case vType of
    DInt -> SW
    DChar -> SB
    DString -> SW
