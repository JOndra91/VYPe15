{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module VYPe15.Types.Assembly
  where

import Prelude (Enum(succ), Num((+), (-)), error, fromIntegral)

import Control.Applicative (Applicative)
import Control.Monad (Monad, return, (>>=))
import Control.Monad.State (MonadState, State, get, runState, state)
import Control.Monad.Writer (MonadWriter, WriterT, execWriterT)
import Data.Function (($), (.))
import Data.Functor (Functor, (<$>))
import Data.Int (Int32)
import Data.Map (Map)
import qualified Data.Map as M (insert, lookup)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>))
import Data.String (String)
import Data.Text (Text, unpack)
import Data.Tuple (swap)
import Data.Word (Word32)
import Text.Show (Show(show))

import VYPe15.Internal.Util (showText)
import VYPe15.Types.AST (getTypeSize)
import VYPe15.Types.SymbolTable (Variable(varType))
import VYPe15.Types.TAC (Label(Label', label'))


type VariableTable = Map Variable Address

type StringTable = [(Word32,Text)]

data AssemblyState = AssemblyState
    { variableTable :: VariableTable
    , stringTable :: StringTable
    , functionLabel :: Label
    , stringCounter :: Word32
    , paramCounter :: Int32
    , variableCounter :: Int32
    , labelCounter :: Word32
    }
  deriving(Show)

data Register
    = Zero -- ^ [$0] Zero (contains zero)
    | V0 -- ^ [$2] Values
    | V1 -- ^ [$3] Values
    | A0 -- ^ [$4] Argument
    | A1 -- ^ [$5] Arguments
    | A2 -- ^ [$6] Arguments
    | A3 -- ^ [$7] Arguments
    | T0 -- ^ [$8] Temporaries
    | T1 -- ^ [$9] Temporaries
    | T2 -- ^ [$10] Temporaries
    | T3 -- ^ [$11] Temporaries
    | T4 -- ^ [$12] Temporaries
    | T5 -- ^ [$13] Temporaries
    | T6 -- ^ [$14] Temporaries
    | T7 -- ^ [$15] Temporaries
    | SP -- ^ [$29] Stack Pointer
    | FP -- ^ [$30] Frame Pointer
    | RA -- ^ [$31] Return Address

instance Show Register where
    show = \case
        Zero -> "$0"
        V0 -> "$2"
        V1 -> "$3"
        A0 -> "$4"
        A1 -> "$5"
        A2 -> "$6"
        A3 -> "$7"
        T0 -> "$8"
        T1 -> "$9"
        T2 -> "$10"
        T3 -> "$11"
        T4 -> "$12"
        T5 -> "$13"
        T6 -> "$14"
        T7 -> "$15"
        SP -> "$sp"
        FP -> "$fp"
        RA -> "$ra"

data Address
    = Reg Register
    | RAM
      { base :: Register
      , offset :: Int32
      }
    | Data Text

instance Show Address where
    show = \case
        Reg r -> show r
        RAM b o -> show o <> "(" <> show b <> ")"
        Data t -> show t

data ASM
    -- Immediate instructions
    = LI Register Int32 -- ^ Load immediate
    | ADDI Register Register Int32 -- ^ Add immediate
    | ADDIU Register Register Int32 -- ^ Add immediate (unsinged)
    -- Load and store
    | LW Register Address -- ^ Load word
    | LB Register Address -- ^ Load byte
    | SW Register Address -- ^ Store word
    | SB Register Address -- ^ Store byte
    -- Manipulation
    | MOV Register Register
    | MOVZ Register Register Register
    -- Arithmetic
    | ADD Register Register Register
    | SUB Register Register Register
    | MUL Register Register -- ^ Stores 64-bit result in Hi and Lo registers
    | DIV Register Register -- ^ Stores result in Lo and remainder in Hi register
    -- Logical bit-wise
    | AND Register Register Register
    | OR Register Register Register
    | XOR Register Register Register
    -- Bit operation
    | SRL Register Register Word32 -- ^ Shift right logical
    -- Special register manipulation instructions
    | MFHi Register -- ^ Move from Hi
    | MFLo Register -- ^ Move from Lo
    -- Branching, jumping and sub-routine calls
    | JAL Label -- ^ Jump and Link (sub-routine call)
    | JR Register -- ^ Jump register
    | B Label -- ^ Branch (jump alternative)
    | BEQZ Register Label
    | BLTZ Register Label
    | BLEZ Register Label
    | BGTZ Register Label
    | BGEZ Register Label
    | BNEZ Register Label
    -- Declarations and directives
    | Label Label
    | Asciz' Word32 Text
    | Data'
    | Text'
    | Org' Word32
    -- System
    | Break
    -- IO instructions
    | PrintInt Register
    | PrintChar Register
    | PrintString Register
    | ReadInt Register
    | ReadChar Register
    | ReadString Register

instance Show ASM where
    show = \case
        LI reg val -> indent $ inst2 "li" reg val
        ADDI dst src val -> indent $ inst3 "addi" dst src val
        ADDIU dst src val -> indent $ inst3 "addiu" dst src val
        LW reg addr -> indent $ inst2 "lw" reg addr
        LB reg addr -> indent $ inst2 "lb" reg addr
        SW reg addr -> indent $ inst2 "sw" reg addr
        SB reg addr -> indent $ inst2 "sb" reg addr
        MOV reg0 reg1 -> indent $ inst2 "move" reg0 reg1
        MOVZ dst src test -> indent $ inst3 "movz" dst src test
        ADD reg0 reg1 reg2 -> indent $ inst3 "add" reg0 reg1 reg2
        SUB reg0 reg1 reg2 -> indent $ inst3 "sub" reg0 reg1 reg2
        MUL reg0 reg1 -> indent $ inst2 "mult" reg0 reg1
        DIV reg0 reg1 -> indent $ inst2 "div" reg0 reg1
        AND reg0 reg1 reg2 -> indent $ inst3 "and" reg0 reg1 reg2
        OR reg0 reg1 reg2 -> indent $ inst3 "or" reg0 reg1 reg2
        XOR reg0 reg1 reg2 -> indent $ inst3 "xor" reg0 reg1 reg2
        SRL reg0 reg1 val -> indent $ inst3 "srl" reg0 reg1 val
        MFHi reg -> indent $ inst1 "mfhi" reg
        MFLo reg -> indent $ inst1 "mflo" reg
        JAL l -> indent $ "jal " <> label l
        JR reg -> indent $ inst1 "jr" reg
        B l -> indent $ "b " <> label l
        BEQZ reg0 l -> indent (inst2 "beq" reg0 Zero) <> ", " <> label l
        BLTZ reg0 l -> indent (inst1 "bltz" reg0) <> ", " <> label l
        BLEZ reg0 l -> indent (inst1 "blez" reg0) <> ", " <> label l
        BGTZ reg0 l -> indent (inst1 "bgtz" reg0) <> ", " <> label l
        BGEZ reg0 l -> indent (inst1 "bgez" reg0) <> ", " <> label l
        BNEZ reg0 l -> indent (inst2 "bne" reg0 Zero) <> ", " <> label l
        Label l -> label l <> ":"
        PrintInt reg -> indent $ inst1 "print_int" reg
        PrintChar reg -> indent $ inst1 "print_char" reg
        PrintString reg -> indent $ inst1 "print_string" reg
        ReadInt reg -> indent $ inst1 "read_int" reg
        ReadChar reg -> indent $ inst1 "read_char" reg
        ReadString reg -> indent $ inst1 "read_string" reg
        Asciz' n txt -> "__asciizString_" <> show n <> ":  .asciz  " <> show txt
        Data' -> ".data"
        Text' -> ".text"
        Org' n -> ".org " <> show n
        Break -> indent "break"
      where
        indent :: String -> String
        indent = ("  " <>)

        label :: Label -> String
        label l = unpack (label' l)

        inst1 :: (Show a) => String -> a -> String
        inst1 i op0 = i <> " " <> show op0

        inst2 :: (Show a, Show b) => String -> a -> b -> String
        inst2 i op0 op1 = i <> " " <> show op0 <> ", " <> show op1

        inst3 :: (Show a, Show b, Show c) => String -> a -> b -> c -> String
        inst3 i op0 op1 op2 =
            i <> " " <> show op0 <> ", " <> show op1 <> ", " <> show op2

newtype Assembly a
    = Assembly
    { runAssembly :: (WriterT  [ASM] (State AssemblyState)) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState AssemblyState
    , MonadWriter [ASM]
    )

evalAssembly
  :: AssemblyState
  -> Assembly a
  -> (AssemblyState, [ASM])
evalAssembly s = swap . (`runState` s) . execWriterT . runAssembly

addParam :: Variable -> Assembly Address
addParam var = state withState
  where
    withState s = (varAddress, newState)
      where
        newCounter = paramCounter s + (fromIntegral . getTypeSize $ varType var)

        varAddress = RAM FP newCounter

        newState = s
          { paramCounter = newCounter
          , variableTable = M.insert var varAddress (variableTable s)
          }

addVariable :: Variable -> Assembly Address
addVariable var = state withState
  where
    withState s = (varAddress, newState)
      where
        varAddress = RAM FP (variableCounter s)

        newState = s
          { variableCounter = variableCounter s - (fromIntegral . getTypeSize $ varType var)
          , variableTable = M.insert var varAddress (variableTable s)
          }

lookupVarAddr :: Variable -> Assembly (Maybe Address)
lookupVarAddr var = M.lookup var . variableTable <$> get

getVarAddr :: Variable -> Assembly Address
getVarAddr v = lookupVarAddr v >>= \case
    Just addr -> return addr
    Nothing -> error $ "BUG: Could not find variable address: " <> show v

addString :: Text -> Assembly Address
addString t = state withState
  where
    withState s = (stringAddress, newState)
      where
        stringAddress = Data $ "__asciizString_" <> showText (stringCounter s)

        newState = s
          { stringCounter = succ $ stringCounter s
          , stringTable = (stringCounter s, t) : stringTable s
          }

mkLabel :: Text -> Assembly Label
mkLabel name = do
  [l'] <- mkLabels [name]
  return l'

mkLabels :: [Text] -> Assembly [Label]
mkLabels ls = state withState
  where
    withState s = (labels, newState)
      where
        ctr = labelCounter s

        labels = mkLabel' <$> ls

        mkLabel' name = Label' $ "label__" <> name <> "_" <> showText ctr

        newState = s
          { labelCounter = succ ctr
          }

getFunctionLabel :: Assembly Label
getFunctionLabel = functionLabel <$> get

getReturnLabel :: Assembly Label
getReturnLabel = (\t -> "__" <> t <> "_return_") <$> getFunctionLabel
