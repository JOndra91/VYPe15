{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module VYPe15.Types.Assembly
  where

import Control.Applicative (Applicative)
import Control.Monad (Monad)
import Control.Monad.State (MonadState, State)
import Control.Monad.Writer (MonadWriter, WriterT)
import Data.Functor (Functor)
import Data.Int (Int32)
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Word (Word32)
import Text.Show (Show(show))

import VYPe15.Types.SymbolTable (Variable)
import VYPe15.Types.TAC (Label)

-- | Location of variable in reference to either stack pointer of frame pointer.
data Location
  = LocSP Int32
  | LocFP Int32

type VariableTable = Map Variable Address

type StringTable = Map Word32 Text

data AssemblyState = AssemblyState
    { variableTable :: VariableTable
    , stringTable :: StringTable
    , stringCounter :: Word32
    }

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
        RAM o b -> show o <> "(" <> show b <> ")"
        Data t -> show t

data ASM
    -- Immediate instructions
    = LI Register Int32 -- ^ Load immediate
    | ADDI Register Int32 -- ^ Add immediate
    -- Load and store
    | LW Register Address -- ^ Load word
    | LB Register Address -- ^ Load byte
    | SW Register Address -- ^ Store word
    | SB Register Address -- ^ Store byte
    -- Arithmetic
    | MOV Register Register
    | ADD Register Register Register
    | SUB Register Register Register
    | MUL Register Register -- ^ Stores 64-bit result in Hi and Lo registers
    | DIV Register Register -- ^ Stores result in Lo and remainder in Hi register
    -- Logical bit-wise
    | AND Register Register Register
    | OR Register Register Register
    -- Bit operation
    | SRL Register Register Word32 -- ^ Shift right logical
    -- Special register manipulation instructions
    | MFHi Register -- ^ Move from Hi
    | MFLo Register -- ^ Move from Lo
    -- Branching, jumping and sub-routine calls
    | JAL Label -- ^ Jump and Link (sub-routine call)
    | JR Register -- ^ Jump register
    | B Label -- ^ Branch (jump alternative)
    | BEQ Register Register Label
    | BLT Register Register Label
    | BLE Register Register Label
    | BGT Register Register Label
    | BGE Register Register Label
    | BNE Register Register Label
    -- Declarations and directives
    | Label Label
  deriving (Show) -- Just for testing


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
