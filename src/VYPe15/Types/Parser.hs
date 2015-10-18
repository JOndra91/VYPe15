{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module VYPe15.Types.Parser
where

import           Control.Applicative ( Applicative )
import           Control.Monad ( Monad )
import           Control.Monad.Trans.State ( StateT )
import           Control.Monad.Trans.Class ( MonadTrans )
import           Data.Attoparsec.ByteString ( Parser )
import           Data.Functor ( Functor )
import           Text.Show ( Show )


data ParserState = ParserState deriving ( Show )

type VYPe15Parser a = VYPe15ParserInternal Parser a
newtype VYPe15ParserInternal m a = VYPe15ParserInternal 
  { runVYPe15Parser :: (StateT ParserState m a)} deriving
  ( Applicative
  , Monad
  , Functor
  , MonadTrans
  )
