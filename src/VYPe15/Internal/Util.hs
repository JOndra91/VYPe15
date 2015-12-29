{-# LANGUAGE NoImplicitPrelude #-}

module VYPe15.Internal.Util
  where

import Data.Function ((.))
import Data.Text (Text)
import qualified Data.Text as T (pack)
import Text.Show (Show(show))


-- {{{ Utility functions ------------------------------------------------------

withHead :: (a -> a) -> [a] -> [a]
withHead f (h:t) = f h : t
withHead _ [] = []

showText :: Show a => a -> Text
showText = T.pack . show

-- }}} Utility functions ------------------------------------------------------
