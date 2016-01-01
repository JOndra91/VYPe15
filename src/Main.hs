{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad (return, (>>), (>>=))
import Control.Monad.Except (Except, ExceptT(ExceptT), runExcept)
import Data.Either (Either(Left, Right), either)
import Data.Function (($), (.))
import Data.Functor.Identity (Identity(Identity))
import Data.Int (Int)
import Data.String (String)
import Data.Text.IO (writeFile)
import Data.Tuple (uncurry)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (FilePath, IO, hPutStrLn, readFile, stderr)
import Text.Parsec (parse)
import Text.Show (Show, show)

import VYPe15.Internal.AssemblyGenerator (generateAssembly)
import VYPe15.Internal.Parser (parseVYPe15)
import VYPe15.Internal.Semantics (semanticAnalysis)

main :: IO ()
main = getArgs >>= \case
    [inputFile] -> runWithArgs inputFile "out.asm"
    [inputFile, outputFile] -> runWithArgs inputFile outputFile
    _ -> usage
  where
    usage = do
        hPutStrLn stderr "./vype <inputFile> [outputFile]"
        exitWith $ ExitFailure 5

runWithArgs :: FilePath -> FilePath -> IO ()
runWithArgs inputFile outputFile = readFile inputFile >>= \input -> runWithExcept $ do
    ast <- liftE 2 $ parse parseVYPe15 inputFile input
    tac <- liftE 3 $ semanticAnalysis ast
    return $ generateAssembly tac
  where
    liftE :: (Show e) => Int -> Either e a -> Except (Int, String) a
    liftE code = ExceptT . Identity . either (Left . (code,) . show) Right

    runWithExcept = either (uncurry printErr) (writeFile outputFile) . runExcept


printErr :: Int -> String -> IO ()
printErr code err = hPutStrLn stderr err >> exitWith (ExitFailure code)
