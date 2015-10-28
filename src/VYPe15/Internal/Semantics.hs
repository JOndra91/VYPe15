{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module VYPe15.Internal.Semantics
where

import Control.Monad (liftM, mapM, mapM_, return, unless, void, (>>))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (get, modify, put)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.State (evalState)
import Data.Bool (Bool(False, True), (&&))
import Data.Either (Either)
import Data.Eq ((==))
import Data.Foldable (and, foldl)
import Data.Function (flip, ($), (.))
import Data.Functor (fmap)
import Data.List (elem, filter, head, tail)
import Data.Map as M (empty, insert, keys, unions, (!))
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Monoid ((<>))
import Data.String (String)
import Data.Tuple (fst, snd)
import Text.Show (show)

import VYPe15.Types.AST
    ( DataType(DChar, DInt, DString)
    , Exp
      ( AND, Cast, ConsChar, ConsNum, ConsString, Div, Eq, FuncCallExp, Greater
      , GreaterEq, IdentifierExp, Less, LessEq, Minus, Mod, NOT, NonEq, OR, Plus
      , Times
      )
    , FunDeclrOrDef(FunDef)
    , Identifier(Identifier)
    , Param(Param)
    , Program
    , Stat(Assign, FuncCall, If, Return, VarDef, While)
    )
import VYPe15.Types.Semantics (SError(SError), SemanticAnalyzer(runSemAnalyzer))
import VYPe15.Types.SymbolTable (builtInFunctions)

semanticAnalysis :: Program -> Either SError ()
semanticAnalysis ast = flip evalState []
    . flip runReaderT mkFunctionTable
    . runExceptT
    . runSemAnalyzer $ semanticAnalysis' ast
  where
    mkFunctionTable = foldl mkFunctionTable' builtInFunctions ast
    mkFunctionTable' table = \case
        FunDef rt (Identifier i) p _ -> M.insert i (rt, p) table
        _ -> table

semanticAnalysis' :: Program -> SemanticAnalyzer ()
semanticAnalysis' = mapM_ checkFunctionDef . filter isFunctionDef
  where
    isFunctionDef FunDef{} = True
    isFunctionDef _ = False

checkFunctionDef
    :: FunDeclrOrDef
    -> SemanticAnalyzer ()
checkFunctionDef = \case
    (FunDef _ _ p s) -> do
        putParams p
        checkStatements s
    _ -> throwError $ SError "Sorry, internal semantic analyzer error occoured."
  where
    putParams Nothing = return ()
    putParams (Just p) = modify (parameters:)
      where
        parameters = foldl (\m (Param d (Identifier i)) ->
            M.insert i d m) M.empty p

checkStatements :: [Stat] -> SemanticAnalyzer ()
checkStatements ss = pushNewVarTable >> mapM_ checkStatement ss >> popVarTable
  where
    -- TODO : check for existence of that id in previous tables
    putVar :: [Identifier] -> DataType -> SemanticAnalyzer ()
    putVar is d = do
        varTable <- get
        put (newTopLevelTable d varTable is:tail varTable)
    newTopLevelTable d table =
        foldl (\t (Identifier i) -> M.insert i d t) (head table)

    checkStatement = \case
        Assign (Identifier i) e -> do
            void $ isIdDefined i
            void $ checkExpression e
        VarDef d i ->
            putVar i d
        If e s s' -> do
            void $ checkExpression e
            checkStatements s
            checkStatements s'
        Return (Just e) ->
            void $ checkExpression e
        Return Nothing ->
            return ()
        While e s -> do
            void $ checkExpression e
            checkStatements s
        FuncCall i es -> void $ checkFunctionCall i es

    pushNewVarTable :: SemanticAnalyzer ()
    pushNewVarTable = modify (M.empty:)

    popVarTable :: SemanticAnalyzer ()
    popVarTable = modify tail

isFunctionDefined :: String -> SemanticAnalyzer ()
isFunctionDefined i = do
    ft <- ask
    unless (i `elem` M.keys ft)
      $ throwError $ SError $ "Function '" <> i <> "' is not defined."

isIdDefined :: String -> SemanticAnalyzer DataType
isIdDefined i = do
    varTable <- get
    if i `elem` M.keys (M.unions varTable)
    then return (M.unions varTable M.! i)
    else throwError $ SError $ "Identifier '" <> i <> "' is not defined."

checkExpression :: Exp -> SemanticAnalyzer DataType
checkExpression = \case
    OR e1 e2 -> matchLogical "||" e1 e2
    AND e1 e2 -> matchLogical "||" e1 e2
    Eq e1 e2 -> matchRelation "==" e1 e2
    NonEq e1 e2 -> matchRelation "!=" e1 e2
    Less e1 e2 -> matchRelation "<" e1 e2
    Greater e1 e2 -> matchRelation ">" e1 e2
    LessEq e1 e2 -> matchRelation "<=" e1 e2
    GreaterEq e1 e2 -> matchRelation ">=" e1 e2
    Plus e1 e2 -> matchNumeric "+" e1 e2
    Minus e1 e2 -> matchNumeric "-" e1 e2
    Times e1 e2 -> matchNumeric "*" e1 e2
    Div e1 e2 -> matchNumeric "/" e1 e2
    Mod e1 e2 -> matchNumeric "%" e1 e2
    NOT e -> do
        t <- checkExpression e
        if and [t == DInt] then return DInt
        else throwError $ SError $ cannotMatchMsg' t
    Cast t e -> checkExpression e >> return t
    ConsNum _ -> return DInt
    ConsString _ -> return DString
    ConsChar _ ->  return DChar
    FuncCallExp i es -> checkFunctionCall i es
    IdentifierExp i -> isIdDefined i
  where
    matchLogical op e1 e2 = do
        t1 <- checkExpression e1
        t2 <- checkExpression e2
        if (t1 == DInt) && (t2 == DInt)
        then return DInt
        else throwError $ SError $ cannotMatchLogMsg op t1 t2
    matchRelation op e1 e2 = do
        t1 <- checkExpression e1
        t2 <- checkExpression e2
        if and [t1 == t2] then return DInt
        else throwError $ SError $ cannotMatchRelMsg op t1 t2
    matchNumeric = matchLogical
    cannotMatchRelMsg op t1 t2 =
        "Cannot match '" <> show t1 <> "' with '" <> show t2
            <> "' in the '" <> op <> "' relation expression."
    cannotMatchLogMsg op DInt t =
        "Cannot match '" <> show t <> "' with 'int' in '" <> op
        <> "' expression."
    cannotMatchLogMsg op t DInt = cannotMatchLogMsg op DInt t
    cannotMatchLogMsg op t1 t2 =
        "Cannot match '" <> show t1 <> "' nor '" <> show t2
            <> "' witn 'int' in the '" <> op <> "' numeric expression."
    cannotMatchMsg' t =
        "Cannot match '" <> show t <> "' with int in '!' expression."

checkFunctionCall :: Identifier -> [Exp] -> SemanticAnalyzer DataType
checkFunctionCall (Identifier i) es = do
        isFunctionDefined i
        actualTs <- mapM checkExpression es
        t <- ask
        if actualTs == getParamTypes t i
        then return $ fromMaybe DInt $ fst $ t M.! i
        else throwError $ SError paramsDoNotMatchMsg
  where
    paramsDoNotMatchMsg = "Parameters do not match"
    getParamTypes ts = typeFromParam . snd . (M.!) ts
      where
        typeFromParam = fromMaybe [] . liftM (fmap (\(Param t _) -> t))
