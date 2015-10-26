{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module VYPe15.Internal.Semantics
where

import Control.Monad (return, mapM_, mapM,(>>), fail, void, liftM)
import Control.Monad.Except (runExceptT)
import Control.Monad.State.Class(put, modify, get)
import Control.Monad.Reader.Class(ask)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.State (evalState)
import Data.Bool(Bool(True,False))
import Data.Either (Either)
import Data.Eq ((==))
import Data.Foldable (foldl, and)
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.List (filter, elem, head, tail)
import Data.Maybe(Maybe(Just,Nothing), fromMaybe)
import Data.Map as M (insert, empty, unions, keys, (!))
import Data.Monoid ((<>))
import Data.String (String)
import Data.Tuple (snd, fst)
import Text.Show(show)

--import Debug.Trace(traceShowId)

import VYPe15.Types.AST
import VYPe15.Types.Semantics(SemanticAnalyzer(runSemAnalyzer), SError)
import VYPe15.Types.SymbolTable (builtInFunctions, FunctionTable)

semanticAnalysis :: Program -> Either SError ()
semanticAnalysis ast = 
    evalState (runReaderT (runExceptT $ runSemAnalyzer $ semanticAnalysis' ast) mkFunctionTable) [] 
  where
    mkFunctionTable = foldl mkFunctionTable' builtInFunctions ast 
    mkFunctionTable' table = \case
        FunDef rt (Identifier i) p _ -> M.insert i (rt, p) table
        _ -> table

semanticAnalysis' :: Program -> SemanticAnalyzer ()
semanticAnalysis' = mapM_ checkFunctionDef . filter isFunctionDef
  where
    isFunctionDef (FunDef _ _ _ _) = True
    isFunctionDef _ = False

checkFunctionDef 
    :: FunDeclrOrDef
    -> SemanticAnalyzer ()
checkFunctionDef = \case
    (FunDef _ _ p s) -> do
        putParams p
        checkStatements s
    _ -> fail "Sorry, internal semantic analyzer error occoured."
  where 
    putParams Nothing = return ()
    putParams (Just p) = modify (parameters:)
      where
        parameters = foldl (\m (Param d (Identifier i)) -> M.insert i d m) M.empty p

checkStatements :: [Stat] -> SemanticAnalyzer ()
checkStatements ss = pushNewVarTable >> mapM_ checkStatement ss >> popVarTable
  where
    -- TODO : check for existence of that id in previous tables
    putVar :: [Identifier] -> DataType -> SemanticAnalyzer ()
    putVar is d = do
        varTable <- get
        let newTopLevelTable = foldl (\t (Identifier i) -> M.insert i d t) (head varTable) is
        put (newTopLevelTable:tail varTable)

    checkStatement = \case 
        Assign (Identifier i) e -> do
            void $ isIdDefined i
            void $ checkExpression e
        VarDef d i -> do
            putVar i d
        If e s s' -> do 
            void $ checkExpression e
            checkStatements s
            checkStatements s'
        Return (Just e) -> do
            void $ checkExpression e
        Return Nothing -> do
            return ()
        While e s -> do
            void $ checkExpression e
            checkStatements s
        FuncCall i es -> void $ checkFunctionCall i es

    pushNewVarTable :: SemanticAnalyzer ()
    pushNewVarTable = modify (M.empty:)

    popVarTable :: SemanticAnalyzer ()
    popVarTable = modify (tail)

isFunctionDefined :: String -> SemanticAnalyzer ()
isFunctionDefined i = do
    ft <- ask
    if i `elem` M.keys ft
    then return ()
    else fail $ "Function '" <> i <> "' is not defined." 

isIdDefined :: String -> SemanticAnalyzer DataType
isIdDefined i = do
    varTable <- get
    if i `elem` (M.keys $ M.unions varTable)
    then return (M.unions varTable M.! i)
    else fail $ "Identifier '" <> i <> "' is not defined."
         
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
        else fail $ cannotMatchMsg' t
    Cast t e -> checkExpression e >> return t
    ConsNum _ -> return DInt
    ConsString _ -> return DString
    ConsChar _ ->  return DChar
    Bracket e -> checkExpression e
    FuncCallExp i es -> checkFunctionCall i es
    IdentifierExp i -> isIdDefined i
  where
    matchLogical op e1 e2 = do 
        t1 <- checkExpression e1 
        t2 <- checkExpression e2
        if and [t1 == DInt, t2 == DInt] then return DInt
        else fail $ cannotMatchLogMsg op t1 t2
    matchRelation op e1 e2 = do
        t1 <- checkExpression e1 
        t2 <- checkExpression e2
        if and [t1 == t2] then return DInt
        else fail $ cannotMatchRelMsg op t1 t2
    matchNumeric = matchLogical
    cannotMatchRelMsg op t1 t2 = 
        "Cannot match '" <> show t1 <> "' with '" <> show t2 
            <> "' in the '" <> op <> "' relation expression."
    cannotMatchLogMsg op DInt t = 
        "Cannot match '" <> show t <> "' with 'int' in '" <> op <> "' expression."
    cannotMatchLogMsg op t DInt = cannotMatchLogMsg op DInt t
    cannotMatchLogMsg op t1 t2 =
        "Cannot match '" <> show t1 <> "' nor '" <> show t2 
            <> "' witn 'int' in the '" <> op <> "' numeric expression."
    cannotMatchMsg' t = "Cannot match '" <> show t <> "' with int in '!' expression."

checkFunctionCall :: Identifier -> [Exp] -> SemanticAnalyzer DataType
checkFunctionCall (Identifier i) es = do
        isFunctionDefined i
        actualTs <- mapM checkExpression es
        t <- ask
        if actualTs == getParamTypes t i
        then return $ fromMaybe DInt $ fst $ t M.! i
        else fail $ paramsDoNotMatchMsg
  where
    paramsDoNotMatchMsg = "Parameters do not match"
    getParamTypes ts = typeFromParam . snd . (M.!) ts
      where
        typeFromParam = fromMaybe [] . liftM (fmap (\(Param t _) -> t))
