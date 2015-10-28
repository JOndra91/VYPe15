{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module VYPe15.Internal.Semantics
where

import Control.Monad (mapM, mapM_, return, unless, void, when, (>>), (>>=))
import Control.Monad.Error.Class (throwError)
import Data.Bool (Bool(False, True), not, otherwise, (&&), (||))
import Data.Either (Either)
import Data.Eq ((/=), (==))
import Data.Foldable (and, foldl)
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.List (elem, head, length, tail, zipWith)
import Data.Map as M (empty, insert, keys, lookup, unions, (!), fromList)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, isJust, fromJust)
import Data.Monoid ((<>))
import Data.String (String)
import Text.Show (show)

import VYPe15.Types.AST
    ( DataType(DChar, DInt, DString)
    , Exp(AND, Cast, ConsChar, ConsNum, ConsString, Div, Eq, FuncCallExp, Greater, GreaterEq, IdentifierExp, Less, LessEq, Minus, Mod, NOT, NonEq, OR, Plus, Times)
    , FunDeclrOrDef(FunDeclr, FunDef)
    , Identifier(Identifier, getId)
    , Param(Param)
    , Program
    , Stat(Assign, FuncCall, If, Return, VarDef, While)
    , getParamType
    )
import VYPe15.Types.Semantics
    ( AnalyzerState(AnalyzerState)
    , SError(SError)
    , SemanticAnalyzer
    , evalSemAnalyzer
    , getFunc
    , getVars
    , modifyFunc
    , modifyVars
    , putVars
    , withFunc'
    , putReturnType
    )
import VYPe15.Types.SymbolTable
    ( Function(Function, functionParams, functionReturn)
    , FunctionState(FuncDeclared, FuncDefined)
    , builtInFunctions
    )

semanticAnalysis :: Program -> Either SError [String]
semanticAnalysis ast = evalSemAnalyzer state $ semanticAnalysis' ast
  where
    state = AnalyzerState builtInFunctions [] Nothing

semanticAnalysis' :: Program -> SemanticAnalyzer ()
semanticAnalysis' = mapM_ funDeclrOrDef

funDeclrOrDef :: FunDeclrOrDef -> SemanticAnalyzer ()
funDeclrOrDef = \case
    FunDeclr returnType identifier params ->
        withFunc' (lookup identifier) >>= \case
            Just (Function FuncDeclared _ _) -> throwError $ SError
                $ "Function '" <> getId identifier <> "' is declared twice."
            Just (Function FuncDefined _ _) -> throwError $ SError
                $ "Function '" <> getId identifier <> "' is already defined."
            Nothing ->
                modifyFunc $ M.insert identifier
                  (Function FuncDeclared returnType params)

    FunDef returnType identifier params stats ->
        withFunc' (lookup identifier) >>= \case
            Just (Function FuncDefined _ _) -> throwError $ SError
                $ "Function '" <> getId identifier <> "' is already defined."
            Just (Function FuncDeclared returnType' params')
                | returnType /= returnType' || not (params `paramsEq` params')
                    -> throwError $ SError
                        $ "Definition and declaration types of function '"
                          <> getId identifier <> "' differs."
                | otherwise -> handleFunctionDefinition
                  returnType identifier params stats
            Nothing -> handleFunctionDefinition
              returnType identifier params stats
  where
    paramsEq p1 p2 = case (p1, p2) of
        (Nothing, Nothing) -> True
        (Just p1', Just p2') -> length p1' == length p2'
                             && and (zipWith paramEq p1' p2')
        _ -> False

    paramEq p1 p2 = getParamType p1 == getParamType p2

    handleFunctionDefinition returnType identifier params stats = do
        modifyFunc $ M.insert identifier
          (Function FuncDefined returnType params)
        when (isJust params) $ modifyVars (paramsToMap (fromJust params):)
        putReturnType returnType
        checkStatements stats

    paramsToMap ps = M.fromList $ fmap (\(Param dt id) -> (id, dt)) ps

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
    putParams (Just p) = modifyVars (parameters:)
      where
        parameters = foldl (\m (Param d i) -> M.insert i d m) M.empty p

checkStatements :: [Stat] -> SemanticAnalyzer ()
checkStatements ss = pushNewVarTable >> mapM_ checkStatement ss >> popVarTable
  where
    -- TODO : check for existence of that id in previous tables
    putVar :: [Identifier] -> DataType -> SemanticAnalyzer ()
    putVar is d = do
        varTable <- getVars
        putVars (newTopLevelTable d varTable is:tail varTable)
    newTopLevelTable d table =
        foldl (\t i -> M.insert i d t) (head table)

    checkStatement = \case
        Assign i e -> do
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
    pushNewVarTable = modifyVars (M.empty:)

    popVarTable :: SemanticAnalyzer ()
    popVarTable = modifyVars tail

isFunctionDefined :: Identifier -> SemanticAnalyzer ()
isFunctionDefined i = do
    ft <- getFunc
    unless (i `elem` M.keys ft)
      $ throwError $ SError $ "Function '" <> getId i <> "' is not defined."

isIdDefined :: Identifier -> SemanticAnalyzer DataType
isIdDefined i = do
    varTable <- getVars
    if i `elem` M.keys (M.unions varTable)
    then return (M.unions varTable M.! i)
    else throwError $ SError $ "Identifier '" <> getId i <> "' is not defined."

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
    IdentifierExp i -> isIdDefined $ Identifier i
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
checkFunctionCall i es = do
        isFunctionDefined i
        actualTs <- mapM checkExpression es
        t <- getFunc
        if actualTs == getParamTypes t i
        then return . fromMaybe DInt . functionReturn $ t M.! i
        else throwError $ SError paramsDoNotMatchMsg
  where
    paramsDoNotMatchMsg = "Parameters do not match"
    getParamTypes ts = typeFromParam . functionParams . (M.!) ts
      where
        typeFromParam = fmap getParamType . fromMaybe []
