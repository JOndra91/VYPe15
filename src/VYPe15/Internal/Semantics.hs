{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module VYPe15.Internal.Semantics
  where

import Prelude (Bounded(minBound))

import Control.Applicative (pure, (<$>), (<*>))
import Control.Monad
    (mapM, mapM_, return, sequence, unless, void, when, (>>), (>>=))
import Control.Monad.Error.Class (throwError)
import Data.Bool (Bool(False, True), not, otherwise, (&&), (||))
import Data.Either (Either)
import Data.Eq ((/=), (==))
import Data.Foldable (and, foldlM)
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.List (elem, length, zipWith)
import Data.Map as M (empty, fromList, insert, keys, lookup, (!))
import Data.Maybe
    (Maybe(Just, Nothing), fromJust, fromMaybe, isJust, maybe)
import Data.Monoid ((<>))
import Data.String (String)
import Text.Show (show)

import VYPe15.Types.AST
    ( DataType(DChar, DInt, DString)
    , Exp(AND, Cast, ConsChar, ConsNum, ConsString, Div, Eq, FuncCallExp, Greater, GreaterEq, IdentifierExp, Less, LessEq, Minus, Mod, NOT, NonEq, OR, Plus, Times)
    , FunDeclrOrDef(FunDeclr, FunDef)
    , Identifier(Identifier, getId)
    , Param(Param, AnonymousParam)
    , Program
    , Stat(Assign, FuncCall, If, Return, VarDef, While)
    , getParamType
    )
import VYPe15.Types.Semantics
    ( AnalyzerState(AnalyzerState, dataId, functionTable, labelId, programData, returnType, variableId, variableTables)
    , SError(SError)
    , SemanticAnalyzer
    , evalSemAnalyzer
    , getFunc
    , getVars
    , mkVar
    , modifyFunc
    , modifyVars
    , popVars
    , pushVars
    , putReturnType
    , withFunc'
    )
import VYPe15.Types.SymbolTable
    ( Function(Function, functionParams, functionReturn)
    , FunctionState(FuncDeclared, FuncDefined)
    , Variable(Variable, varType)
    , builtInFunctions
    )

semanticAnalysis :: Program -> Either SError [String]
semanticAnalysis ast = evalSemAnalyzer state $ semanticAnalysis' ast
  where
    state = AnalyzerState
            { functionTable = builtInFunctions
            , variableTables = []
            , returnType = Nothing
            , programData = M.empty
            , variableId = minBound
            , dataId = minBound
            , labelId = minBound
            }

semanticAnalysis' :: Program -> SemanticAnalyzer ()
semanticAnalysis' = mapM_ funDeclrOrDef

funDeclrOrDef :: FunDeclrOrDef -> SemanticAnalyzer ()
funDeclrOrDef = \case
    FunDeclr returnType' identifier params ->
        withFunc' (lookup identifier) >>= \case
            Just (Function FuncDeclared _ _) -> throwError $ SError
                $ "Function '" <> getId identifier <> "' is declared twice."
            Just (Function FuncDefined _ _) -> throwError $ SError
                $ "Function '" <> getId identifier <> "' is already defined."
            Nothing ->
                modifyFunc $ M.insert identifier
                  (Function FuncDeclared returnType' params)

    FunDef returnType' identifier params stats ->
        withFunc' (lookup identifier) >>= \case
            Just (Function FuncDefined _ _) -> throwError $ SError
                $ "Function '" <> getId identifier <> "' is already defined."
            Just (Function FuncDeclared returnType'' params')
                | returnType' /= returnType'' || not (params `paramsEq` params')
                    -> throwError $ SError
                        $ "Definition and declaration types of function '"
                          <> getId identifier <> "' differs."
                | otherwise -> handleFunctionDefinition
                  returnType' identifier params stats
            Nothing -> handleFunctionDefinition
              returnType' identifier params stats
  where
    paramsEq p1 p2 = case (p1, p2) of
        (Nothing, Nothing) -> True
        (Just p1', Just p2') -> length p1' == length p2'
                             && and (zipWith paramEq p1' p2')
        _ -> False

    paramEq p1 p2 = getParamType p1 == getParamType p2

    handleFunctionDefinition returnType' identifier params stats = do
        modifyFunc $ M.insert identifier
          (Function FuncDefined returnType' params)
        when (isJust params) $ paramsToMap (fromJust params) >>= pushVars
        putReturnType returnType'
        checkStatements stats

    paramsToMap ps = (M.fromList <$>) . sequence $ fmap paramToVar ps

    paramToVar (Param dt id) = (id,) <$> mkVar dt
    paramToVar AnonymousParam{} = throwError
        $ SError "Unexpected anonymous parameter."

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
    putParams (Just p) = parameters >>= pushVars
      where
        parameters = foldlM
          (\m (Param d i) -> M.insert i <$> mkVar d <*> pure m) M.empty p

checkStatements :: [Stat] -> SemanticAnalyzer ()
checkStatements ss = pushVars M.empty >> mapM_ checkStatement ss >> popVars
  where
    putVar :: Identifier -> DataType -> SemanticAnalyzer ()
    putVar id d = do
        v <- mkVar d
        modifyVars $ withHead $ M.insert id v

    checkStatement = \case
        Assign i e -> do
            void $ findVar i
            void $ checkExpression e
        VarDef d i ->
            mapM_ (`putVar` d) i
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

checkFunctionDecl :: Identifier -> SemanticAnalyzer ()
checkFunctionDecl i = do
    ft <- getFunc
    unless (i `elem` M.keys ft)
      $ throwError $ SError $ "Function '" <> getId i <> "' is not defined."

findVar :: Identifier -> SemanticAnalyzer Variable
findVar i = getVars >>= search
  where
    search (table:tail') = case i `lookup` table of
        Just var -> return var
        Nothing -> search tail'
    search [] = throwError $ SError
        $ "Identifier '" <> getId i <> "' is not defined."

checkExpression :: Exp -> SemanticAnalyzer (Maybe Variable)
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
        if mVarType t == Just DInt
          then mkVarJust DInt
          else throwError $ SError $ cannotMatchMsg' t
    Cast t e -> checkExpression e >> mkVarJust t
    ConsNum _ -> mkVarJust DInt
    ConsString _ -> mkVarJust DString
    ConsChar _ ->  mkVarJust DChar
    FuncCallExp i es -> checkFunctionCall i es
    IdentifierExp i -> Just <$> findVar (Identifier i)
  where
    matchLogical op e1 e2 = do
        t1 <- checkExpression e1
        t2 <- checkExpression e2
        if t1 `hasType` DInt && t2 `hasType` DInt
          then mkVarJust DInt
          else throwError $ SError $ cannotMatchLogMsg op t1 t2

    matchRelation op e1 e2 = do
        t1 <- checkExpression e1
        t2 <- checkExpression e2
        if isJust t1 && mVarType t1 == mVarType t2
          then mkVarJust DInt
          else throwError $ SError $ cannotMatchRelMsg op t1 t2

    matchNumeric = matchLogical

    cannotMatchRelMsg op t1 t2 =
        "Cannot match '" <> varShow t1 <> "' with '" <> varShow t2
            <> "' in the '" <> op <> "' relation expression."

    cannotMatchLogMsg op t1 t2
        | t1 `hasType` DInt = cannotMatchLogMsg op t2 t1
        | otherwise = "Cannot match '" <> varShow t1 <> "' with 'int' in '"
            <> op <> "' expression."

    cannotMatchMsg' t =
        "Cannot match '" <> varShow t <> "' with 'int' in '!' expression."

checkFunctionCall :: Identifier -> [Exp] -> SemanticAnalyzer (Maybe Variable)
checkFunctionCall i es = do
        checkFunctionDecl i
        actualTs <- mapM checkExpression es
        t <- getFunc
        if (mVarType <$> actualTs) == (Just <$> getParamTypes t i)
          then maybe (pure Nothing) mkVarJust . functionReturn $ t M.! i
          else throwError $ SError paramsDoNotMatchMsg
  where
    paramsDoNotMatchMsg = "Parameters do not match"
    getParamTypes ts = typeFromParam . functionParams . (M.!) ts
      where
        typeFromParam = fmap getParamType . fromMaybe []

-- {{{ Utility functions ------------------------------------------------------

withHead :: (a -> a) -> [a] -> [a]
withHead f (h:t) = f h : t
withHead _ [] = []

mkVarJust :: DataType -> SemanticAnalyzer (Maybe Variable)
mkVarJust dt = Just <$> mkVar dt

mVarType :: Maybe Variable -> Maybe DataType
mVarType = fmap varType

varShow :: Maybe Variable -> String
varShow = maybe "void" (show . varType)

hasType :: Maybe Variable -> DataType -> Bool
hasType v t = case v of
    Just (Variable _ t') -> t == t'
    Nothing              -> False

-- }}} Utility functions ------------------------------------------------------
