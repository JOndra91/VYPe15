{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module VYPe15.Internal.Semantics
  where

import Prelude (Bounded(minBound))

import Control.Applicative (pure, (<$>))
import Control.Monad
    (mapM, mapM_, return, sequence, unless, void, when, (>>), (>>=))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Writer (tell)
import Data.Bool (Bool(False, True), not, otherwise, (&&), (||))
import Data.Either (Either)
import Data.Eq ((/=), (==))
import Data.Foldable (and)
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.List (elem, length, zipWith)
import Data.Map as M (empty, fromList, insert, keys, lookup, (!))
import Data.Maybe (Maybe(Just, Nothing), fromJust, fromMaybe, isJust, maybe)
import Data.Monoid ((<>))
import Data.String (IsString(fromString), String)
import Text.Show (show)

import VYPe15.Types.AST
    ( DataType(DChar, DInt, DString)
    , Exp(AND, Cast, ConsChar, ConsNum, ConsString, Div, Eq, FuncCallExp, Greater, GreaterEq, IdentifierExp, Less, LessEq, Minus, Mod, NOT, NonEq, OR, Plus, Times)
    , FunDeclrOrDef(FunDeclr, FunDef)
    , Identifier(Identifier, getId)
    , Param(AnonymousParam, Param)
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
    , getReturnType
    , getVars
    , modifyFunc
    , modifyVars
    , newLabelId
    , newVarId
    , popVars
    , pushVars
    , putReturnType
    , withFunc'
    )
import VYPe15.Types.SymbolTable
    ( Function(Function, functionParams, functionReturn)
    , FunctionState(FuncDeclared, FuncDefined)
    , Id(Id, idWord)
    , Variable(Variable, varType)
    , builtInFunctions
    )
import VYPe15.Types.TAC
    (Label(Label'), Operator, TAC(Label, Begin, End, JmpZ, Goto))
import qualified VYPe15.Types.TAC as TAC (TAC(Assign, Return))
import qualified VYPe15.Types.TAC as Const (Constant(Char, Int, String))
import qualified VYPe15.Types.TAC as Op
    ( Operator(Add, And, Const, Div, Eq, GE, GT, LE, LT, Mod, Mul, Neq, Not, Or, Set, Sub)
    )

semanticAnalysis :: Program -> Either SError [TAC]
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
        tell [Begin]
        tell [Label (Label' . fromString $ getId identifier)]
        checkStatements stats
        tell [End]

    paramsToMap ps = (M.fromList <$>) . sequence $ fmap paramToVar ps

    paramToVar (Param dt id) = (id,) <$> mkVar dt
    paramToVar AnonymousParam{} = throwError
        $ SError "Unexpected anonymous parameter."

checkStatements :: [Stat] -> SemanticAnalyzer ()
checkStatements ss = pushVars M.empty >> mapM_ checkStatement ss >> popVars
  where
    putVar :: Identifier -> DataType -> SemanticAnalyzer ()
    putVar id d = do
        v <- mkVar d
        modifyVars $ withHead $ M.insert id v

    undefVariable i = "Variable '" <> show i <> "'is not defined."
    voidAssign i = "Cannot assign void to variable '" <> show i <> "'."

    checkStatement = \case
        Assign i e -> do
            dest <- findVar i
            res <- checkExpression e
            unless (isJust res) $ throwError $ SError $ voidAssign i
            void $ dest <= Op.Set (fromJust res)
        VarDef d i ->
            mapM_ (`putVar` d) i
        If e s s' -> do
            ifResult <- checkExpression e
            elseL <- mkLabel "IfElse"
            endL <- mkLabel "IfEnd"
            unless (mVarType ifResult == Just DInt)
                $ throwError $ SError "TBD"
            tell [JmpZ (fromJust ifResult) elseL]
            checkStatements s
            tell [Goto endL]
            tell [Label elseL]
            checkStatements s'
            tell [Label endL]
        Return Nothing -> do
            expected <- getReturnType
            unless (expected == Nothing) $ throwError $ SError "TBD"
            tell [TAC.Return Nothing]
        Return (Just e) -> do
            expected <- getReturnType
            actual <- checkExpression e
            unless (expected == (mVarType actual)) $ throwError $ SError "TBD"
            tell [TAC.Return actual]
        While e s -> do
            whileSL <- mkLabel "WhileStart"
            whileEL <- mkLabel "WhileEnd"
            tell [Label whileSL]
            whileResult <- checkExpression e
            unless (mVarType whileResult == Just DInt)
                $ throwError $ SError "TBD"
            tell [JmpZ (fromJust whileResult) whileEL]
            checkStatements s
            tell [Goto whileSL]
            tell [Label whileEL]
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
    OR e1 e2 -> matchLogical Op.Or e1 e2
    AND e1 e2 -> matchLogical Op.And e1 e2
    Eq e1 e2 -> matchRelation Op.Eq e1 e2
    NonEq e1 e2 -> matchRelation Op.Neq e1 e2
    Less e1 e2 -> matchRelation Op.LT e1 e2
    Greater e1 e2 -> matchRelation Op.GT e1 e2
    LessEq e1 e2 -> matchRelation Op.LE e1 e2
    GreaterEq e1 e2 -> matchRelation Op.GE e1 e2
    Plus e1 e2 -> matchNumeric Op.Add e1 e2
    Minus e1 e2 -> matchNumeric Op.Sub e1 e2
    Times e1 e2 -> matchNumeric Op.Mul e1 e2
    Div e1 e2 -> matchNumeric Op.Div e1 e2
    Mod e1 e2 -> matchNumeric Op.Mod e1 e2
    NOT e -> checkExpression e >>= \case
        Just v@(Variable _ DInt) -> DInt *= Op.Not v
        t -> throwError $ SError $ cannotMatchMsg' t
    Cast t e -> checkExpression e >>= \case
        Just v -> t *= Op.Set v
        v -> throwError $ SError $ invalidCast v t
    ConsNum i -> DInt *= Op.Const (Const.Int i)
    ConsString s -> DString *= Op.Const (Const.String $ fromString s)
    ConsChar c ->  DChar *= Op.Const (Const.Char c)
    FuncCallExp i es -> checkFunctionCall i es
    IdentifierExp i -> Just <$> findVar (Identifier i)
  where
    matchLogical op e1 e2 = do
        t1 <- checkExpression e1
        t2 <- checkExpression e2
        case (t1, t2) of
            (Just v1@(Variable _ DInt), Just v2@(Variable _ DInt)) ->
                DInt *= op v1 v2
            _ -> throwError $ SError $ cannotMatchLogMsg (op2 op) t1 t2

    matchRelation op e1 e2 = do
        t1 <- checkExpression e1
        t2 <- checkExpression e2
        if isJust t1 && mVarType t1 == mVarType t2
          then DInt *= op (fromJust t1) (fromJust t2)
          else throwError $ SError $ cannotMatchRelMsg (op2 op) t1 t2

    matchNumeric = matchLogical

    cannotMatchRelMsg op t1 t2 =
        "Cannot match '" <> varShow t1 <> "' with '" <> varShow t2
            <> "' in the '" <> show op <> "' relation expression."

    cannotMatchLogMsg op t1 t2
        | t1 `hasType` DInt = cannotMatchLogMsg op t2 t1
        | otherwise = "Cannot match '" <> varShow t1 <> "' with 'int' in '"
            <> show op <> "' expression."

    cannotMatchMsg' t =
        "Cannot match '" <> varShow t <> "' with 'int' in '!' expression."

    invalidCast :: Maybe Variable -> DataType -> String
    invalidCast from to = "Cannot cast from type '" <> varShow from
        <> "' to type'" <> show to <> "'."

    dummyVar = Variable (Id 0) DInt

    op2 op = op dummyVar dummyVar

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

-- {{{ Variable related functions ---------------------------------------------

mkVar :: DataType -> SemanticAnalyzer Variable
mkVar dt = (`Variable` dt) <$> newVarId

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

-- }}} Variable related functions ---------------------------------------------
-- {{{ Label related functions ------------------------------------------------

mkLabel :: String -> SemanticAnalyzer Label
mkLabel s = Label' . fromString . ((s <> "_") <>) . (show . idWord) <$> newLabelId

mkLabel' :: SemanticAnalyzer Label
mkLabel' = mkLabel "label"

-- }}} Label related functions ------------------------------------------------
-- {{{ Three address code related functions -----------------------------------

tellAssign :: Variable -> Operator -> SemanticAnalyzer ()
tellAssign dest op = tell [TAC.Assign dest op]

infixr 5 <=

-- | Writes the operation to monad writer with destination to given variable.
(<=) :: Variable -> Operator -> SemanticAnalyzer (Maybe Variable)
var <= op = tellAssign var op >> pure (Just var)

-- | Writes the operation to new variable with given type.
(*=) :: DataType
    -> Operator
    -> SemanticAnalyzer (Maybe Variable)
dt *= op = mkVar dt >>= (<= op)

-- }}} Three address code related functions -----------------------------------
-- {{{ Utility functions ------------------------------------------------------

withHead :: (a -> a) -> [a] -> [a]
withHead f (h:t) = f h : t
withHead _ [] = []

-- }}} Utility functions ------------------------------------------------------
