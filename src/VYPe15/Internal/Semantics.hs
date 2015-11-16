{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module VYPe15.Internal.Semantics
  where

import Prelude (Bounded(minBound))

import Control.Applicative (pure, (<$>))
import Control.Monad (mapM, mapM_, return, sequence, unless, void, (>>), (>>=))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Writer (tell)
import Data.Bool (Bool(False), not, otherwise, (&&), (||))
import Data.Either (Either)
import Data.Eq ((/=), (==))
import Data.Foldable (and)
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.List (elem, length, zipWith)
import Data.Map as M (empty, fromList, insert, keys, lookup, (!))
import Data.Maybe (Maybe(Just, Nothing), fromJust, isJust, maybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T (pack)
import Text.Show (Show(show))

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
    , LabelId
    , Variable(Variable, varType)
    , builtInFunctions
    , idToText
    )
import VYPe15.Types.TAC
    ( Label(Label')
    , Operator
    , TAC(Begin, End, Goto, JmpZ, Label, PushParam, Void)
    )
import qualified VYPe15.Types.TAC as TAC (TAC(Assign, Return))
import qualified VYPe15.Types.TAC as Const (Constant(Char, Int, String))
import qualified VYPe15.Types.TAC as Op
    ( Operator(Add, And, Call, Const, Div, Eq, GE, GT, LE, LT, Mod, Mul, Neq, Not, Or, Set, Sub)
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
semanticAnalysis' = mapM_ processFunDeclrOrDef

processFunDeclrOrDef :: FunDeclrOrDef -> SemanticAnalyzer ()
processFunDeclrOrDef = \case
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
                | otherwise -> processFunctionDefinition
                  returnType' identifier params stats
            Nothing -> processFunctionDefinition
              returnType' identifier params stats
  where
    paramsEq p1 p2 = length p1 == length p2 && and (zipWith paramEq p1 p2)

    paramEq p1 p2 = getParamType p1 == getParamType p2

    processFunctionDefinition returnType' identifier params stats = do
        modifyFunc $ M.insert identifier
          (Function FuncDefined returnType' params)
        paramsToMap params >>= pushVars
        putReturnType returnType'
        tell [Begin]
        tell [Label $ labelFromId identifier]
        processStatements stats
        tell [End]

    paramsToMap ps = (M.fromList <$>) . sequence $ fmap paramToVar ps

    paramToVar (Param dt id) = (id,) <$> mkVar dt
    paramToVar AnonymousParam{} = throwError
        $ SError "Unexpected anonymous parameter."

processStatements :: [Stat] -> SemanticAnalyzer ()
processStatements ss = pushVars M.empty >> mapM_ processStatement ss >> popVars
  where
    putVar :: Identifier -> DataType -> SemanticAnalyzer ()
    putVar id d = do
        v <- mkVarNamed d $ getId id
        modifyVars $ withHead $ M.insert id v

    processStatement = \case
        Assign i e -> do
            dest <- findVar i
            res <- processExpression e
            unless (isJust res) $ throwError $ SError voidAssign
            void $ dest <= Op.Set (fromJust res)
        VarDef d i ->
            mapM_ (`putVar` d) i
        If e s s' -> do
            ifResult <- processExpression e
            [elseL, endL] <- mkLabels ["IfElse", "IfEnd"]
            unless (mVarType ifResult == Just DInt)
                $ throwError $ SError "TBD"
            tell [JmpZ (fromJust ifResult) elseL]
            processStatements s
            tell [Goto endL]
            tell [Label elseL]
            processStatements s'
            tell [Label endL]
        Return Nothing -> do
            expected <- getReturnType
            unless (expected == Nothing) $ throwError $ SError "TBD"
            tell [TAC.Return Nothing]
        Return (Just e) -> do
            expected <- getReturnType
            actual <- processExpression e
            unless (expected == mVarType actual) $ throwError $ SError "TBD"
            tell [TAC.Return actual]
        While e s -> do
            [whileSL, whileEL] <- mkLabels ["WhileStart", "WhileEnd"]
            tell [Label whileSL]
            whileResult <- processExpression e
            unless (mVarType whileResult == Just DInt)
                $ throwError $ SError "TBD"
            tell [JmpZ (fromJust whileResult) whileEL]
            processStatements s
            tell [Goto whileSL]
            tell [Label whileEL]
        FuncCall i es -> do
            void $ processFunctionCall i es
            tell [Void $ Op.Call $ labelFromId i]

processFunctionDecl :: Identifier -> SemanticAnalyzer ()
processFunctionDecl i = do
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

processExpression :: Exp -> SemanticAnalyzer (Maybe Variable)
processExpression = \case
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
    NOT e -> processExpression e >>= \case
        Just v@(Variable _ DInt) -> DInt *= Op.Not v
        t -> throwError $ SError $ cannotMatchMsg' t
    Cast t e -> processExpression e >>= \case
        Just v -> t *= Op.Set v
        v -> throwError $ SError $ invalidCast v t
    ConsNum i -> DInt *= Op.Const (Const.Int i)
    ConsString s -> DString *= Op.Const (Const.String s)
    ConsChar c ->  DChar *= Op.Const (Const.Char c)
    FuncCallExp i es ->
        processFunctionCall i es >>= \case
          Just t -> t *= Op.Call (labelFromId i)
          Nothing -> throwError $ SError voidAssign

    IdentifierExp i -> Just <$> findVar (Identifier i)
  where
    matchLogical op e1 e2 = do
        t1 <- processExpression e1
        t2 <- processExpression e2
        case (t1, t2) of
            (Just v1@(Variable _ DInt), Just v2@(Variable _ DInt)) ->
                DInt *= op v1 v2
            _ -> throwError $ SError $ cannotMatchLogMsg (op2 op) t1 t2

    matchRelation op e1 e2 = do
        t1 <- processExpression e1
        t2 <- processExpression e2
        if isJust t1 && mVarType t1 == mVarType t2
          then DInt *= op (fromJust t1) (fromJust t2)
          else throwError $ SError $ cannotMatchRelMsg (op2 op) t1 t2

    matchNumeric = matchLogical

    cannotMatchRelMsg op t1 t2 =
        "Cannot match '" <> varShow t1 <> "' with '" <> varShow t2
            <> "' in the '" <> showText op <> "' relation expression."

    cannotMatchLogMsg op t1 t2
        | t1 `hasType` DInt = cannotMatchLogMsg op t2 t1
        | otherwise = "Cannot match '" <> varShow t1 <> "' with 'int' in '"
            <> showText op <> "' expression."

    cannotMatchMsg' t =
        "Cannot match '" <> varShow t <> "' with 'int' in '!' expression."

    invalidCast from to = "Cannot cast from type '" <> varShow from
        <> "' to type'" <> showText to <> "'."

    dummyVar = Variable "$" DInt

    op2 op = op dummyVar dummyVar

    varShow = maybe "void" (showText . varType)

    showText :: Show a => a -> Text
    showText = T.pack . show

processFunctionCall :: Identifier -> [Exp] -> SemanticAnalyzer (Maybe DataType)
processFunctionCall i es = do
        processFunctionDecl i
        actualTs <- mapM processExpression es
        t <- getFunc
        unless ((mVarType <$> actualTs) == (Just <$> getParamTypes t i))
          $ throwError $ SError paramsDoNotMatchMsg
        -- It's safe to use 'fromJust' because function cannot have void type
        -- parameters. Except for functions without parameters but in this
        -- case the list is empty.
        tell (PushParam . fromJust <$> actualTs)
        return $ functionReturn $ t M.! i
  where
    paramsDoNotMatchMsg = "Parameters do not match"
    getParamTypes ts = typeFromParam . functionParams . (M.!) ts
      where
        typeFromParam = fmap getParamType

-- {{{ Variable related functions ---------------------------------------------

mkVarNamed :: DataType -> Text -> SemanticAnalyzer Variable
mkVarNamed dt name =
    (`Variable` dt) . ((name <> "_") <>) . idToText <$> newVarId

mkVar :: DataType -> SemanticAnalyzer Variable
mkVar = (`mkVarNamed` "var")

mkVarJust :: DataType -> SemanticAnalyzer (Maybe Variable)
mkVarJust dt = Just <$> mkVar dt

mVarType :: Maybe Variable -> Maybe DataType
mVarType = fmap varType

hasType :: Maybe Variable -> DataType -> Bool
hasType v t = case v of
    Just (Variable _ t') -> t == t'
    Nothing              -> False

-- }}} Variable related functions ---------------------------------------------
-- {{{ Label related functions ------------------------------------------------

mkLabel :: Text -> SemanticAnalyzer Label
mkLabel s = mkLabel' s <$> newLabelId

mkLabel' :: Text -> LabelId -> Label
mkLabel' s = Label' . ((s <> "_") <>) . idToText

mkLabels :: [Text] -> SemanticAnalyzer [Label]
mkLabels ss = (\id -> (`mkLabel'` id) <$> ss) <$> newLabelId

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

labelFromId :: Identifier -> Label
labelFromId = Label' . getId

-- }}} Utility functions ------------------------------------------------------

voidAssign :: Text
voidAssign = "Cannot assign void to variable."
