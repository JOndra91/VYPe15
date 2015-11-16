{-# LANGUAGE NoImplicitPrelude #-}

module VYPe15.Internal.Parser
    (parseVYPe15)
where

import Prelude (fromInteger)

import Control.Applicative ((<$>), (<*), (<*>))
import Control.Monad (return, (>>))
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.Functor.Identity (Identity)
import Data.Maybe (Maybe(Just, Nothing))
import Data.String (String, fromString)

import Data.Default (Default(def))
import Text.Parsec (ParsecT, eof, many, try, (<?>), (<|>))
import Text.Parsec.Expr
    ( Assoc(AssocLeft)
    , Operator(Infix, Prefix)
    , OperatorTable
    , buildExpressionParser
    )
import Text.Parsec.String (Parser)

import VYPe15.Internal.Lexer
    ( m_braces
    , m_charLit
    , m_commaSep
    , m_commaSep1
    , m_identifier
    , m_integer
    , m_parens
    , m_reserved
    , m_reservedOp
    , m_semi
    , m_stringLit
    )
import VYPe15.Types.AST
    ( DataType(DChar, DInt, DString)
    , Exp(AND, Cast, ConsChar, ConsNum, ConsString, Div, Eq, FuncCallExp, Greater, GreaterEq, IdentifierExp, Less, LessEq, Minus, Mod, NOT, NonEq, OR, Plus, Times)
    , FunDeclrOrDef(FunDeclr, FunDef)
    , Param(AnonymousParam, Param)
    , Program
    , Stat(Assign, FuncCall, If, Return, VarDef, While)
    )

exprparser :: Parser Exp
exprparser = buildExpressionParser table term <?> "expression"

table :: OperatorTable String u Identity Exp
table = [
          [ Infix (m_reservedOp "||" >> return OR) AssocLeft ]
        , [ Infix (m_reservedOp "&&" >> return AND) AssocLeft ]
        , [ Infix (m_reservedOp "==" >> return Eq) AssocLeft
          , Infix (m_reservedOp "!=" >> return NonEq) AssocLeft
          ]
        , [ Infix (m_reservedOp "<" >> return Less) AssocLeft
          , Infix (m_reservedOp ">" >> return Greater) AssocLeft
          , Infix (m_reservedOp "<=" >> return LessEq) AssocLeft
          , Infix (m_reservedOp ">=" >> return GreaterEq) AssocLeft
          ]
        , [ Infix (m_reservedOp "*" >> return Times) AssocLeft
          , Infix (m_reservedOp "/" >> return Div) AssocLeft
          , Infix (m_reservedOp "%" >> return Mod) AssocLeft
          ]
        , [ Infix (m_reservedOp "+" >> return Plus) AssocLeft
          , Infix (m_reservedOp "-" >> return Minus) AssocLeft
          ]
        , [ Prefix (m_reservedOp "!" >> return NOT) ]
        , [ Prefix (Cast <$> m_parens dataTypeParser) ]
        ]

term :: Parser Exp
term = funcCall <|> m_parens exprparser
    <|> IdentifierExp . fromString <$> m_identifier
    <|> ConsChar <$> m_charLit
    <|> ConsString . fromString <$> m_stringLit
    <|> ConsNum . fromInteger <$> m_integer
  where
    funcCall = try $ FuncCallExp
        <$> fmap fromString m_identifier
        <*> m_parens (many exprparser)

statparser :: Parser [Stat]
statparser = many statement
  where
    statement =
        try assignStatement
        <|> whileStatement
        <|> ifStatement
        <|> returnStatement
        <|> funCallStatement
        <|> varDefStatement
      where
        assignStatement = Assign
            <$> fmap fromString m_identifier
            <*  m_reservedOp "="
            <*> exprparser
            <*  m_semi

        whileStatement = m_reserved "while" >> While
            <$> m_parens exprparser
            <*> m_braces statparser

        ifStatement = m_reserved "if" >> If
            <$> m_parens exprparser
            <*> m_braces statparser
            <*  m_reserved "else"
            <*> m_braces statparser

        returnStatement = m_reserved "return" >> Return
            <$> (fmap Just exprparser <|> return Nothing)
            <*  m_semi

        funCallStatement = FuncCall
            <$> fmap fromString m_identifier
            <*> m_parens (m_commaSep exprparser)
            <*  m_semi

        varDefStatement = VarDef
            <$> dataTypeParser
            <*> m_commaSep (fmap fromString m_identifier)
            <*  m_semi

dataTypeParser :: ParsecT String u Identity DataType
dataTypeParser = return DInt <* m_reserved "int"
             <|> return DChar <* m_reserved "char"
             <|> return DString <* m_reserved "string"

typeParamsParser :: Parser [Param]
typeParamsParser = m_commaSep1 (AnonymousParam <$> dataTypeParser)
               <|> voidParser

paramParser :: Parser Param
paramParser = Param <$> dataTypeParser <*> fmap fromString m_identifier

paramsParser :: Parser [Param]
paramsParser = m_commaSep1 paramParser <|> voidParser

voidParser :: Default a => Parser a
voidParser = return def <* m_reserved "void"

programParser :: Parser Program
programParser = many (try parseFunDeclr <|> parseFunDef)
  where
    returnTypeParser = Just <$> dataTypeParser <|> voidParser
    parseFunDeclr = FunDeclr
        <$> returnTypeParser
        <*> fmap fromString m_identifier
        <*> m_parens typeParamsParser
        <*  m_semi

    parseFunDef = FunDef
        <$> returnTypeParser
        <*> fmap fromString m_identifier
        <*> m_parens paramsParser
        <*> m_braces statparser

parseVYPe15 :: Parser Program
parseVYPe15 = programParser <* eof
