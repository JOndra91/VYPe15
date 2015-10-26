{-# LANGUAGE NoImplicitPrelude #-}

module VYPe15.Internal.Parser 
    (parseVYPe15)
where

import Control.Applicative((<*), (<$>), (<*>))
import Control.Monad(return, (>>))
import Data.Function (($))
import Data.Functor (fmap)
import Data.Functor.Identity (Identity)
import Data.Maybe (Maybe(Just,Nothing))
import Data.String (String)

import Text.Parsec ((<?>), eof, (<|>), many, try, ParsecT)
import Text.Parsec.Expr (buildExpressionParser, Operator(Infix, Prefix), Assoc(AssocLeft), OperatorTable)
import Text.Parsec.String (Parser)

import VYPe15.Internal.Lexer    
    ( m_parens                                        
    , m_braces
    , m_identifier                                
    , m_commaSep1
    , m_commaSep
    , m_semi
    , m_reservedOp                                
    , m_reserved                                    
    , m_integer                                       
    , m_stringLit                               
    , m_charLit
    )
import VYPe15.Types.AST (Exp(Plus, Minus, Times, Div, Mod, Less, Greater, LessEq, GreaterEq, Eq, NonEq, AND, OR
    , IdentifierExp, ConsChar, ConsString, ConsNum, NOT, Cast, FuncCallExp)
    , Stat(Assign, While, If, Return, FuncCall, VarDef)
    , Identifier(Identifier)
    , Program, FunDeclrOrDef(FunDeclr, FunDef)
    , DataType(DInt, DChar, DString)
    , Param(Param)
    )

exprparser :: Parser Exp
exprparser = buildExpressionParser table term <?> "expression"

table :: OperatorTable String u Identity Exp
table = [ 
          [ Infix (m_reservedOp "||" >> return (OR)) AssocLeft ]
        , [ Infix (m_reservedOp "&&" >> return (AND)) AssocLeft ]
        , [ Infix (m_reservedOp "==" >> return (Eq)) AssocLeft
          , Infix (m_reservedOp "!=" >> return (NonEq)) AssocLeft
          ]
        , [ Infix (m_reservedOp "<" >> return (Less)) AssocLeft
          , Infix (m_reservedOp ">" >> return (Greater)) AssocLeft
          , Infix (m_reservedOp "<=" >> return (LessEq)) AssocLeft
          , Infix (m_reservedOp ">=" >> return (GreaterEq)) AssocLeft
          ]
        , [ Infix (m_reservedOp "*" >> return (Times)) AssocLeft
          , Infix (m_reservedOp "/" >> return (Div)) AssocLeft
          , Infix (m_reservedOp "%" >> return (Mod)) AssocLeft
          ]
        , [ Infix (m_reservedOp "+" >> return (Plus)) AssocLeft
          , Infix (m_reservedOp "-" >> return (Minus)) AssocLeft
          ]
        , [ Prefix (m_reservedOp "!" >> return (NOT)) ]
        , [ Prefix (Cast <$> m_parens dataTypeParser) ]
        ]

term :: Parser Exp
term = funcCall <|> m_parens exprparser 
    <|> IdentifierExp <$> m_identifier
    <|> ConsChar <$> m_charLit
    <|> ConsString <$> m_stringLit
    <|> ConsNum <$> m_integer
  where
    funcCall = try $ FuncCallExp 
        <$> fmap Identifier m_identifier
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
            <$> fmap Identifier m_identifier 
            <*  m_reservedOp "="
            <*> exprparser
            <* m_semi
        
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
            <* m_semi

        funCallStatement = FuncCall 
            <$> fmap Identifier m_identifier 
            <*> (m_parens $ m_commaSep exprparser)
            <* m_semi
    
        varDefStatement = VarDef
            <$> dataTypeParser
            <*> (m_commaSep $ fmap Identifier m_identifier)
            <* m_semi
            
dataTypeParser :: ParsecT String u Identity DataType
dataTypeParser = return DInt <* m_reserved "int" 
                <|> return DChar <* m_reserved "char"
                <|> return DString <* m_reserved "string"

typeParamsParser :: Parser (Maybe [DataType])
typeParamsParser = Just <$> m_commaSep1 dataTypeParser <|> voidParser

paramParser :: Parser Param
paramParser = Param <$> dataTypeParser <*> fmap Identifier m_identifier

paramsParser :: Parser (Maybe [Param])
paramsParser = (Just <$> m_commaSep1 paramParser) <|> voidParser

voidParser :: Parser (Maybe a)
voidParser = return Nothing <* m_reserved "void"

programParser :: Parser Program
programParser = many (try parseFunDeclr <|> parseFunDef)
  where
    returnTypeParser = Just <$> dataTypeParser <|> voidParser
    parseFunDeclr = FunDeclr
        <$> returnTypeParser
        <*> fmap Identifier m_identifier
        <*> m_parens typeParamsParser
        <* m_semi

    parseFunDef = FunDef
        <$>returnTypeParser
        <*> fmap Identifier m_identifier
        <*> m_parens paramsParser
        <*> m_braces statparser

parseVYPe15 :: Parser Program
parseVYPe15 = programParser <* eof
