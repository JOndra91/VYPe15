{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module VYPe15.Internal.Lexer
where

import Data.Bool (Bool(False))
import Data.Monoid ((<>))

import Text.Parsec (alphaNum, char, letter, oneOf, (<|>))
import Text.Parsec.Language (LanguageDef, emptyDef)
import Text.Parsec.Token
    ( GenTokenParser(TokenParser)
    , braces
    , charLiteral
    , commaSep
    , commaSep1
    , commentEnd
    , commentLine
    , commentStart
    , identLetter
    , identStart
    , identifier
    , integer
    , makeTokenParser
    , nestedComments
    , opLetter
    , opStart
    , parens
    , reserved
    , reservedNames
    , reservedOp
    , reservedOpNames
    , semi
    , semiSep
    , stringLiteral
    , whiteSpace
    )

def :: LanguageDef st
def = emptyDef
    { commentStart = "/*"
    , commentEnd = "*/"
    , commentLine = "//"
    , nestedComments = False
    , identStart = letter <|> char '_'
    , identLetter = alphaNum <|> char '_'
    , opStart = oneOf "=+-*/%<>!&|"
    , opLetter = oneOf "=+-*/%<>!&|"
    , reservedOpNames = operators
    , reservedNames = names
    }
  where
    operators = ["=", "+", "-", "*", "/", "%", "<", ">", "<=", ">=", "==", "!=", "&&", "||", "!"]
    names = ["if", "else", "return", "while", "string", "char", "int", "void"] <> vypeReserved
    -- | There are reserved keywords according to project specification (Chapter 3.1)
    vypeReserved = ["break", "continue", "for", "short", "unsigned"]

TokenParser { parens = m_parens
            , braces = m_braces
            , identifier = m_identifier
            , commaSep1 = m_commaSep1
            , commaSep = m_commaSep
            , semi = m_semi
            , reservedOp = m_reservedOp
            , reserved = m_reserved
            , semiSep = m_semiSep
            , whiteSpace = m_whiteSpace
            , integer = m_integer
            , stringLiteral = m_stringLit
            , charLiteral = m_charLit
            } = makeTokenParser def
