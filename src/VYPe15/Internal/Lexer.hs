{-# LANGUAGE NoImplicitPrelude #-}

module VYPe15.Internal.Lexer
where

import Text.Parsec ((<|>), letter, char, alphaNum, oneOf)
import Text.Parsec.Language (emptyDef, LanguageDef)
import Text.Parsec.Token 
    ( commentStart, commentEnd, identStart, identLetter, opStart, opLetter, reservedOpNames, reservedNames, semi
    , makeTokenParser, GenTokenParser(TokenParser)
    , parens, braces, identifier, reservedOp, semiSep, reserved, whiteSpace, integer
    , stringLiteral, charLiteral, commaSep1, commaSep
    )

def :: LanguageDef st
def = emptyDef 
    { commentStart = "/*"
    , commentEnd = "*/" 
    , identStart = letter <|> char '_'
    , identLetter = alphaNum <|> char '_'
    , opStart = oneOf "=+-*/%<>!&|"
    , opLetter = oneOf "=+-*/%<>!&|"
    , reservedOpNames = operators
    , reservedNames = names
    }
  where
    operators = ["=", "+", "-", "*", "/", "%", "<", ">", "<=", ">=", "==", "!=", "&&", "||", "!"] 
    names = ["if", "else", "return", "while", "string", "char", "int", "void"]
                                                                       
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
