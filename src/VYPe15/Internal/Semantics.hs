{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module VYPe15.Internal.Semantics
    ( checkAssignment
    , addBlock
    )
where

import Debug.Trace (traceShowId)

import Control.Monad (return)
import Data.Foldable (foldl)
import Data.Function (($))
import Data.Map as M (lookup, insert, empty)
import Data.Maybe(Maybe(Just,Nothing))
import Data.Vector as V (head ,cons)

import VYPe15.Types.AST
import VYPe15.Types.Parser
import VYPe15.Types.SymbolTable 


checkAssignment :: Identifier -> Exp -> Parser Stat
checkAssignment i'@(Identifier i) e = Parser $ \s (SymbolTable it ft) ->
    case M.lookup i $ V.head (M.empty `V.cons` it) of
        _ -> runParser (return $ Assign i' e) s (SymbolTable (M.empty `V.cons` it) ft)

addBlock :: Parser ()
addBlock = Parser $ \s (SymbolTable it ft) ->
    runParser (return ()) s $ SymbolTable (M.empty `V.cons` it) ft
    


--checkVarDef :: DataType -> [Identifier] -> Parser Stat
--checkVarDef datatype identifiers  = Parser $ \_ (SymbolTable it _) ->
--    let currentTable = head it
--        newMap  = newfoldl (\t i -> M.insert i datatype t) currentTable identifiers
--    in 
            
        
    
    
