{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
module Test.Parser where

import Data.Map as M
import Data.List as L
import Data.Set as S
import Data.Char
import Debug.Trace
import Control.Monad.Error hiding (Error,join)

import           Text.ParserCombinators.UU hiding (parse, join)
import qualified Text.ParserCombinators.UU as UU
import           Text.ParserCombinators.UU.Utils
import           Text.ParserCombinators.UU.BasicInstances hiding (Error)

import CCO.Feedback hiding (trace, Error)
import CCO.Printing as P hiding (join)

import PHP.IR hiding (Null, Double, Bool, Integer)
import Analysis.Lattice.AbstractValue
import Analysis.Lattice.ValueMap
import Analysis.ProgramState
import Analysis.Resource
import Framework.Lattice
import Analysis.Type as Ty

pLine :: Parser (Label, Var, Ty.Type)
pLine = (,,) <$> pLabel <* pSymbol ":" <*> pVariable <* pSymbol ":" <*> pType

pLabel :: Parser Label
pLabel = pInteger

pVariable :: Parser Var
pVariable = pMany (pLetter <|> pDigit <|> pSym '_')

pResource :: Parser Resource
pResource = BZ          <$ pSymbol "bz"
        <|> CURL        <$ pSymbol "curl"
        <|> CURLMulti   <$ pSymbol "curlmulti"
        <|> DBA         <$ pSymbol "dba"
        <|> FTP         <$ pSymbol "ftp"
        <|> GD          <$ pSymbol "gd"
        <|> GDFont      <$ pSymbol "gdfont"
        <|> IMAP        <$ pSymbol "imap"
        <|> File        <$ pSymbol "file"
        <|> ProcessFile <$ pSymbol "processfile"
        <|> Directory   <$ pSymbol "directory"
        <|> Context     <$ pSymbol "context"
        <|> Socket      <$ pSymbol "socket"
        <|> Process     <$ pSymbol "process"
        <|> Zip         <$ pSymbol "zip"
        <|> ZipEntry    <$ pSymbol "zipentry"
        <|> File        <$ pSymbol "stream"

pType :: Parser Ty.Type
pType = const String                 <$> pSymbol "string"
    <|> const Integer                <$> pSymbol "integer"
    <|> const Double                 <$> pSymbol "double"
    <|> const Bool                   <$> pSymbol "boolean"
    <|>       Object                 <$> pSymbol "array"
    <|> const Null                   <$> pSymbol "NULL"
    <|>       Resource               <$  pSymbol "resource" <*> pBrackets pResource
    <|>       Object . L.map toLower <$  pSymbol "object" <*> pBrackets (pMany (pLetter <|> pDigit <|> pSym '_'))
      

parseLine :: MonadError [Char] m => String -> m (Label, Var, Ty.Type)     
parseLine inp = do 
    let (a, errors) = UU.parse ((,) <$> pLine <*> pEnd) (createStr (LineColPos 0 0 0) inp)
    case L.null errors of
        True  -> return a 
        False -> throwError . render_ 79 . above . L.map showable $ errors


parseFile :: MonadError [Char] m => String -> m [(Label, Var, Ty.Type)]
parseFile = mapM parseLine . lines    


parseFileAndCollapse :: MonadError [Char] m => Maybe String -> m (Map Label (Set Ty.Type))
parseFileAndCollapse Nothing    = return M.empty
parseFileAndCollapse (Just inp) = do 
    parsed <- parseFile inp
    return . M.fromListWith S.union . L.map (\(l,v,t) -> (l,S.singleton t)) $ parsed