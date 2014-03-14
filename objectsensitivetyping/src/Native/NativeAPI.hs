{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
module Native.NativeAPI where

import Control.Applicative
import Data.Char
import Data.Map as M hiding (union)
import Data.List as L hiding (length,union)
import Data.Set as S hiding (union)
import qualified Data.Set as S
import System.IO
import System.Exit

import           Text.ParserCombinators.UU hiding (parse, join)
import qualified Text.ParserCombinators.UU as UU
import           Text.ParserCombinators.UU.Utils
import           Text.ParserCombinators.UU.BasicInstances hiding (Error)
import           Analysis.Resource

import PHP.IR hiding (functions, classes, )
        
                             
data NativeAPI = NativeAPI { 
      constants :: Map String NativeTerminalType
    , functions :: Map String (NativeType NativeTypeSet) -- (FType TypeSet)
    , classes   :: Map Type   NativeClassDecl
} deriving (Show)


-- specializes an NativeType based on the number of arguments given 
specialize :: Int -> NativeType a -> Maybe (NativeType a)
specialize n = L.lookup (n + 1) . L.map (\ftype -> (typeLength ftype, ftype)) . flatten True
    where
        flatten :: Bool -> NativeType a -> [NativeType a]
        flatten b (TFun output False l r) = do 
            r' <- flatten b r
            return $ TFun output False l r'
        flatten b (TFun output True l r) = if b
            then do r' <- flatten True r
                    [r', TFun output False l r']
            else flatten False r
        flatten b other = return other

typeLength :: NativeType a -> Int
typeLength (TValue _)     = 1
typeLength (TFun _ _ l r) = typeLength l + typeLength r


constantByName :: NativeAPI -> String -> Maybe NativeTerminalType
constantByName = flip M.lookup . constants

functionByName :: NativeAPI -> String -> Maybe (NativeType NativeTypeSet)
functionByName = flip M.lookup . functions

isNativeConstant :: NativeAPI -> String -> Bool
isNativeConstant = flip M.member . constants

isNativeFunction :: NativeAPI -> String -> Bool
isNativeFunction = flip M.member . functions

isNativeConstructor :: NativeAPI -> String -> Bool
isNativeConstructor = flip M.member . classes

isNativeClassConstant :: NativeAPI -> String -> String -> Bool
isNativeClassConstant api ty name = maybe False (const True) (getNativeClassConstant api ty name)
        
getNativeClassConstant :: NativeAPI -> String -> String -> Maybe NativeTerminalType        
getNativeClassConstant api ty name  = do NativeClassDecl _ _ _ constants _ _ <- M.lookup ty . classes $ api    
                                         L.lookup name . L.map (\(NativeClassConstantDecl name ty) -> (name,ty)) $ constants 
  
returnTypesByFunction :: NativeAPI -> String -> S.Set String
returnTypesByFunction api name = case M.lookup name $ functions api of
    Nothing         -> error $ "Unable to find return type for: " ++ show name
    Just nativeType -> returnTypes nativeType



pNativeAPI :: Parser NativeAPI
pNativeAPI = build <$  pBrackets (pSymbol "constants") 
                   <*> pMany pConstant 
                   <*  pBrackets (pSymbol "functions") 
                   <*> pMany pFunction
                   <*  pBrackets (pSymbol "classes")
                   <*> pMany pClass
    where
        build xs ys zs = NativeAPI { 
            constants = M.fromList xs, 
            functions = M.fromList ys,
            classes   = M.fromList zs
        }
        
pFunction :: Parser (String, NativeType NativeTypeSet)
pFunction = (,) <$> pFunctionName <* pSymbol "::" <*> pType

pConstant :: Parser (String, NativeTerminalType)
pConstant = (,) <$> pConstantName <* pSymbol "::" <*> pTerminalType

pIdentifier :: Parser String
pIdentifier = (:) <$> (pSym '_' <|> pLetter) <*> pMany (pLetter <|> pDigit <|> pSym '_')

pConstantName :: Parser String
pConstantName = lexeme pIdentifier

pFunctionName :: Parser String
pFunctionName = lexeme pIdentifier

pClassName :: Parser String
pClassName = lexeme pIdentifier

pType :: Parser (NativeType NativeTypeSet)
pType = build <$> pValue <*> pArrowKind <*> pType
    <|> pValue
    where
        build left (output,optional) right = TFun output optional left right
    
    
  --  TFun False False <$> pValue <* pSymbol "->"   <*> pType
  --  <|> TFun False True  <$> pValue <* pSymbol "?->"  <*> pType
  --  <|> TFun True  False <$> pValue <* pSymbol "&->"  <*> pType
  --  <|> TFun True  True  <$> pValue <* pSymbol "&?->" <*> pType
  --  <|> pValue


pArrowKind :: Parser (Bool,Bool)
pArrowKind = (False,False) <$ pSymbol "->"
         <|> (False,True)  <$ pSymbol "?->"
         <|> (True, False) <$ pSymbol "&->"
         <|> (True, True)  <$ pSymbol "&?->"
    
pValue :: Parser (NativeType NativeTypeSet)
pValue = TValue <$> pTypeSet

pTerminalType :: Parser NativeTerminalType
pTerminalType = NativeString    <$ pSymbol "string"
            <|> NativeBoolean   <$ pSymbol "boolean"
            <|> NativeTrue      <$ pSymbol "true"
            <|> NativeFalse     <$ pSymbol "false"
            <|> NativeInteger   <$ pSymbol "integer"
            <|> NativeDouble    <$ pSymbol "double"
            <|> NativeFloat     <$ pSymbol "float"
            <|> NativeNull      <$ pSymbol "null"
            <|> NativeObject    <$ pSymbol "object" <*> pBrackets pIdentifier
            <|> NativeResource  <$ pSymbol "resource" <*> pParens pResource
            <|> NativeVar . ord <$> pParens pLetter
            <|> NativeArray     <$ pSymbol "array" <* pSymbol "(" <*> pTypeSet <* pSymbol "=>" <*> pTypeSet <* pSymbol ")"

pTypeSet :: Parser NativeTypeSet
pTypeSet = L.foldr S.insert S.empty <$> pBraces (pListSep pComma pTerminalType)
          
pResource :: Parser Resource
pResource = GD          <$ pSymbol "gd"
        <|> GDFont      <$ pSymbol "gdfont"
        <|> ProcessFile <$ pSymbol "processfile"
        <|> File        <$ pSymbol "file"
        <|> Context     <$ pSymbol "context"
        
        
-- TODO Add support for fields. One may inherit a class and use protected fields.        
pClass :: Parser (String, NativeClassDecl)
pClass = build <$> pClassName 
               <*> opt (Just <$ pSymbol ":" <*> pClassName) Nothing 
               <*  pSymbol "=" 
               <*> pBraces (
                       (,,) <$  pBrackets (pSymbol "constants")
                            <*> pMany pConstant    
                            <*  pBrackets (pSymbol "fields")
                            <*> pMany pConstant
                            <*  pBrackets (pSymbol "methods")
                            <*> pMany pFunction
               )
               where
                   build name extends (constants,fields,methods) = (name, NativeClassDecl name extends NativeConstructorDecl constants' fields' methods')
                       where
                           constants' = L.map (\(name,ty) -> NativeClassConstantDecl name ty) constants
                           fields'    = L.map (\(name,ty) -> NativeFieldDecl False name ty) fields
                           methods'   = L.map (\(name,ty) -> NativeMethodDecl False name ty) methods
                           

parse :: String -> NativeAPI
parse inp = let (a,errors) = UU.parse ((,) <$> pNativeAPI <*> pEnd) (createStr (LineColPos 0 0 0) inp)
            in case L.null errors of
                True  -> a 
                False -> error $ "Error while parsing native API: " ++ show errors

getNativeAPI :: FilePath -> IO NativeAPI
getNativeAPI file = do inp <- readFile file
                       return . parse $ inp