{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module PHP.Parser where

import           Data.Char
import           Data.Set as S
import           Data.List as L
import qualified Data.Text as T
import           Control.Applicative hiding (Const)
import           Control.Monad.Error hiding (sequence)
import qualified Data.Attoparsec.Text as P

--import qualified Data.Attoparsec.Text as A

import           Data.Attoparsec.Text (Parser)
import           Data.Attoparsec.Combinator
import           Data.Text (Text)

import           PHP.IR
import           Prelude hiding (sequence)



parser :: (MonadError String m) => Text -> m Program 
parser inp = (process . P.parse pProgram) inp
    where 
        process (P.Fail rm ctx msg) = throwError msg
        process (P.Partial c)       = process (P.feed (P.Partial c) inp)
        process (P.Done rm r)       = return r


pProgram :: Parser Program
pProgram = flip Program [] <$ pOpenTag <*> pStmt <* pCloseTag

pOpenTag, pCloseTag :: Parser T.Text
pOpenTag  = pSymbol "<?php"
pCloseTag = pSymbol "?>"


sequence left Skip  = left
sequence left right = Sequence left right

pStmt :: Parser Stmt
pStmt = (sequence <$> pStmt' <*> pStmt) 
    <|> pStmt'
    <|> pure Skip
                      
buildForeach :: Var -> Bool -> Var -> Maybe (Bool, Var) -> Stmt -> Stmt
buildForeach iter _   key (Just (ref, value)) stmt = Foreach iter (Just key) ref value stmt
buildForeach iter ref value Nothing           stmt = Foreach iter Nothing    ref value stmt


--pFoo = (\ref value -> Left (ref, value)) <$> pReference <*> pVar
--   <|> (\key ref value -> Right (key,ref,value)) <$> pVar <* pDoubleArrow <*> pReference <*> pVar

pStmt' :: P.Parser Stmt
pStmt' = If                <$ pIfToken <*> pParens pVar <*> (pBraces pStmt <|> pStmt') <*> opt (pElseToken *> pBraces pStmt) Skip
     <|> While             <$ pWhileToken <* pParens pTrueToken <*> pBraces pStmt
     <|> Try               <$ pTryToken <*> pBraces pStmt <*> pMany pCatchClause
     <|> buildForeach      <$ pForeachToken <* pLParen <*> pVar <* pAsToken <*> pReference <*> pVar <*> opt ((\ref value -> Just (ref, value)) <$ pDoubleArrow <*> pReference <*> pVar) Nothing <* pRParen <*> pBraces pStmt
                           
     <|> Return            <$ pReturnToken <*> pVar <* pSemicolon
     <|> Throw             <$ pThrowToken <*> pVar <* pSemicolon
     <|> Continue          <$ pContinueToken <* pSemicolon -- Add continue 1;
     <|> Break             <$ pBreakToken <* pSemicolon -- TODO Add break 1;
     <|> InstanceOf        <$> pVar <* pEqual <*> pVar <* pInstanceOfToken <*> pType <* pSemicolon
     <|> Static            <$ pStaticToken <*> pVar <*> opt (pEqual *> pConst) Null <* pSemicolon
     
     
                           
     <|> Declare           <$> pVar <* pEqual <*> pConst <* pSemicolon
     <|> Assign            <$> pVar <* pEqual <*> pReference <*> pVar <* pSemicolon
     
     -- Special functions: exit, die, unset, define, and clone
     <|> Die               <$  (pExitToken <|> pDieToken) <* pLParen <*> pVar <* pRParen <* pSemicolon
     <|> Clone             <$> pVar <* pEqual <* pCloneToken <*> pParens pVar <* pSemicolon
     <|> Unset             <$  pUnsetToken <*> pParens pVar <* pSemicolon
     <|> Define            <$  pDefineToken <* pLParen <*> pVar <* pComma <*> pVar <* pRParen <* pSemicolon
     
     -- Casts
     <|> Cast              <$> pVar <* pEqual <*> pParens pCast <*> pVar <* pSemicolon     
     
     -- Calls
     <|> ConstructorCall   <$> pVar <* pEqual <* pNewToken <*> pType <*> tupleParser pVar <* pSemicolon
     <|> FunctionCall      <$> opt (Just <$> pVar <* pEqual) Nothing <*> pReference <*> pFunctionName <*> tupleParser pVar <* pSemicolon
     <|> MethodCall        <$> opt (Just <$> pVar <* pEqual) Nothing <*> pReference <*> pVar <* pArrow <*> pMethodName <*> tupleParser pVar <* pSemicolon
     <|> StaticCall        <$> opt (Just <$> pVar <* pEqual) Nothing <*> pReference <*> pType <* pDoubleColon <*> pMethodName <*> tupleParser pVar <* pSemicolon
                           
     -- Fields             
     <|> WriteField        <$> pVar <* pArrow <*> pFieldName <* pEqual <*> pReference <*> pVar <* pSemicolon
     <|> ReadField         <$> pVar <* pEqual <*> pReference <*> pVar <* pArrow <*> pFieldName <* pSemicolon
                           
     -- Static Fields      
     <|> WriteStaticField  <$> pType <* pDoubleColon <*> pVar <* pEqual <*> pReference <*> pVar <* pSemicolon
     <|> ReadStaticField   <$> pVar <* pEqual <*> pReference <*> pType <* pDoubleColon <*> pVar <* pSemicolon
                                       
     -- Array              
     <|> ReadArray         <$> pVar <* pEqual <*> pReference <*> pVar <*> pBrackets pVar <* pSemicolon
     <|> WriteArray        <$> pVar <*> pBrackets pIndex <* pEqual <*> pReference <*> pVar <* pSemicolon
    
     <|> PreDecr           <$  pDoubleMin <*> pVar <* pSemicolon
     <|> PreIncr           <$  pDoublePlus <*> pVar <* pSemicolon
    
     <|> buildBinOp        <$> pVar 
                           <*  pEqual 
                           <*  pSymbol "(" 
                           <*> pVar 
                           <*> pBinaryOp 
                           <*> pVar 
                           <*  pSymbol ")" 
                           <*  pSemicolon
                           
     <|> buildUnaryOp      <$> pVar
                           <*  pEqual
                           <*> pUnaryOp
                           <*> pVar
                           <*  pSemicolon
     
     -- Interfaces, classes and functions
     <|> InterfaceDecl     <$  pInterfaceToken
                           <*> pType
                           <*> pExtendsInterface
                           <*> pBraces (pMany pMember)
                           
     <|> ClassDecl         <$> pAbstract 
                           <*> pFinal 
                           <*  pClassToken 
                           <*> pType 
                           <*> pExtendsClass
                           <*> pImplements 
                           <*> pBraces (pMany pMember)
                           
     <|> FunctionDecl      <$  pFunctionToken 
                           <*> pReference
                           <*> pFunctionName 
                           <*> tupleParser pParam 
                           <*> pBraces pStmt

pReference :: P.Parser Bool
pReference = opt (True <$ pAmpersand) False

pIndex :: P.Parser (Maybe Var)
pIndex = opt (Just <$> pVar) Nothing




pCast :: P.Parser Cast
pCast = CastArray  <$ pSymbol "array"
    <|> CastBool   <$ pSymbol "bool"
    <|> CastInt    <$ pSymbol "int"
    <|> CastObject <$ pSymbol "object"
    <|> CastReal   <$ pSymbol "real"
    <|> CastString <$ pSymbol "string"
    <|> CastUnset  <$ pSymbol "unset"
                           
pCatchClause :: P.Parser Clause
pCatchClause = Clause      <$  pCatchToken 
                           <*  pLParen 
                           <*> pType 
                           <*> pVar 
                           <*  pRParen 
                           <*> pBraces pStmt

pExtendsClass :: P.Parser (Maybe Type)
pExtendsClass = Just    <$ pExtendsToken <*> pType
            <|> pure Nothing

pImplements :: P.Parser (Set Type)
pImplements = S.fromList <$ pImplementsToken <*> pListSep pComma pType
          <|> pure S.empty

pExtendsInterface :: P.Parser (Set Type)
pExtendsInterface = S.fromList <$ pExtendsToken <*> pListSep pComma pType
                <|> pure S.empty





pMember :: Parser Member 
pMember = MethodDecl       <$> (pVisibility <|> pure Public)          
                           <*> pStatic 
                           <*> pAbstract 
                           <*> pFinal 
                           <*  pFunctionToken 
                           <*> pReference
                           <*> pMethodName 
                           <*> tupleParser pParam 
                           <*> (Just <$> pBraces pStmt <|> Nothing <$ pSemicolon)
            
     <|> MethodDefinitionDecl <$ (pPublicToken <* pFunctionToken <|> pFunctionToken)
                           <*> pMethodName
                           <*> tupleParser pParam
                           <*  pSemicolon
                           
     <|> FieldDecl         <$> (pVisibility <|> Public <$ pVarToken)
                           <*> pStatic 
                           <*> pVar 
                           <*> opt (Just <$ pEqual <*> pConst) Nothing
                           <*  pSemicolon

     <|> ConstantDecl      <$  pConstToken 
                           <*> pClassConstantName 
                           <*  pEqual
                           <*> pConst 
                           <*  pSemicolon


pAbstract :: P.Parser Bool
pAbstract = True <$ pAbstractToken
        <|> pure False

pFinal :: P.Parser Bool 
pFinal = True <$ pFinalToken
     <|> pure False

pStatic :: P.Parser Bool
pStatic = True <$ pStaticToken
      <|> pure False

pVisibility :: P.Parser Visibility
pVisibility = Public    <$ pPublicToken
          <|> Protected <$ pProtectedToken
          <|> Private   <$ pPrivateToken 
                


pIdentifier :: P.Parser String
pIdentifier = (:) <$> (pSym '_' <|> pLetter) <*> pMany (pLetter <|> pDigit <|> pSym '_')

pClassConstantName :: P.Parser ClassConstantName
pClassConstantName = lexeme pIdentifier

pFunctionName :: P.Parser FunctionName
pFunctionName = lexeme pIdentifier

pMethodName :: P.Parser MethodName
pMethodName = lexeme pIdentifier

pConstantName :: P.Parser ConstantName
pConstantName = lexeme pIdentifier

pParam :: P.Parser Param
pParam = Param <$> opt (Just <$> pType) Nothing <*> pReference <*> pVar <*> opt (Just <$ pEqual <*> pConst) Nothing -- TODO Add Type annotation and default value
--pParam = Param <$> (Just <$> pType) `opt` Nothing <*> pVar <*> (Just <$ pEqual <*> pConst) `opt` Nothing

pVar :: P.Parser Var
pVar = lexeme $ pSym '$' *> pIdentifier

pType :: P.Parser Type
pType = lexeme $ (:) <$> (pLetter <|> pSym '_') <*> pMany (pLetter <|> pDigit <|> pSym '_')

pFieldName :: P.Parser FieldName
pFieldName = lexeme pIdentifier


pBool :: P.Parser Bool
pBool = True  <$ (pSymbol "true"  <|> pSymbol "True")
    <|> False <$ (pSymbol "false" <|> pSymbol "False")

pNoneOf :: [Char] -> P.Parser Char
pNoneOf cs = P.satisfy (\c -> c `L.notElem` cs) 

pQuotedString :: Char -> P.Parser String
pQuotedString d = T.unpack . T.concat <$ P.char d <*> P.many' (pEscape <|> (T.pack <$> P.many1 (pNoneOf ['\\', d]))) <* P.char d
    where
        pEscape = f <$> P.char '\\' <*> ((Just <$> P.char d) <|> (Just <$> P.char '\\') <|> pure Nothing)
        
        f x Nothing  = T.singleton x
        f x (Just y) = T.pack [x,y]
        
--        pEscape = (\x y -> T.pack [x,y]) <$> P.char '\\' <*> (P.char d <|> P.char '\\' <|> pure '\\')
--        pEscape = (\x y -> T.singleton y) <$> P.char '\\' <*> (P.char d <|> P.char '\\' <|> pure '\\')

pSingleQuotedString, pDoubleQuotedString :: P.Parser String
pSingleQuotedString = pQuotedString '\''
pDoubleQuotedString = pQuotedString '"'


pConst :: P.Parser Const
pConst = id                 <$> pParens pConst 
     <|> Null               <$  pSymbol "NULL"  -- `micro` 0
     <|> Array unknown      <$  pSymbol "array" <*> tupleParser pArrayElem
     <|> Bool               <$> pBool 
     <|> process            <$> P.number
     <|> SingleQuotedString <$> pSingleQuotedString
     <|> DoubleQuotedString <$> pDoubleQuotedString
     <|> ClassConstant      <$> pType <* pDoubleColon <*> pClassConstantName
     <|> Constant           <$> pConstantName --`micro` 1
     where
         process (P.I value) = Integer value
         process (P.D value) = Double value
         
         unknown = error "Unknown array label. An array label will be created in the first transformation phase"
     
pArrayElem = ElemWithKey    <$> lexeme pConst <* pDoubleArrow <*> lexeme pConst
         <|> ElemWithoutKey <$> lexeme pConst


 
pUnaryOp :: P.Parser UnaryOp
pUnaryOp = Not            <$ pSymbol "!"    
       <|> BNot           <$ pSymbol "~"

pBinaryOp :: P.Parser BinaryOp
pBinaryOp = Plus             <$ pSymbol "+"
        <|> Min              <$ pSymbol "-"
        <|> Mul              <$ pSymbol "*"
        <|> Div              <$ pSymbol "/"
        <|> Mod              <$ pSymbol "%"
       
        -- String statements
        <|> Concat           <$ pSymbol "."
       
        -- Logical statements
        <|> And              <$ pSymbol "&&"
        <|> Or               <$ pSymbol "||"
        <|> Xor              <$ pSymbol "xor"
       
        -- Bitwise statements
        <|> SL               <$ pSymbol "<<"
        <|> SR               <$ pSymbol ">>"
        <|> BAnd             <$ pSymbol "&"
        <|> BOr              <$ pSymbol "|"
        <|> BXor             <$ pSymbol "^"
       
        -- Comarision
        <|> IsIdentical      <$ pSymbol "==="
        <|> IsEqual          <$ pSymbol "=="
        <|> IsNotIdentical   <$ pSymbol "!=="
        <|> IsNotEqual       <$ pSymbol "!="
        <|> LessEqual        <$ pSymbol "<="
        <|> GreaterEqual     <$ pSymbol ">="
        <|> Less             <$ pSymbol "<"
        <|> Greater          <$ pSymbol ">"


     
buildBinOp :: Var -> Var -> BinaryOp -> Var -> Stmt
buildBinOp a b op c = BinaryOp op a b c

buildUnaryOp :: Var -> UnaryOp -> Var -> Stmt
buildUnaryOp a op b = UnaryOp op a b



-- Keywords
pIfToken, pElseToken, pWhileToken, pTryToken, pCatchToken, pBreakToken, pContinueToken, 
    pNewToken, pReturnToken, pFunctionToken, pClassToken, pVarToken, pExtendsToken, 
    pImplementsToken, pConstToken, pPublicToken, pProtectedToken, pPrivateToken, 
    pAbstractToken, pStaticToken, pFinalToken, pInterfaceToken, pThrowToken, 
    pInstanceOfToken, pForeachToken, pAsToken, pUnsetToken, pDefineToken, pExitToken, 
    pDieToken, pCloneToken :: P.Parser T.Text
pIfToken         = pSymbol "if"
pElseToken       = pSymbol "else"
pWhileToken      = pSymbol "while"
pTryToken        = pSymbol "try"
pCatchToken      = pSymbol "catch"
pThrowToken      = pSymbol "throw"
pBreakToken      = pSymbol "break"
pContinueToken   = pSymbol "continue"
pNewToken        = pSymbol "new"
pReturnToken     = pSymbol "return"
pFunctionToken   = pSymbol "function"
pClassToken      = pSymbol "class"
pVarToken        = pSymbol "var"
pExtendsToken    = pSymbol "extends"
pImplementsToken = pSymbol "implements"
pConstToken      = pSymbol "const"
pPublicToken     = pSymbol "public"
pProtectedToken  = pSymbol "protected"
pPrivateToken    = pSymbol "private"
pAbstractToken   = pSymbol "abstract"
pFinalToken      = pSymbol "final"
pStaticToken     = pSymbol "static"
pInterfaceToken  = pSymbol "interface"
pInstanceOfToken = pSymbol "instanceof"
pForeachToken    = pSymbol "foreach"
pAsToken         = pSymbol "as"
pUnsetToken      = pSymbol "unset"
pDefineToken     = pSymbol "define"
pExitToken       = pSymbol "exit"
pDieToken        = pSymbol "die"
pCloneToken      = pSymbol "clone"

pTrueToken, pFalseToken :: P.Parser T.Text
pTrueToken       = pSymbol "True"
pFalseToken      = pSymbol "False"

pEqual, pSemicolon, pArrow, pDoubleColon, pDoublePlus, pDoubleMin, pAmpersand, pDoubleArrow :: P.Parser T.Text
pEqual           = pSymbol "="
pSemicolon       = pSymbol ";"
pArrow           = pToken "->"
pDoubleColon     = pToken "::"
pDoublePlus      = pToken "++"
pDoubleMin       = pToken "--"
pAmpersand       = pSymbol "&"
pDoubleArrow     = pSymbol "=>"


-- Convert API: Atto to UU

lexeme p = p <* pSpaces

pSpaces :: P.Parser ()
pSpaces = P.skipSpace

pSymbol :: Text -> P.Parser T.Text
pSymbol = lexeme . pToken

pToken :: Text -> P.Parser T.Text
pToken = P.string

pSym :: Char -> P.Parser Char
pSym = P.char

pDigit, pLetter :: P.Parser Char
pLetter = P.letter
pDigit  = P.digit
pUpper  = P.satisfy isUpper
pLower  = P.satisfy isLower


pComma, pLParen, pRParen, pLBrace, pRBrace, pLBracket, pRBracket :: P.Parser Char
pComma    = lexeme $ P.char ','
pLParen   = lexeme $ P.char '('
pRParen   = lexeme $ P.char ')'
pLBrace   = lexeme $ P.char '{'
pRBrace   = lexeme $ P.char '}'
pLBracket = lexeme $ P.char '['
pRBracket = lexeme $ P.char ']'

pParens   p = pLParen   *> p <* pRParen
pBraces   p = pLBrace   *> p <* pRBrace
pBrackets p = pLBracket *> p <* pRBracket

pListSep = flip P.sepBy
opt      = flip P.option

tupleParser :: P.Parser a -> P.Parser [a]
tupleParser = pParens . pListSep pComma

pMany       = P.many'