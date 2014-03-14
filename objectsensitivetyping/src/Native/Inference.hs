{-# LANGUAGE FlexibleContexts #-}
module Native.Inference where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Analysis.Lattice.AbstractValue
import Analysis.SensitivitySpec
import Analysis.Identifier
import Framework.Lattice

--import Native.NativeType hiding (False, value)

import Native.NativeAPI

import Native.ToValue
import PHP.IR hiding (value)

import Debug.Trace

type Subst = [(TypeVar, AbstractValue)]

nullSubst :: Subst
nullSubst = []

infixr 4 @@
(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = M.toList $ M.unionWith join (M.fromList s1) (M.fromList s2)

(+->) :: TypeVar -> AbstractValue -> Subst
u +-> t = [(u, t)]

infixr 4 `fn`
fn :: NativeType a -> NativeType a -> NativeType a
a `fn` b = TFun False False a b

--union :: NativeType InferenceValue
--union = TUnion Nothing Nothing Nothing (bottom, bottom, bottom)

unify :: NativeType InferenceValue -> NativeType InferenceValue -> Subst
unify (TFun _ _ l r) (TFun _ _ l' r') = 
    let s1 = unify l l'
        s2 = unify (apply s1 r) (apply s1 r')
    in s1 @@ s2                            
unify l@(TValue iv) r@(TValue iv') = 
    let s1 = unifyValue normalVar normal iv iv'
        s2 = unifyValue keyVar    key    iv iv'
        s3 = unifyValue valueVar  value  iv iv'
    in s3 @@ s2 @@ s1

unifyValue :: (InferenceValue -> S.Set TypeVar) -> (InferenceValue -> AbstractValue) -> InferenceValue -> InferenceValue -> Subst
unifyValue var value left right = if S.null (var left) 
                                  then S.fold (@@) nullSubst . S.map (\u -> u +-> value left)  $ (var right)
                                  else S.fold (@@) nullSubst . S.map (\u -> u +-> value right) $ (var left)


--unifyUnion var value left right = 
--    case var left of
--        Nothing -> case var right of 
--            Nothing -> nullSubst
--            Just u  -> u +-> value left
--        Just u  -> u +-> value right
        

-- selectFirst  (n,k,v) = n
-- selectSecond (n,k,v) = k
-- selectThird  (n,k,v) = v
        

apply :: Subst -> NativeType InferenceValue -> NativeType InferenceValue
apply s   (TFun o b l r) = TFun o b (apply s l) (apply s r)
apply s t@(TValue iv)    = TValue 
                         ( L.foldr (.) id (L.map (applyNormal s) (S.toList $ normalVar iv))
                         . L.foldr (.) id (L.map (applyKey s)    (S.toList $ keyVar iv))
                         . L.foldr (.) id (L.map (applyValue s)  (S.toList $ valueVar iv))
                         $ iv)

applyNormal s u iv = case lookup u s of
   Just v  -> iv { normal = join v (normal iv), normalVar = S.empty }
   Nothing -> iv   

applyKey s u iv = case lookup u s of
   Just v  -> iv { key = join v (key iv), keyVar = S.empty }
   Nothing -> iv

applyValue s u iv = case lookup u s of
   Just v  -> iv { value = join v (value iv), valueVar = S.empty } 
   Nothing -> iv
      
    
    
result :: NativeType InferenceValue
result = TValue $ def { normalVar = S.singleton 0, keyVar = S.singleton 1, valueVar = S.singleton 2 }    
    
inference :: Maybe Identifier -> [Identifier] -> HContext -> NativeType NativeTypeSet -> [(AbstractValue,AbstractValue,AbstractValue)] -> [(Identifier, (AbstractValue,AbstractValue,AbstractValue))]
inference v ps hctx tFunction params = --trace ("Infericing: " ++ functionName ++ ", params: " ++ show params) $
    let tSpecialized = case specialize (length params) tFunction of
            Nothing -> error $ "Unable to specialize: " ++ show tFunction ++ " for " ++ show (length params) ++ " parameters"
            Just t  -> fmap (toValue hctx) t
        tMatch  = foldr fn result 
--                . reverse 
                . map (\(i,(n,k,v)) -> TValue $ def { normal = n, key = k, value = v, normalVar = S.singleton (i * 3), keyVar = S.singleton (i * 3 + 1), valueVar = S.singleton (i * 3 + 2) })
                . zip [1..] 
                $ params
        s       = unify tSpecialized tMatch -- trace ("About to unify --- \n" ++ show tSpecialized ++ "\n" ++ show tMatch) $
      
        work (TFun True _ _ r)  (TFun _ _ (TValue l) r') xs = [(head xs, toTriple l)] ++ work r r' (tail xs)
        work (TFun False _ _ r) (TFun _ _ _ r')          xs = work r r' (tail xs)
        work (TValue v)         (TValue v')              xs = if L.null xs 
                                                              then []
                                                              else [(head xs, toTriple v')]
                                                              
        toTriple iv = (normal iv, key iv, value iv)                                                      
        
    in work tSpecialized (apply s tMatch) (ps ++ maybe [] (:[]) v)
    
    