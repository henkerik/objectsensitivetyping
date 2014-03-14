{-# LANGUAGE BangPatterns #-}
module Analysis.ProgramState.Naive where

import Control.DeepSeq
import Data.Map as M
import Data.Set as S
import Data.List as L
import CCO.Printing hiding (join)

import Framework.Lattice
import Analysis.Inject
import Analysis.Coerce
import Analysis.Identifier
import Analysis.ProgramState
import Analysis.Lattice.Stack as Stack
import Analysis.Lattice.Heap
import Analysis.Lattice.AbstractNull as Null
-- import Analysis.Lattice.AbstractUndefined as Undefined
import Analysis.Lattice.AbstractAddress
import Analysis.Lattice.AbstractValue
import Analysis.Lattice.AbstractString
import Analysis.Lattice.ConstantMap

import Debug.Trace

data NPS = NPS {
    stack     :: Stack,
    heap      :: Heap,
    constants :: ConstantMap
} deriving (Eq, Ord)

data State = Processed
           | Unprocessed
           deriving (Eq,Ord,Show)

instance NFData NPS where
    rnf (NPS stack heap cons) = rnf stack `seq` rnf heap `seq` rnf cons

instance Lattice NPS where
    bottom = NPS bottom bottom bottom
    (NPS stack heap cons) `join` (NPS stack' heap' cons') = NPS (stack `join` stack') (heap `join` heap') (cons `join` cons')
    (NPS stack heap cons) <:     (NPS stack' heap' cons') = (stack <: stack') && (heap <: heap') && (cons <: cons')
    
    
instance Printable NPS where
    pp (NPS stack heap cons) = pp stack >-< pp heap >-< pp cons


instance ProgramState NPS where
    insertIdent ident value     st = st { stack = M.insert ident value $ stack st } 
    lookupIdent ident           st = M.findWithDefault (inject Null.Top) ident (stack st)
    removeIdent ident           st = st { stack = M.delete ident $ stack st }
    clearHeap                   st = st { heap = bottom } 
    clearStack                  st = st { stack = M.filterWithKey (flip . const $ isStaticVariable) $ stack st }
    varsToParams ps             st = st { stack = Stack.varsToParams ps $ stack st }
   -- paramsToVars ps             st = let (stack', xs) = Stack.paramsToVars ps (stack st)
     --                                in (st { stack = stack' }, xs)
                                     
    paramsToVars ps             (NPS stack heap cons) = let (stack', f) = Stack.paramsToVars ps stack
                                                        in f (NPS stack' heap cons)
                                     
    insertConstant v value      st = st { constants = constants' }
                                   where
                                       constants' = L.foldr (\idx -> M.insertWith join idx value) (constants st) . maybe [Default] (L.map Named) . toString . coerce . lookupVar v $ st
    lookupConstant name         st = M.findWithDefault bottom (Named name) (constants st) `join` M.findWithDefault bottom Default (constants st)
    readField ident idx hctx    st = (st, value) 
        where
            value = if S.null addresses 
                    then inject Null.Top 
                    else S.fold join bottom -- (inject Null.Top)  -- Undefined or NULL or bottom?
                       . S.map (M.findWithDefault (inject Null.Top) idx)  -- lookupField
                       . S.map (flip (M.findWithDefault bottom) (heap st))     -- lookupObject
                       $ addresses

            addresses = coerce
                      . M.findWithDefault bottom ident
                      . stack 
                      $ st
                  
    writeField' hctx idx value st = st { heap = heap' }
        where
            heap' = M.insertWith join hctx (M.singleton idx value) (heap st)
                  
    writeField ident idx value hctx st = st { heap = heap' }
        where
            heap' = S.fold (flip (M.insertWith join) (M.singleton idx value)) (heap st)
                  . coerce 
                  . M.findWithDefault bottom ident -- (error "Should be implossible") (Variable v)
                  $ stack st

{-                  $ stack'


    readField ident idx hctx        st = (st, value) 
        where
            value = if S.null addresses 
                    then inject Undefined.Top
                    else S.fold join bottom -- (inject Null.Top)  -- Undefined or NULL?
                       . S.map (M.findWithDefault (inject Undefined.Top) idx)  -- lookupField
                       . S.map (flip (M.findWithDefault bottom) (heap st))     -- lookupObject
                       $ addresses

            addresses = coerce
                      . M.findWithDefault bottom ident
                      . stack 
                      $ st
                                             
            stack' = M.alter f (Variable v) (stack st)
                where 
                    f Nothing      = Just new
                    f (Just value) | isAddress value = Just value -- $ value `join` new -- NON MONOTOMIC Just value
                                   | otherwise       = Just $ value `join` new
                  
                    new = inject . fromHContext $ hctx 
                    
                    -}
        
    getParameters = L.filter isParameter . M.keys . stack    
           
    preformGC False st = st                                                     
    preformGC True  st = st { heap = M.map getValue . M.filter isProcessed $ heap' }
        where
            makeProcessed = (,)  Processed   . snd
            isProcessed   = (==) Processed   . fst 
            getValue      = snd
        
            initialHeap  = M.map ((,) Unprocessed) (heap st)
            start        = S.toList . addresses . stack $ st
                
            heap'        = process start initialHeap
        
            process []          heap = heap
            process (hctx:tail) heap = 
                if isProcessed entry
                then process tail          heap
                else process (new ++ tail) heap'
                    where
                        heap' = M.adjust makeProcessed hctx heap
                        new   = S.toList . addresses . getValue $ entry
                        
                        entry = case M.lookup hctx heap of 
                            Nothing    -> (Processed, error "Dangling pointer to heap")
                            Just entry -> entry
        
  