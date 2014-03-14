{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BangPatterns #-}
module Analysis.Lattice.Stack where

import Control.Monad
import Data.Map as M
import Data.List as L
import Data.Set as S
import Data.Maybe
import Data.List
import CCO.Printing
import Framework.Lattice
import Analysis.Lattice.AbstractValue
import Analysis.Lattice.AbstractAddress
import Analysis.Lattice.AbstractNull as Null
import Analysis.Inject
import PHP.IR as IR
import Debug.Trace
import Analysis.Identifier

import Control.Monad
import Control.Arrow
import Debug.Trace

type Stack = M.Map Identifier AbstractValue


instance Addressable Stack where
    addresses = M.fold S.union S.empty . M.map addresses


instance Printable Stack where
    pp stack = lbracket 
           >|< space 
           >|< above (M.elems . M.mapWithKey (\identifier value -> showable identifier >|< text " :: " >|< pp value) $ stack)
           >|< space 
           >|< rbracket


                
varsToParams :: [Var] -> Stack -> Stack
varsToParams ps stack = M.filterWithKey (flip . const $ \ident -> isParameter ident || isStaticVariable ident || isThisIdentifier ident || isReturn ident) $ L.foldr updateStack stack (L.zip [0..] ps)
    where
        updateStack :: (Int, Var) -> Stack -> Stack
        updateStack (n,v) st = let value = case M.lookup (Variable v) st of
                                              Nothing    -> inject Null.Top
                                              Just value -> value
                               in M.insert (Parameter n) value st
    
    
  --                  
paramsToVars :: Monad m => [(Var, st -> m st)] -> Stack -> (Stack, st -> m st)
paramsToVars ps stack = first (M.filterWithKey (flip . const $ \ident -> isVariable ident || isStaticVariable ident || isThisIdentifier ident || isReturn ident))
                      . L.foldr updateStack (stack, return) 
                      . L.zip [0..] 
                      $ ps
    where
       updateStack (n,(v,g)) (st, f) = case M.lookup (Parameter n) stack of
           Nothing     -> (st, g >=> f) -- error "Required parameter missing"
           Just value  -> (M.insert (Variable v) value st, f)
           
--       updateStack (n,(Param hint ref v (Just cons))) (st, xs) = case M.lookup (Parameter n) stack of
--           Nothing     -> (st, (v,cons):xs)
--           Just value' -> (M.insert (Variable v) value' st, xs)
              


 --                        in (M.filterWithKey (flip . const $ \ident -> isVariable ident || isStaticVariable ident || isThisIdentifier ident) stack', f)

-- Should not be used anymore  but is still used by Test.hs
lookupValue = flip (M.findWithDefault $ inject Null.Top)