{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Analysis.Lattice.CallSet where

import Data.Set     as S
import Data.List    as L

import CCO.Printing as P hiding (render, join)
import Framework.Lattice

import PHP.IR
    
type CallSet l ctx func = Set (l,ctx,func,ctx)

instance (Ord l, Ord ctx, Ord func) => Lattice (CallSet l ctx func) where
    join = S.union
    (<:) = S.isSubsetOf
    bottom = S.empty

instance (Show l, Show ctx, Show func) => Printable (CallSet l ctx func) where
    pp = L.foldl (>-<) P.empty . L.map showable . S.toList