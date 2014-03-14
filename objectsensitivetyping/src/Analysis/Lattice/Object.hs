{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BangPatterns #-}
module Analysis.Lattice.Object where
    
import Data.Map as M
import Data.Set as S
import CCO.Printing

-- import PHP.IR 

import Framework.Lattice
import Analysis.Lattice.AbstractValue
import Analysis.Lattice.AbstractAddress
import Analysis.Lattice.AbstractNull as Null
import Analysis.Inject
import Analysis.Index

type Object = M.Map Index AbstractValue

lookupField :: Index -> Object -> AbstractValue
lookupField = M.findWithDefault $ inject Null.Top

instance Printable Object where
    pp fields = lbrace 
            >|< space 
            >|< above (M.elems . M.mapWithKey (\index value -> pp index >|< colon >|< space >|< pp value) $ fields) 
            >|< space 
            >|< rbrace
    
instance Addressable Object where
    addresses = M.fold S.union S.empty . M.map addresses 