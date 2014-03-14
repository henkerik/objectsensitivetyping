{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Analysis.Coerce where

import Prelude hiding (null, undefined, fromInteger)
import Data.Set as S hiding (null)
import Data.List as L hiding (null)

import Framework.Lattice

import Analysis.Lattice.Sign
import Analysis.Lattice.AbstractValue
import Analysis.Lattice.AbstractAddress
import Analysis.Lattice.AbstractInteger
import Analysis.Lattice.AbstractDouble
import Analysis.Lattice.AbstractBool
import Analysis.Lattice.AbstractString as String
import Analysis.Lattice.AbstractResource
import Analysis.Lattice.AbstractNull as Null
import Analysis.Lattice.AbstractUnset 
-- import Analysis.Lattice.AbstractUndefined as Undefined
import Analysis.Lattice.AbstractNumeric

import Analysis.Resource
import Analysis.SensitivitySpec


class Coerce a b where
    coerce :: a -> b


-- To AbstractAddress

instance Coerce AbstractValue AbstractAddress where
    coerce = address



-- To AbstractInteger  

instance Coerce AbstractValue AbstractInteger where
    coerce value = L.foldr join bottom [ coerce (address value)
                                       , integer value
                                       , coerce (double value)
                                       , coerce (bool value)
                                       , coerce (string value)
                                       , coerce (resource value)
                                       , coerce (null value)
                                       -- , coerce (undefined value)
                                       ]
                                       
instance Coerce AbstractAddress AbstractInteger where
    coerce = AI . S.map coerce                                       

instance Coerce AbstractDouble AbstractInteger where
    coerce (AD set) = S.fold join bottom . S.map coerce $ set
        
instance Coerce AbstractBool AbstractInteger where
    coerce = AI . S.map coerce 
        
instance Coerce AbstractString AbstractInteger where
    coerce String.Bottom = bottom
    coerce (Value str)   = fromInteger (parseToInteger str)
    coerce String.Top    = anyInteger
    
instance Coerce AbstractResource AbstractInteger where
    coerce = AI . S.map coerce
        
instance Coerce AbstractNull AbstractInteger where
    coerce Null.Bottom = bottom
    coerce Null.Top    = fromInteger 0
 
 {-
instance Coerce AbstractUndefined AbstractInteger where
    coerce Undefined.Bottom = bottom
    coerce Undefined.Top    = fromInteger 0
   -} 

-- To AbstractDouble

instance Coerce AbstractValue AbstractDouble where
    coerce value = L.foldr join bottom [ coerce (address value)
                                       , coerce (integer value)
                                       , double value
                                       , coerce (bool value)
                                       , coerce (string value)
                                       , coerce (resource value)
                                       , coerce (null value)
                                       --, coerce (undefined value)
                                       ]                                       

instance Coerce AbstractAddress AbstractDouble where
    coerce = AD . S.map coerce

instance Coerce AbstractInteger AbstractDouble where
    coerce (AI set) = AD set 
    
instance Coerce AbstractBool AbstractDouble where    
    coerce = AD . S.map coerce
    
instance Coerce AbstractString AbstractDouble where
    coerce String.Bottom = bottom
    coerce (Value str)   = fromDouble . fromIntegral . parseToInteger $ str
    coerce String.Top    = anyDouble     
        
instance Coerce AbstractResource AbstractDouble where
    coerce = AD . S.map coerce
    
instance Coerce AbstractNull AbstractDouble where
    coerce Null.Bottom = bottom       
    coerce Null.Top    = fromDouble 0
    
{-

instance Coerce AbstractUndefined AbstractDouble where
    coerce Undefined.Bottom = bottom
    coerce Undefined.Top    = fromDouble 0 
-}

-- To AbstractBool
                                       
instance Coerce AbstractValue AbstractBool where
    coerce value = L.foldr join bottom [ coerce (address value)
                                       , coerce (integer value) 
                                       , coerce (double value)
                                       , bool value 
                                       , coerce (string value)
                                       , coerce (resource value)
                                       , coerce (null value)
                                       --, coerce (undefined value)
                                       ]

instance Coerce AbstractAddress AbstractBool where
    coerce = S.map coerce

instance Coerce AbstractInteger AbstractBool where
    coerce (AI set) = S.map coerce set
    
instance Coerce AbstractDouble AbstractBool where
    coerce (AD set) = S.map coerce set
        
instance Coerce AbstractString AbstractBool where
    coerce String.Bottom            = bottom
    coerce (Value str) | str == "0" = S.singleton False
                       | otherwise  = S.singleton True
    coerce String.Top               = anyBool 
        
instance Coerce AbstractResource AbstractBool where
    coerce = S.map coerce
    
instance Coerce AbstractNull AbstractBool where
    coerce Null.Bottom = bottom
    coerce Null.Top    = S.singleton False
    
    {-
instance Coerce AbstractUndefined AbstractBool where
    coerce Undefined.Bottom = bottom
    coerce Undefined.Top    = S.singleton False
-}
    
                                                                    
                                       
-- To AbstractString                                       
                                       
instance Coerce AbstractValue AbstractString where
    coerce value = L.foldr join bottom [ coerce (integer value)
                                       , coerce (double value)
                                       , coerce (bool value)
                                       , string value
                                       , coerce (resource value)
                                       , coerce (null value)
                                       -- , coerce (undefined value)
                                       ]
                                       
instance Coerce AbstractInteger AbstractString where
    coerce (AI set) = S.fold join bottom . S.map coerce $ set 
    
instance Coerce AbstractDouble AbstractString where
    coerce (AD set) = S.fold join bottom . S.map coerce $ set 
    
instance Coerce AbstractBool AbstractString where
    coerce = S.fold join bottom . S.map coerce
    
instance Coerce AbstractResource AbstractString where
    coerce = S.fold join bottom . S.map coerce
               
instance Coerce AbstractNull AbstractString where
    coerce Null.Bottom = bottom
    coerce Null.Top    = fromString ""

{-
instance Coerce AbstractUndefined AbstractString where
    coerce Undefined.Bottom = bottom
    coerce Undefined.Top    = fromString ""
                                                   
  -}                                     

-- To AbstractResource                                       

instance Coerce AbstractValue AbstractResource where
    coerce = resource 


-- To AbstractNull

instance Coerce AbstractValue AbstractNull where
    coerce = null


-- To AbstractUnset

instance Coerce AbstractValue AbstractUnset where
    coerce = unset
    

-- To AbstractUndefined        
{-
instance Coerce AbstractValue AbstractUndefined where
    coerce = undefined
-}


instance Coerce AbstractValue AbstractNumeric where
    coerce value = (integer value, double value)
    

-- Auxillarily

instance Coerce Bool Sign where
    coerce True  = Positive 
    coerce False = Zero
    
instance Coerce Resource Sign where
    coerce = const Positive
    
instance Coerce Resource Bool where
    coerce = const True    
    
instance Coerce Sign Bool where
    coerce Positive = True
    coerce Zero     = False
    coerce Negative = True 
    
instance Coerce Sign AbstractInteger where
    coerce Negative = AI $ S.fromList [Negative,Zero]
    coerce Zero     = AI $ S.singleton Zero
    coerce Positive = AI $ S.fromList [Zero,Positive]
    
instance Coerce Resource AbstractString where
    coerce = const String.Top
    
instance Coerce Bool AbstractString where
    coerce True  = fromString "1"
    coerce False = fromString "0"
    
instance Coerce Sign AbstractString where
    coerce Negative = anyString
    coerce Zero     = fromString "0"
    coerce Positive = anyString 
    
instance Coerce HContext Sign where
    coerce = const (fromNumeric 1)
    
instance Coerce HContext Bool where
    coerce = const True        