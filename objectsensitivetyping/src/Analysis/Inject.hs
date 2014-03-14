{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Analysis.Inject where

import Prelude hiding (null, undefined, fromInteger)
import Data.Set hiding (null)
import Framework.Lattice
import Analysis.Lattice.AbstractValue
import Analysis.Lattice.AbstractAddress
import Analysis.Lattice.AbstractInteger
import Analysis.Lattice.AbstractDouble
import Analysis.Lattice.AbstractBool
import Analysis.Lattice.AbstractString
import Analysis.Lattice.AbstractResource
import Analysis.Lattice.AbstractNull
import Analysis.Lattice.AbstractUnset
-- import Analysis.Lattice.AbstractUndefined
import Analysis.Lattice.AbstractNumeric

import Analysis.Resource
import Analysis.SensitivitySpec
    
class Inject a where
    inject :: a -> AbstractValue

instance Inject AbstractAddress where
    inject v = bottom { address = v }

instance Inject AbstractInteger where
    inject v = bottom { integer = v }
    
instance Inject AbstractDouble where
    inject v = bottom { double = v }
    
instance Inject AbstractBool where
    inject v = bottom { bool = v }

instance Inject AbstractString where
    inject v = bottom { string = v }

instance Inject AbstractResource where
    inject v = bottom { resource = v }

instance Inject AbstractNull where
    inject v = bottom { null = v}

instance Inject AbstractUnset where
    inject v = bottom { unset = v }
    
{-
instance Inject AbstractUndefined where
    inject v = bottom { undefined = v }
-} 

instance Inject AbstractNumeric where
    inject (left, right) = bottom { integer = left, double = right }    
    
instance Inject Bool where
    inject = inject . fromBool
    
instance Inject Integer where
    inject = inject . fromInteger
    
instance Inject Double where
    inject = inject . fromDouble
        
instance Inject String where
    inject = inject . fromString
    
instance Inject Resource where
    inject = inject . fromResource
    
instance Inject HContext where
    inject = inject . fromHContext    
        