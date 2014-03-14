{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
module Analysis.Lattice.AbstractValue where

import Control.DeepSeq
import Prelude hiding (null,undefined)
import Data.Set as S hiding (null)
import Data.List as L hiding (null)
import CCO.Printing hiding (isEmpty,join)

import Analysis.Lattice.AbstractAddress
import Analysis.Lattice.AbstractInteger
import Analysis.Lattice.AbstractDouble
import Analysis.Lattice.AbstractBool hiding (not)
import Analysis.Lattice.AbstractString
import Analysis.Lattice.AbstractResource
import Analysis.Lattice.AbstractNull
import Analysis.Lattice.AbstractUnset
-- import Analysis.Lattice.AbstractUndefined

import Analysis.SensitivitySpec

import Analysis.Type
import Framework.Lattice

data AbstractValue = V { 
    address   :: AbstractAddress,
    integer   :: AbstractInteger, 
    double    :: AbstractDouble,
    bool      :: AbstractBool, 
    string    :: AbstractString, 
    resource  :: AbstractResource, 
    null      :: AbstractNull, 
    unset     :: AbstractUnset --,
    -- undefined :: AbstractUndefined
} deriving (Eq,Ord,Show)

isAddress :: AbstractValue -> Bool
isAddress = not . isEmpty . address

isInteger      = (/=) bottom . integer
isDouble       = (/=) bottom . double
isBool         = (/=) bottom . bool
isString       = (/=) bottom . string
isResource rs =  S.member rs . resource
isNull         = (/=) bottom . null
isUnset        = (/=) bottom . unset
-- isUndefined    = (/=) bottom . undefined
isClass g name = S.member name . S.map (typeByAddress g) . address

-- HList
data Typeable = forall a . TypeSet a => Typeable (AbstractValue -> a)

pack :: TypeSet a => (AbstractValue -> a) -> Typeable
pack = Typeable

instance TypeSet AbstractValue where    
    typeSet g v = L.foldl S.union S.empty 
                . L.map (\(Typeable selector) -> typeSet g (selector v))
                $ [pack address, pack integer, pack double, pack bool, pack string, pack resource, pack null, pack unset] --, pack undefined]


instance Addressable AbstractValue where
    addresses = addresses . address

instance NFData AbstractValue where
    rnf (V address integer double bool string resource null unset) = rnf address 
                                                                         `seq` rnf integer
                                                                         `seq` rnf double
                                                                         `seq` rnf bool
                                                                         `seq` rnf string
                                                                         `seq` rnf resource
                                                                         `seq` rnf null
                                                                         `seq` rnf unset
                                                                       --  `seq` rnf undefined

instance Printable AbstractValue where
    pp value = lparen 
           >|< lbrace >|< pp (address value) >|< rbrace >|< comma
           >|< lbrace >|< pp (integer value) >|< rbrace >|< comma
           >|< lbrace >|< pp (double value) >|< rbrace >|< comma
           >|< lbrace >|< pp (bool value) >|< rbrace >|< comma
           >|< lbrace >|< pp (string value) >|< rbrace >|< comma
           >|< lbrace >|< pp (resource value) >|< rbrace >|< comma
           >|< lbrace >|< pp (null value) >|< rbrace >|< comma
           >|< lbrace >|< pp (unset value) >|< rbrace >|< comma
           -- >|< lbrace >|< pp (undefined value) >|< rbrace
           >|< rparen
           
instance Lattice AbstractValue where
    bottom = V { 
          address   = bottom 
        , integer   = bottom
        , double    = bottom
        , bool      = bottom
        , string    = bottom
        , resource  = bottom
        , null      = bottom
        , unset     = bottom 
        -- , undefined = bottom
    }
    
    join left right = V {
        address   = address left   `join` address right,
        integer   = integer left   `join` integer right,
        double    = double left    `join` double right,
        bool      = bool left      `join` bool right,
        string    = string left    `join` string right,
        resource  = resource left  `join` resource right,
        null      = null left      `join` null right,
        unset     = unset left     `join` unset right
         --,
        -- undefined = undefined left `join` undefined right
    }
    
    (<:) left right = (address left <: address right)
                   && (integer left <: integer right)
                   && (double left <: double right)
                   && (bool left <: bool right)
                   && (string left <: string right)
                   && (resource left <: resource right)
                   && (null left <: null right)
                   && (unset left <: unset right)
                 --  && (undefined left <: undefined right)