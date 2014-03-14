module Native.ToValue where

import Data.List as L
import Data.Set as S


-- import qualified Native.NativeType as NT

import Framework.Lattice
import Analysis.Lattice.AbstractValue
import Analysis.Lattice.AbstractAddress
import Analysis.Lattice.AbstractInteger
import Analysis.Lattice.AbstractDouble
import Analysis.Lattice.AbstractBool
import Analysis.Lattice.AbstractString
import Analysis.Lattice.AbstractNull as Null
import Analysis.Inject
import Analysis.SensitivitySpec

import PHP.IR hiding (value)
    
data InferenceValue = IV {
    normalVar :: S.Set TypeVar,
    keyVar    :: S.Set TypeVar,
    valueVar  :: S.Set TypeVar,
    normal    :: AbstractValue,
    key       :: AbstractValue,
    value     :: AbstractValue
} deriving (Show,Eq,Ord)

def :: InferenceValue
def = IV S.empty S.empty S.empty bottom bottom bottom    

class Merge a where
    merge :: a -> a -> a

instance Merge InferenceValue where
    merge left right = IV {
        normalVar = merge (normalVar left) (normalVar right),
        keyVar    = merge (keyVar left)    (keyVar right),
        valueVar  = merge (valueVar left)  (valueVar right),
        normal    = join  (normal left)    (normal right),
        key       = join  (key left)       (key right),
        value     = join  (value left)     (value right)
    }
    
instance Merge (Set TypeVar) where
    merge = S.union    

--instance Merge (Maybe a) where
--    merge other Nothing     = other
--    merge Nothing other     = other
--    merge (Just a) (Just b) = error "Impossible to merge two type variables."
--

class ToValue a where
    toValue :: HContext -> a -> InferenceValue

instance (Ord a, ToValue a) => ToValue (Set a) where
    toValue hctx = S.fold merge def . S.map (toValue hctx)

instance ToValue NativeTerminalType where
    toValue hctx NativeString              = def { normal = inject anyString }
    toValue hctx NativeBoolean             = def { normal = inject anyBool }
    toValue hctx NativeInteger             = def { normal = inject anyInteger }
    toValue hctx NativeDouble              = def { normal = inject anyDouble }
    toValue hctx NativeFloat               = def { normal = inject anyDouble }
    toValue hctx NativeNull                = def { normal = inject Null.Top }
    toValue hctx NativeTrue                = def { normal = inject True  }
    toValue hctx NativeFalse               = def { normal = inject False }
    toValue hctx (NativeResource resource) = def { normal = inject resource }
    toValue hctx (NativeVar u)             = def { normalVar = S.singleton u }
    toValue hctx (NativeObject ty)         = def { normal = inject (fromHContext hctx) }
    toValue hctx (NativeArray k v)         = let keyVar   = normalVar (toValue hctx k)
                                                 key      = normal    (toValue hctx k)
                                                 valueVar = normalVar (toValue hctx v)
                                                 value    = normal    (toValue hctx v)
                                             in def { keyVar = keyVar, key = key, valueVar = valueVar, value = value }