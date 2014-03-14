{-# LANGUAGE FlexibleInstances #-}
module Analysis.Lattice.AbstractAddress where
    
import Data.Set as S
import Data.List as L
import CCO.Printing as P hiding (join)
import Framework.Lattice
import Analysis.Type
import Analysis.SensitivitySpec

type AbstractAddress = Set HContext

isEmpty      = S.null
fromHContext = S.singleton

class Addressable a where
    addresses :: a -> AbstractAddress

instance Addressable AbstractAddress where
    addresses = id

instance Lattice AbstractAddress where
    bottom = S.empty
    join   = S.union
    (<:)   = S.isSubsetOf

instance Printable AbstractAddress where
    pp set = sepBy (L.map showable . S.toList $ set) (comma >|< space)

instance TypeSet AbstractAddress where
    typeSet g = S.map (Object . typeByAddress g)

