{-# LANGUAGE FlexibleInstances #-}
module Analysis.Lattice.AbstractBool where
    
import Data.Set as S
import Data.List as L
import CCO.Printing
import Analysis.Type
import Framework.Lattice

anyBool = S.fromList [True, False]
fromBool = S.singleton

type AbstractBool = Set Bool

instance TypeSet AbstractBool where
    typeSet g set = if S.null set 
                    then S.empty 
                    else S.singleton Bool

instance Lattice AbstractBool where
    join = S.union
    (<:) = S.isSubsetOf
    bottom = S.empty
    
instance Printable AbstractBool where
    pp set = sepBy (L.map showable . S.toList $ set) (comma >|< space)
    
not True  = [False]
not False = [True]

and True  True  = [True]
and False True  = [False]
and True  False = [False]
and False False = [False]

or True  True  = [True]
or False True  = [True]
or True  False = [True]
or False False = [False]

xor True  True  = [False]
xor False True  = [True]
xor True  False = [True]
xor False False = [False]
