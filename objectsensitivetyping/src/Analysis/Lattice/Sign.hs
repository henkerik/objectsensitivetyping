{-# LANGUAGE FlexibleInstances #-}
module Analysis.Lattice.Sign where
    
import Control.DeepSeq    
import Prelude hiding (fromInteger)
import Data.Set as S
import Data.List as L
import CCO.Printing
import Analysis.Type
import Framework.Lattice

data Sign = Negative | Zero | Positive deriving (Eq, Ord)

instance Show Sign where
    show Negative = "-"
    show Zero     = "0"
    show Positive = "+"
    
instance NFData Sign where
    rnf Negative = ()    
    rnf Zero     = ()
    rnf Positive = ()
    
unaryLift :: (Ord e) => (e -> [e]) -> Set e -> Set e
unaryLift operator argument = L.foldl S.union S.empty [S.fromList $ operator e | e <- S.toList argument]

lift :: (Ord e) => (e -> e -> [e]) -> Set e -> Set e -> Set e
lift operator left right = L.foldl S.union S.empty [S.fromList $ operator l r | l <- S.toList left, r <- S.toList right]

plus Positive Positive = [Positive]
plus Positive Zero     = [Positive]
plus Positive Negative = [Negative, Zero, Positive]
plus Zero     Positive = [Positive]
plus Zero     Zero     = [Zero]
plus Zero     Negative = [Negative]
plus Negative Positive = [Negative, Zero, Positive]
plus Negative Zero     = [Negative]
plus Negative Negative = [Negative]

min Positive Positive = [Negative, Zero, Positive]
min Positive Zero     = [Positive]
min Positive Negative = [Positive]
min Zero     Positive = [Negative]
min Zero     Zero     = [Zero]
min Zero     Negative = [Positive]
min Negative Positive = [Negative]
min Negative Zero     = [Negative]
min Negative Negative = [Negative, Zero, Positive]

mul Positive Positive = [Positive]
mul Positive Zero     = [Zero]
mul Positive Negative = [Negative]
mul Zero     Positive = [Zero]
mul Zero     Zero     = [Zero]
mul Zero     Negative = [Zero]
mul Negative Positive = [Negative]
mul Negative Zero     = [Zero]
mul Negative Negative = [Positive]

div Positive Positive = [Positive]
div Positive Zero     = []              -- Special case: gives False in PHP!
div Positive Negative = [Negative]
div Zero     Positive = [Zero]
div Zero     Zero     = []              -- Special case: gives False in PHP!
div Zero     Negative = [Zero]
div Negative Positive = [Negative]
div Negative Zero     = []              -- Special case: gives False in PHP!
div Negative Negative = [Positive]

mod Positive Positive = [Zero,Positive]
mod Positive Zero     = []              -- Special case: gives False in PHP!
mod Positive Negative = [Zero,Positive]
mod Zero     Positive = [Zero]
mod Zero     Zero     = []              -- Special case: gives False in PHP!
mod Zero     Negative = [Zero]
mod Negative Positive = [Negative,Zero]
mod Negative Zero     = []              -- Special case: gives False in PHP!
mod Negative Negative = [Negative,Zero]

fromNumeric n 
    | n < 0     = Negative
    | n > 0     = Positive
    | otherwise = Zero
    
anySign = S.fromList [Negative, Zero, Positive]

type SignSet = Set Sign

instance Lattice SignSet where
    join = S.union
    (<:) = S.isSubsetOf
    bottom = S.empty
    
instance Printable SignSet where
    pp set = sepBy (L.map showable . S.toList $ set) (comma >|< space)
    
-- instance TypeSet SignSet where
--     typeSet g set = if S.null set 
--                     then S.empty
--                     else S.fromList [Int,Double]