module Analysis.Lattice.AbstractDouble where
    
import Control.DeepSeq    
import Data.Set as S    
import Analysis.Lattice.Sign
import Framework.Lattice
import CCO.Printing hiding (join)
import Analysis.Type
    
data AbstractDouble = AD SignSet deriving (Show,Eq,Ord)

anyDouble = AD anySign

fromDouble :: Double -> AbstractDouble
fromDouble = AD . S.singleton . fromNumeric

instance Lattice AbstractDouble where
    bottom = AD bottom
    join (AD left) (AD right) = AD (join left right)
    (<:) (AD left) (AD right) = left <: right

instance Printable AbstractDouble where
    pp (AD ss) = pp ss
    
instance NFData AbstractDouble where
    rnf (AD set) = rnf set
    
instance TypeSet AbstractDouble where
    typeSet g (AD set) = if S.null set 
                         then S.empty 
                         else S.singleton Double
    
    