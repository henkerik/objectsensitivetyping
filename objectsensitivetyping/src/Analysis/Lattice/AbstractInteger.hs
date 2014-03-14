module Analysis.Lattice.AbstractInteger where

import Control.DeepSeq
import Data.Set as S    
import Analysis.Lattice.Sign
import Framework.Lattice
import CCO.Printing hiding (join)
import Analysis.Type
    
data AbstractInteger = AI SignSet deriving (Show,Eq,Ord)

anyInteger = AI anySign

fromInteger :: Integer -> AbstractInteger
fromInteger = AI . S.singleton . fromNumeric

instance Lattice AbstractInteger where
    bottom = AI bottom
    join (AI left) (AI right) = AI (join left right)
    (<:) (AI left) (AI right) = left <: right

instance Printable AbstractInteger where
    pp (AI set) = pp set
    
instance NFData AbstractInteger where
    rnf (AI set) = rnf set    
    
instance TypeSet AbstractInteger where
    typeSet g (AI set) = if S.null set 
                         then S.empty 
                         else S.singleton Integer