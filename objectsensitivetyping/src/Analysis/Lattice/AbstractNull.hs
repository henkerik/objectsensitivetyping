module Analysis.Lattice.AbstractNull where
    
import Data.Set as S
import Control.DeepSeq        
import Framework.Lattice
import CCO.Printing as P
import Analysis.Type
    
data AbstractNull = Bottom | Top deriving (Show, Eq, Ord)

instance Lattice AbstractNull where
    bottom = Bottom
    
    join Top    Top    = Top
    join Top    Bottom = Top
    join Bottom Top    = Top
    join Bottom Bottom = Bottom

instance Printable AbstractNull where 
    pp Bottom = P.empty
    pp Top    = text "⊤"
    
instance NFData AbstractNull where
    rnf Top    = ()
    rnf Bottom = ()
    
instance TypeSet AbstractNull where
    typeSet g Top    = S.singleton Null
    typeSet g Bottom = S.empty