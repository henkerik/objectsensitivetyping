module Analysis.Lattice.AbstractUnset where
    
import Data.Set as S
import Control.DeepSeq        
import Framework.Lattice
import CCO.Printing as P
import Analysis.Type
    
data AbstractUnset = Bottom | Top deriving (Show, Eq, Ord)

instance Lattice AbstractUnset where
    bottom = Bottom
    
    join Top    Top    = Top
    join Top    Bottom = Top
    join Bottom Top    = Top
    join Bottom Bottom = Bottom
    
instance Printable AbstractUnset where 
    pp Bottom = P.empty
    pp Top    = text "⊤"

instance NFData AbstractUnset where
    rnf Top    = ()
    rnf Bottom = ()

instance TypeSet AbstractUnset where
    typeSet = (const . const) S.empty