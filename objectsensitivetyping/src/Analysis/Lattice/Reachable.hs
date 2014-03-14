module Analysis.Lattice.Reachable where

import Data.Set as S
import Framework.Lattice
import Control.DeepSeq        
import CCO.Printing as P
    
data Reachable = Bottom | Top deriving (Show, Eq, Ord)

instance Lattice Reachable where
    bottom = Bottom
    
    join Top    Top    = Top
    join Top    Bottom = Top
    join Bottom Top    = Top
    join Bottom Bottom = Bottom
    
instance Printable Reachable where 
    pp Bottom = P.empty
    pp Top    = text "⊤"

instance NFData Reachable where
    rnf Top    = ()
    rnf Bottom = ()

