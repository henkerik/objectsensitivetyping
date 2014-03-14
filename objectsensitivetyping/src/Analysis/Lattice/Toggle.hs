module Analysis.Lattice.Toggle where

import Control.DeepSeq
import CCO.Printing as P hiding (join)
import Framework.Lattice

data Toggle = Top | Bottom deriving (Show, Eq, Ord)

instance Lattice Toggle where
    bottom = Bottom
    
    join Top    Top    = Top
    join Top    Bottom = Top
    join Bottom Top    = Top
    join Bottom Bottom = Bottom

instance Printable Toggle where 
    pp Bottom = P.empty
    pp Top    = text "⊤"
    
instance NFData Toggle where
    rnf Top    = ()
    rnf Bottom = ()
    
