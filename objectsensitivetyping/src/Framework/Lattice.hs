{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Framework.Lattice where
    
import Data.Map as M

-- | The 'Lattice' type class is used to denote a join semi lattice.

class (Eq l) => Lattice l where
    bottom :: l
    join :: l -> l -> l
    (<:) :: l -> l -> Bool
    (<:) x y = x `join` y == y

  

instance (Lattice a, Lattice b) => Lattice (a,b) where
    bottom               = (bottom,bottom)
    join (la,lb) (ra,rb) = (join la ra, join lb rb)
    (<:) (la,lb) (ra,rb) = (la <: ra) && (lb <: rb)
    
instance (Lattice a, Lattice b, Lattice c) => Lattice (a,b,c) where
    bottom                     = (bottom,bottom,bottom)
    join (la,lb,lc) (ra,rb,rc) = (join la ra, join lb rb, join lc rc)
    (<:) (la,lb,lc) (ra,rb,rc) = (la <: ra) && (lb <: rb) && (lc <: rc)

type k :-> l = M.Map k l

instance (Lattice l, Ord k) => Lattice (k :-> l) where
    join = M.unionWith join
    (<:) = M.isSubmapOfBy (<:)
    bottom = M.empty