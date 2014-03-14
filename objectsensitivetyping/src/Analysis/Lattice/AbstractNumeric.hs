module Analysis.Lattice.AbstractNumeric where

import Framework.Lattice
    
import Analysis.Lattice.AbstractInteger
import Analysis.Lattice.AbstractDouble
    
type AbstractNumeric = (AbstractInteger, AbstractDouble)


liftNumeric f (AI ai, AD ad) (AI ai', AD ad') = (AI $ f ai ai', AD $ f ai ad' `join` f ad ai' `join` f ad ad')