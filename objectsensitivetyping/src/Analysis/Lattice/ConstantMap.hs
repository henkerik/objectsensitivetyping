{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Analysis.Lattice.ConstantMap where

import Control.DeepSeq
import Data.Map as M    
import PHP.IR
import Analysis.Lattice.AbstractValue
import Framework.Lattice
import CCO.Printing

data ConstantMapIndex = Default
                      | Named ConstantName
                      deriving (Show,Eq,Ord)
                      
instance NFData ConstantMapIndex where
    rnf (Default)    = ()
    rnf (Named name) = rnf name                      

type ConstantMap = M.Map ConstantMapIndex AbstractValue

instance Printable ConstantMap where
    pp heap = lbracket >|< space >|< sepBy (M.elems . M.mapWithKey (\idx value -> pp idx >|< text " :: " >|< pp value) $ heap) (comma >|< space) >|< space >|< rbracket

instance Printable ConstantMapIndex where
    pp Default      = text "[]"
    pp (Named name) = showable name