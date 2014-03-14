{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Analysis.Lattice.Heap where

import Data.Map as M
import Data.Set as S
import CCO.Printing
import Framework.Lattice
import Analysis.Lattice.Object
import Analysis.SensitivitySpec

type Heap = M.Map HContext Object

lookupObject :: Heap -> HContext -> Object
lookupObject = flip (M.findWithDefault bottom)

instance Printable Heap where
    pp heap = lbracket 
          >|< space 
          >|< above (M.elems . M.mapWithKey (\address object -> showable address >|< text " :: " >|< pp object) $ heap) 
          >|< space 
          >|< rbracket
