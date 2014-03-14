{-# LANGUAGE FlexibleInstances #-}
module Analysis.Lattice.AbstractResource where

import Data.Set as S
import Data.List as L
import CCO.Printing
import Framework.Lattice
import Analysis.Resource
import Analysis.Type

anyResource = S.fromList [ BZ          
                         , CURL        
                         , CURLMulti   
                         , DBA         
                         , FTP         
                         , GD          
                         , GDFont      
                         , IMAP        
                         , File        
                         , ProcessFile 
                         , Directory   
                         , Context     
                         , Socket      
                         , Process     
                         , Zip         
                         , ZipEntry    
                         ]

fromResource = S.singleton

type AbstractResource = Set Resource

instance Lattice AbstractResource where
    join = S.union
    (<:) = S.isSubsetOf
    bottom = S.empty

instance Printable AbstractResource where
    pp set = sepBy (L.map showable . S.toList $ set) (comma >|< space)

instance TypeSet AbstractResource where
    typeSet g = S.map Resource