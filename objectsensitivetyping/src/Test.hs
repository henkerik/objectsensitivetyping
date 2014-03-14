{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
module Test where

import Data.Map as M
import Data.List as L
import Data.Char
import Debug.Trace
import Control.Monad.Error hiding (Error,join)

import CCO.Feedback hiding (trace, Error)
import CCO.Printing as P hiding (join)

import PHP.IR hiding (Null, Double, Bool, Integer)
import Analysis.Lattice.AbstractValue
import Analysis.Lattice.ValueMap
import Analysis.ProgramState
import Analysis.Resource
import Framework.Lattice
import Analysis.Type as Ty
import Test.Parser

contains g String        = isString
contains g Integer       = isInteger
contains g Double        = isDouble
contains g Bool          = isBool
contains g Null          = isNull
contains g (Resource rs) = isResource rs
contains g (Object name) = isClass g name

data Report = Success
            | Failure [Error]
            
instance Printable Report where
    pp Success          = text "SUCCESS"
    pp (Failure errors) = text "FAILURE" >-< (indent 4 . above . L.map pp $ errors)
    
instance Printable Error where
    pp (Error label var observed value) = text "Observed: "   >|< showable observed 
                                      >|< text " on label "   >|< showable label 
                                      >|< text " for var $"   >|< text var 
                                      >|< text ", Inferred: " >|< pp value

Success        <+> Success         = Success 
Success        <+> failure         = failure
failure        <+> Success         = failure
(Failure left) <+> (Failure right) = Failure $ left ++ right
    
            
data Error = Error Label Var Ty.Type AbstractValue

check g inferred (l,v,ty) = 
    let ps    = M.findWithDefault bottom l inferred in
    let value = lookupVar v ps in
    case contains g ty value of
        True  -> Nothing
        False -> Just $ Error l v ty value


aggregate :: [Maybe Error] -> Report 
aggregate = L.foldl (<+>) Success . L.map (maybe Success (Failure . (:[])))

compareTypes g inferred observed = 
    do let flattened = removeContext inferred
       parsed <- parseFile observed
       return $ aggregate (L.map (check g flattened) parsed)