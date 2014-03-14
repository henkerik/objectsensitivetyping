module Precision where

import Data.List as L
import Control.Arrow
import Data.Either  
import Analysis.SensitivitySpec
import Metric.Common
import Metric
import Native.NativeAPI
import AppAction
import CCO.Printing

newtype MetricTable = MetricTable { 
    groups :: [MetricGroup] 
} deriving Show

data MetricGroup = MetricGroup { 
    label   :: String
  , metrics :: [(String, Maybe Metric)] 
} deriving Show



instance Printable MetricTable where
    pp (MetricTable xs) = text "Name,UnionTypes,PolymorphicCallSites,CallGraphEdges,Contexts" 
                      >-< above (map pp xs)

instance Printable MetricGroup where
    pp (MetricGroup label metrics) = above (map f metrics)
        where
            f (spec, Nothing)     = text "\""
                                >|< text label 
                                >|< text "/"
                                >|< text spec
                                >|< text "\","
                                >|< text "DNF"
            f (spec, Just metric) = text "\"" 
                                >|< text label 
                                >|< text "/" 
                                >|< text spec 
                                >|< text "\"," 
                                >|< showable (averageVarPointsTo metric)
                                >|< text ","
                                >|< showable (numberOfUnionTypesWithoutCollapsing metric) 
                                >|< text ","
                                >|< showable (numberOfUnionTypesWithCollapsing metric)
                                >|< text ","
                                >|< showable (numberOfPolymorphicCallSites metric)
                                >|< text ","
                                >|< showable (numberOfCallGraphEdges metric)
                                >|< text ","
                                >|< showable (numberOfContexts metric)
                                >|< text ","
                                >|< showable (numberOfIterations metric)
                                >|< text ","
                                >|< showable (numberOfPreciseMatches metric)
                                >|< text ","
                                >|< showable (numberOfAssignments metric)


main = do 
    nativeAPI <- getNativeAPI "nativeAPI.txt"
    projects <- projects "nativeAPI.txt"
    let runIt' projectName text specName spec = runMetricCalculator nativeAPI True True projectName text specName spec
    let eachSpec projectName text specName spec = (,) specName $ runIt' projectName text specName spec        
    let eachProject name text = MetricGroup name . map (uncurry $ eachSpec name text) $ sensitivities
    let result = MetricTable $ map (uncurry eachProject) projects
    putStrLn . render_ 79 . pp $ result
  

