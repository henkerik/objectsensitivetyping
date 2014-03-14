{-# LANGUAGE FlexibleInstances #-}
module Types where
   
import Data.List as L 
import Debug.Trace
   
type ProjectName = String
type MetricName  = String    

type Report         = [Project]
newtype GCReport    = GCReport [Project]    
newtype MatchReport = MatchReport [Project]

data Project = Project {
      projectName :: ProjectName
    , metrics     :: [Metric] 
}

data Metric = FinishedMetric {
      metricName                          :: MetricName
    , executionTime                       :: Double
    , averageVarPointsTo		              :: Double
    , numberOfUnionTypesWithoutCollapsing :: Integer
    , numberOfUnionTypesWithCollapsing    :: Integer
    , numberOfPolymorphicCallSites        :: Integer
    , numberOfCallGraphEdges              :: Integer
    , numberOfContexts                    :: Integer
    , numberOfIterations                  :: Integer
    , numberOfPreciseMatches              :: Integer
} | GCMetric { 
      metricName                          :: MetricName
    , executionTime                       :: Double
} | MatchMetric {
      metricName                          :: MetricName
    ,  numberOfPreciseMatches             :: Integer
} | UnfinishedMetric {
      metricName                          :: MetricName
}              

createProject :: ProjectName -> Metric -> Project
createProject name metric = Project name [metric]

createUnfinishedMetric = UnfinishedMetric
createFinishedMetric   = FinishedMetric

isFinishedMetric (FinishedMetric _ _ _ _ _ _ _ _ _ _) = True
isFinishedMetric otherwise                            = False

isUnfinishedMetric (UnfinishedMetric _) = True
isUnfinishedMetric otherwise            = False

addMetric :: Project -> Metric -> Project
addMetric (Project name metrics) metric = Project name (metric:metrics)

filterMetrics :: [MetricName] -> Report -> Report
filterMetrics xs = L.map (\project -> project { metrics = L.filter isSelected (metrics project)})
	where
		isSelected metric = L.elem (metricName metric) xs

metricNames :: Report -> [MetricName]
metricNames = L.map metricName . metrics . head


class MakeRelative a where
  makeRelative :: a -> a

instance MakeRelative ([] Project) where
  makeRelative = map makeRelative

instance MakeRelative Project where
  makeRelative p = p { metrics = makeRelative (metrics p) }

instance MakeRelative ([] Metric) where
  makeRelative = foldr f [] 
    where
      f :: Metric -> [Metric] -> [Metric]
      f m []     = [m]
      f m (x:xs) = m:(substract x m):xs 

substract :: Metric -> Metric -> Metric 
substract b   (UnfinishedMetric name)   = b
substract b m@(MatchMetric name number) = b {
  numberOfPreciseMatches = numberOfPreciseMatches b - number
}
substract b m@(FinishedMetric _ _ _ _ _ _ _ _ _ _) = b { 
  numberOfUnionTypesWithoutCollapsing = numberOfUnionTypesWithoutCollapsing b - numberOfUnionTypesWithoutCollapsing m,
  numberOfUnionTypesWithCollapsing    = numberOfUnionTypesWithCollapsing b - numberOfUnionTypesWithCollapsing m,
  numberOfPolymorphicCallSites        = numberOfPolymorphicCallSites b - numberOfPolymorphicCallSites m,
  numberOfCallGraphEdges              = numberOfCallGraphEdges b - numberOfCallGraphEdges m
}     
