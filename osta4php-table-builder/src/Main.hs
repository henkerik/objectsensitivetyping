
module Main where

import Data.String.Utils
import Data.Maybe 
import System.Exit
import Text.PrettyPrint
import Text.CSV
import Printing
import Types
import Debug.Trace
import System.Environment
        
        
buildReport :: CSV -> CSV -> Report
buildReport xs = snd 
               . foldr buildProject (Nothing,[]) 
               . map fromJust
               . filter isJust 
               . map buildMetric 
               . filter (\(x,y) -> x /= [""] || y /= [""])
               . zip xs

buildMetric :: (Record,Record) -> Maybe (ProjectName, Metric)
buildMetric (precision, benchmark) = if name !! 0 == "gc" 
                                     then Nothing
                                     else Just (projectName, metric)
    where
        name = split "/" (benchmark !! 0)

        projectName = name !! 1
        metricName  = name !! 2
        
        
        metric = if trace (show precision) $ precision !! 1 == "DNF" 
                 then createUnfinishedMetric metricName
                 else let executionTime                       = read (benchmark !! 1)
                          averageVarPointsTo                  = read (precision !! 1)
                          numberOfUnionTypesWithoutCollapsing = read (precision !! 2)
                          numberOfUnionTypesWithCollapsing    = read (precision !! 3)
                          numberOfPolymorphicCallSites        = read (precision !! 4)
                          numberOfCallGraphEdges              = read (precision !! 5)
                          numberOfContexts                    = read (precision !! 6)
                          numberOfIterations                  = read (precision !! 7)
                          numberOfPreciseMatches              = read (precision !! 8)
                      in createFinishedMetric 
                          metricName
                          executionTime
                          averageVarPointsTo
                          numberOfUnionTypesWithoutCollapsing
                          numberOfUnionTypesWithCollapsing
                          numberOfPolymorphicCallSites
                          numberOfCallGraphEdges
                          numberOfContexts
                          numberOfIterations
                          numberOfPreciseMatches
            
buildProject :: (ProjectName,Metric) -> (Maybe ProjectName, Report) -> (Maybe ProjectName, Report)
buildProject (name,metric) (Nothing, [])      = (Just name, [createProject name metric])
buildProject (name,metric) (Just name', p:ps) = if name == name' 
                                                then (Just name, addMetric p metric:ps)
                                                else (Just name, createProject name metric:p:ps)

cleanUp (MatchReport xs) = MatchReport $ filterMetrics ["insensitive", "1obj", "1obj+1H", "2plain+1H", "2full+1H"] xs


processReport PlainVSFull = filterMetrics ["insensitive", "1obj", "1obj+1H", "2plain+1H", "2full+1H"]
processReport Type        = filterMetrics ["1obj+1H", "1type1obj+1H", "2full+1H", "2type+1H"]

data Mode = PlainVSFull
          | Type
          | GC
          | Matches
    
main = do
    args <- getArgs
    mode <- case args of 
        ["PlainVSFull"] -> return PlainVSFull
        ["Type"]        -> return Type
        ["GC"]          -> return GC
        ["Matches"]     -> return Matches
        otherwise       -> print "Unknown mode. " >> exitFailure

    report <- execute mode
    (putStrLn . render) report


execute Matches = do precisionMetrics <- parseCSVFromFile "precision.txt" >>= check
                     return $ (pp . cleanUp) (buildMatchReport precisionMetrics)
execute GC      = do benchmarkMetrics <- parseCSVFromFile "benchmark.txt" >>= check
                     return $ pp (buildGCReport benchmarkMetrics)
execute mode    = do precisionMetrics <- parseCSVFromFile "precision.txt" >>= check
                     benchmarkMetrics <- parseCSVFromFile "benchmark.txt" >>= check
                     return $ (pp . processReport mode) (buildReport precisionMetrics benchmarkMetrics)

check (Left e)  = print e >> exitFailure
check (Right r) = return (tail r)

buildGCReport :: CSV -> GCReport
buildGCReport = GCReport 
              . snd
              . foldr buildProject (Nothing,[]) 
              . map fromJust
              . filter isJust 
              . map buildGCMetric 
              . filter (\x -> x /= [""])


buildGCMetric :: Record -> Maybe (ProjectName,Metric)
buildGCMetric benchmark = if name !! 0 == "gc" 
                          then Just (projectName, metric)
                          else Nothing
    where
        name = split "/" (benchmark !! 0)

        projectName = name !! 1
        metricName  = name !! 2

        metric = GCMetric metricName (read $ benchmark !! 1)


buildMatchReport :: CSV -> MatchReport
buildMatchReport = MatchReport
                 . snd
                 . foldr buildProject (Nothing,[])
                 . map buildMatchMetric
                 . filter (\x -> x /= [""])


buildMatchMetric :: Record -> (ProjectName,Metric)
buildMatchMetric precision = trace (show precision) (projectName, metric)
  where
     name = split "/" (precision !! 0)

     projectName = name !! 0
     metricName  = name !! 1

     metric = if precision !! 1 == "DNF" 
              then createUnfinishedMetric metricName
              else MatchMetric metricName (read $ precision !! 8)

