module Metric where

import Control.Arrow
import Data.Maybe
import Data.List as L
import Data.Set as S
import Data.Map as M
import Control.DeepSeq
import CCO.Printing  
import ResultTable  
import Analysis.Lattice.AbstractAddress
import Analysis.Lattice.AbstractValue
import Analysis.Lattice.ValueMap
import Analysis.SensitivitySpec
import Analysis.Typing
import Analysis.Type as T
import PHP.IR hiding (Type)
import PHP.Interface
import Debug.Trace
import Framework.Solve
import Framework.Lattice as L
import qualified Data.Foldable as FA

data Metric = Metric {
      numberOfSingletons           :: Int
    , numberOfUnionTypesWithoutCollapsing :: Int
    , numberOfUnionTypesWithCollapsing :: Int
    , numberOfPolymorphicTypes     :: Int
    , averageVarPointsTo           :: Double
    , numberOfPolymorphicCallSites :: Int
    , numberOfCallGraphEdges       :: Int
    , numberOfCallGraphNodes       :: Int
    , numberOfContexts             :: Int
    , numberOfIterations           :: Int
    , numberOfPreciseMatches       :: Int
    , numberOfAssignments          :: Int
} deriving (Show, Eq, Ord)

instance NFData Metric where
    rnf (Metric numberOfSingletons
                numberOfUnionTypesWithoutCollapsing
                numberOfUnionTypesWithCollapsing
                numberOfPolymorphicTypes
                averageVarPointsTo
                numberOfPolymorphicCallSites
                numberOfCallGraphEdges
                numberOfCallGraphNodes
                numberOfContexts
                numberOfIterations
                numberOfPreciseMatches
                numberOfAssignments) = rnf numberOfSingletons
                                    `seq` rnf numberOfUnionTypesWithoutCollapsing
                                    `seq` rnf numberOfUnionTypesWithCollapsing
                                    `seq` rnf numberOfPolymorphicTypes
                                    `seq` rnf averageVarPointsTo
                                    `seq` rnf numberOfPolymorphicCallSites
                                    `seq` rnf numberOfCallGraphEdges
                                    `seq` rnf numberOfCallGraphNodes
                                    `seq` rnf numberOfContexts
                                    `seq` rnf numberOfIterations
                                    `seq` rnf numberOfPreciseMatches
                                    `seq` rnf numberOfAssignments


toMetric graph result observed = 
    let assignmentTable = toAssignmentTable graph (snd result)
        callSiteTable   = toCallSiteTable graph (snd result)
        
        typeSetTable    = toTypeSetTable graph assignmentTable
    in Metric { 
           numberOfSingletons                  = calculateNumberOfSingleton typeSetTable
         , numberOfUnionTypesWithoutCollapsing = calculateNumberOfUnionTypesWithoutCollapsing typeSetTable
         , numberOfUnionTypesWithCollapsing    = calculateNumberOfUnionTypesWithCollapsing graph typeSetTable
         , numberOfPolymorphicTypes            = 0
         , averageVarPointsTo                  = calculateAverageVarPointsTo assignmentTable
         , numberOfPolymorphicCallSites        = calculateNumberOfPolymorphicCallSites graph callSiteTable
         , numberOfCallGraphEdges              = calculateNumberOfCallGraphEdges graph (fst . fst $ result)
         , numberOfCallGraphNodes              = 0 --calculateNumberOfCallGraphNodes graph (fst . fst $ result)
         , numberOfContexts                    = trace (show . S.map snd . M.keysSet . snd $ result) $ numberOfDistinctContextElements (snd result) 
         , numberOfIterations                  = 0
         , numberOfPreciseMatches              = calculateNumberOfPreciseMatches typeSetTable observed
         , numberOfAssignments                 = M.size observed
       }


-- calculateNumberOfPreciseMatches :: TypeSetTable -> TypeSetTable -> Int

calculateNumberOfPreciseMatches (ResultTable a) b = M.size . M.filter id . M.intersectionWith (\(Just inferred) observed -> inferred == observed) (M.fromList a) $ b 

calculateNumberOfCallGraphEdges :: Graph -> DynamicWorkMap -> Int
calculateNumberOfCallGraphEdges graph (DWM dwm) = M.fold (+) 0 
                                                . M.map (S.size . S.filter (isEntryLabel graph) . S.map (fst . endLabel))
                                                . M.filterWithKey (\k v -> isCallLabel graph k) 
                                                . M.mapKeysWith L.join fst 
                                                $ dwm
    
-- This function returns the number of assignments for which the type
-- analysis could infer a single type. Remember that due to the dynamic
-- nature of PHP this is not always possible.
calculateNumberOfSingleton :: TypeSetTable -> Int    
calculateNumberOfSingleton = L.length
                           . L.filter ((==) 1 . S.size . snd)
                           . cleanTable
                        

-- This function returns the number of assignments for which the type
-- analysis infers a union type. 
calculateNumberOfUnionTypesWithoutCollapsing :: TypeSetTable -> Int    
calculateNumberOfUnionTypesWithoutCollapsing = L.length
                                             . L.filter ((\x -> x > 1) . S.size . snd)
                                             . cleanTable                        



calculateNumberOfUnionTypesWithCollapsing :: Graph -> TypeSetTable -> Int    
calculateNumberOfUnionTypesWithCollapsing g = L.length
                                            . L.filter ((\x -> x > 1) . S.size . ignoreNull . collapse g . snd)
                                            . cleanTable





collapse :: Graph -> Set Type -> Set Type 
collapse g = S.fold collapse' S.empty
    where
        collapse' (Object ty) acc = S.insert (Object $ getAncestor g ty) acc
        collapse' ty          acc = S.insert ty acc


ignoreNull :: Set Type -> Set Type
ignoreNull xs = let xs' = S.delete T.Null xs
                in if S.size xs' > 0 && FA.all isObject xs' then xs' else xs


--calculateNumberOfPolymorphicTypes .. = L.length 
--                                     . L.filter (maybe False (isPolymorphic graph) . snd)
--                                     
--isPolymorphic :: Graph -> Set Type -> Bool
--isPolymorphic graph typeSet | S.all isObject = not . S.empty . S.map (\(Object ty) -> getSubtypes graph ty) $ typeSet
--                            | otherwise      = False
--                            
--                   

-- This function returns the number of method call sites for
-- which the type analysis could not infer a unique receiver method.
calculateNumberOfPolymorphicCallSites :: Graph -> CallSiteTable -> Int
calculateNumberOfPolymorphicCallSites graph = L.length
                                            . L.filter ((<) 1 . S.size)
                                            . L.map (S.map fromJust)
                                            . L.map (S.filter isJust)
                                            . L.map (S.map (resolve graph))
                                            . L.map (\(hctxs,name) -> S.map (\hctx -> Method (typeByAddress graph hctx) name) hctxs)
                                            . L.map (first addresses)
                                            . L.map snd
                                            . cleanTable
                            

calculateAverageVarPointsTo :: AbstractValueTable -> Double
calculateAverageVarPointsTo = average 
                            . L.map S.size
                            . L.map (S.map head . addresses)
                            . L.filter isAddress
                            . L.map snd
                            . cleanTable
                            where
                                average xs = realToFrac (sum xs) / genericLength xs

instance Printable Metric where
    pp metric = text "# of Singleton Types:" 
            >#< showable (numberOfSingletons metric)
            >-< text "# of Union Types (Without collapsing): "
            >#< showable (numberOfUnionTypesWithoutCollapsing metric)
            >-< text "# of Union Types (With collapsing):"
            >#< showable (numberOfUnionTypesWithCollapsing metric)
            >-< text "# of Polymorphic Call Sites:"
            >#< showable (numberOfPolymorphicCallSites metric)
            >-< text "# of Callgraph edges"
            >#< showable (numberOfCallGraphEdges metric)
            >-< text "Average var-points-to:" 
            >#< showable (averageVarPointsTo metric)
            >-< text "Number of context elements: "
            >#< showable (numberOfContexts metric)
            >-< text "Number of iterations: "
            >#< showable (numberOfIterations metric)
            