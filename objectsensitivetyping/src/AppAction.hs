module AppAction where
    
import Data.Map as M    
import Data.Maybe
import Data.IntMap as IM    
import Control.Monad.Reader
import Control.Monad.State
import Control.DeepSeq

import CCO.Printing

import AppConfig
import Metric
import ResultTable
import Test
import Analysis.ProgramState.Naive as NPS
import Analysis.ProgramState.WithReachable
import Analysis.Typing
import Native.NativeAPI as API
import PHP.Interface
import PHP.IR
import PHP.Parser
import Framework.Solve hiding (solve)
import Test.Parser

transformer text = do 
    program               <- parser text
    isNativeFunction      <- reader (API.isNativeFunction . nativeAPI)
    isNativeConstant      <- reader (API.isNativeConstant . nativeAPI)
    isNativeConstructor   <- reader (API.isNativeConstructor . nativeAPI)
    isNativeClassConstant <- reader (API.isNativeClassConstant . nativeAPI)
    returnTypesByFunction <- reader (API.returnTypesByFunction . nativeAPI)
    nativeClasses         <- reader (M.elems . API.classes . nativeAPI)
    return $ transform program isNativeFunction isNativeConstant isNativeConstructor isNativeClassConstant nativeClasses returnTypesByFunction

converter text = do 
    program <- transformer text
    return $ graph program

instrumenter text = do 
    program <- transformer text
    return $ instrument program

inferencer text = do 
    graph <- converter text
    solve (undefined::NPS) graph
    
filter labels text = do
    result <- inferencer text
    return $ M.filterWithKey (\k v -> flip elem labels . fst $ k) (snd result)

tester text = do
    graph    <- converter text
    result   <- inferencer text
    observed <- reader observed
    compareTypes graph (snd result) (fromJust observed)
    
debugger text = do
    graph <- converter text
    return $ IM.fold (++) [] (IM.mapWithKey (\l set -> show l ++ " :: " ++ show set ++ "\n") $ flow graph)

differ text = do
    graph  <- converter text
    result <- inferencer text
    return $ toTypeSetTable graph (toAssignmentTable graph (snd result))
    
metricCalculator text = do
    graph    <- converter text
    result   <- inferencer text
    state    <- get
    observed <- reader observed
    parsed   <- parseFileAndCollapse observed
    let metric = toMetric graph (force result) parsed
    return $ metric { numberOfIterations = M.foldr (+) 0 (counters state) } 