module Benchmark where
    
import System.Environment
import Data.List as L    
import Criterion.Main as C
import Analysis.SensitivitySpec
import Metric.Common 
import Native.NativeAPI
import AppAction

main = do 
    nativeAPI <- getNativeAPI "nativeAPI.txt"
    projects <- projects "nativeAPI.txt"
    let textMime = case lookup "mime" projects of
                Nothing -> error "Should not happen"
                Just t  -> t
    let textRaytracer = case lookup "raytracer" projects of
                Nothing -> error "Should not happen"
                Just t  -> t
    let textInterpreter = case lookup "interpreter" projects of
                Nothing -> error "Should not happen"
                Just t  -> t
    let runIt' = runMetricCalculator nativeAPI True True
    let bench projectName text specName spec = C.bench specName $ nf (runIt' projectName text specName) spec
    let eachProject name text = bgroup name . map (uncurry $ bench name text) $ sensitivities
    defaultMain [
          bgroup "variations" (map (uncurry eachProject) projects )
        , bgroup "gc" [
              bgroup "mime" [
                  C.bench "enabled"  $ nf (runMetricCalculator nativeAPI True  True "mime" textMime "enabled")  twoFullOneH
                , C.bench "disabled" $ nf (runMetricCalculator nativeAPI False True "mime" textMime "disabled") twoFullOneH            
              ] 
            , bgroup "raytracer" [
                  C.bench "enabled"  $ nf (runMetricCalculator nativeAPI True  True "mime" textRaytracer "enabled")  twoFullOneH
                , C.bench "disabled" $ nf (runMetricCalculator nativeAPI False True "mime" textRaytracer "disabled") twoFullOneH
              ]
            , bgroup "interpreter" [
                  C.bench "enabled"  $ nf (runMetricCalculator nativeAPI True  True "mime" textInterpreter "enabled")  twoFullOneH
                , C.bench "disabled" $ nf (runMetricCalculator nativeAPI False True "mime" textInterpreter "disabled") twoFullOneH
              ]
        ]]

