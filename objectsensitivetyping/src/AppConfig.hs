module AppConfig where

import Native.NativeAPI
import Analysis.SensitivitySpec
import PHP.IR    
    
data AppConfig = InferenceAppConfig  { inputFile :: Maybe FilePath, nativeAPI :: !NativeAPI, sensitivitySpecBuilder :: Graph -> SensitivitySpec, enableGC :: Bool, isHintingEnabled :: Bool }
               | MetricAppConfig     { inputFile :: Maybe FilePath, nativeAPI :: !NativeAPI, sensitivitySpecBuilder :: Graph -> SensitivitySpec, enableGC :: Bool, isHintingEnabled :: Bool, observed :: Maybe String }
               | BenchmarkAppConfig  { inputFile :: Maybe FilePath, nativeAPI :: !NativeAPI, sensitivitySpecBuilder :: Graph -> SensitivitySpec, enableGC :: Bool, isHintingEnabled :: Bool }
               | DumpAppConfig       { inputFile :: Maybe FilePath, nativeAPI :: !NativeAPI }
               | PrintAppConfig      { inputFile :: Maybe FilePath, nativeAPI :: !NativeAPI }
               | InstrumentAppConfig { inputFile :: Maybe FilePath, nativeAPI :: !NativeAPI }
               | DebugAppConfig      { inputFile :: Maybe FilePath, nativeAPI :: !NativeAPI }
               | TestAppConfig       { inputFile :: Maybe FilePath, nativeAPI :: !NativeAPI, sensitivitySpecBuilder :: Graph -> SensitivitySpec, enableGC :: Bool, isHintingEnabled :: Bool, observed :: Maybe String }
               


