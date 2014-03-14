{-# OPTIONS_GHC -fno-cse #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Set as S 
import Data.IntMap as IM
import Data.Map as M
import Data.List as L
import Control.Monad.Reader
import Control.Monad.State
import Control.DeepSeq
import Data.Functor
import Data.Text hiding (take)
import Data.Text.IO hiding (putStrLn, readFile)
import qualified Data.Text.IO as TIO
import Prelude hiding (print,getContents,compare)
import System.Exit

import System.Console.CmdArgs.Implicit hiding (Mode)
import CCO.Printing as P

import qualified Native.NativeAPI as API
-- import qualified Native.NativeType as NT

import App
import AppConfig hiding (inputFile, enableGC, isHintingEnabled)
import AppAction as A
import Metric hiding (Metric)
import ResultTable
import qualified AppConfig as AC
import PHP.IR hiding (output, Type)
import qualified PHP.IR as IR

import PHP.Interface hiding (instrument)
import qualified PHP.Interface as I
import PHP.Parser 
import Analysis.SensitivitySpec hiding (record)
import Analysis.Typing
import Analysis.ProgramState
import Analysis.ProgramState.Naive
--import Analysis.ProgramState.Experimental

import Framework.Solve hiding(solve)
import Framework.Lattice
import Analysis.Lattice.ValueMap
import Analysis.Type
import Test

data Sensitivity = OneObj
                 | OneObjOneH
                 | TwoPlainOneH
                 | TwoFullOneH
                 | TwoTypeOneH
                 | OneTypeOneObjOneH
                 | Insensitive
                 deriving (Eq, Show, Data, Typeable, Read)

data Mode = Inference  { inputFile :: Maybe FilePath, apiFile :: Maybe FilePath, sensitivity :: Sensitivity, enableGC :: Bool, isHintingEnabled :: Bool }
          | Benchmark  { inputFile :: Maybe FilePath, apiFile :: Maybe FilePath, sensitivity :: Sensitivity, enableGC :: Bool, isHintingEnabled :: Bool }
          | Diff       { inputFile :: Maybe FilePath, apiFile :: Maybe FilePath, sensitivity :: Sensitivity, enableGC :: Bool, isHintingEnabled :: Bool }
          | Metric     { inputFile :: Maybe FilePath, apiFile :: Maybe FilePath, sensitivity :: Sensitivity, enableGC :: Bool, isHintingEnabled :: Bool, file :: Maybe FilePath }
          | Filtered   { inputFile :: Maybe FilePath, apiFile :: Maybe FilePath, sensitivity :: Sensitivity, enableGC :: Bool, isHintingEnabled :: Bool, labels :: [Label] }
          | Interflow  { inputFile :: Maybe FilePath, apiFile :: Maybe FilePath, sensitivity :: Sensitivity, enableGC :: Bool, isHintingEnabled :: Bool }
          | Dump       { inputFile :: Maybe FilePath, apiFile :: Maybe FilePath }
          | Print      { inputFile :: Maybe FilePath, apiFile :: Maybe FilePath }
          | Instrument { inputFile :: Maybe FilePath, apiFile :: Maybe FilePath }
          | Debug      { inputFile :: Maybe FilePath, apiFile :: Maybe FilePath }
          | Test       { inputFile :: Maybe FilePath, apiFile :: Maybe FilePath, sensitivity :: Sensitivity, enableGC :: Bool, isHintingEnabled :: Bool, file :: Maybe FilePath }
          deriving (Eq, Show, Data, Typeable)
          
apiFileMsg          = apiFile   := def  += help "Configuration file with native API" += typFile
inputFileMsg        = inputFile := def  += help "Input file to analyze" += typFile
labelsMsg           = labels    := def  += help "Labels to display"
enableGCMsg         = enableGC  := True += help "Enable abstract GC"
isHintingEnabledMsg = isHintingEnabled := True += help "Enable type hinting"

sensitivityMsg = enum_ sensitivity
    [
      atom OneObj            += help "1obj"
    , atom OneObjOneH        += help "1obj+1H"
    , atom TwoPlainOneH      += help "2plain+1H"
    , atom TwoFullOneH       += help "2full+1H"
    , atom TwoTypeOneH       += help "2type+1H"
    , atom OneTypeOneObjOneH += help "1type1obj+1H"
    , atom Insensitive       += help "insensitive"
    ]
fileMsg       = file := def += help "File with observed types" += typFile    

inference   = record Inference { enableGC = True } [inputFileMsg, apiFileMsg, sensitivityMsg, enableGCMsg, isHintingEnabledMsg] 
           += help "Run the inference" 
           += auto
           
benchmark   = record Benchmark { enableGC = True } [inputFileMsg, apiFileMsg, sensitivityMsg, enableGCMsg, isHintingEnabledMsg] 
           += help "Run the inferencer without displaying or verifying the output"
           
diff        = record Diff { enableGC = True } [inputFileMsg, apiFileMsg, sensitivityMsg, enableGCMsg, isHintingEnabledMsg] 
           += help "Run the inferencer and output the types in a diff friendly format"
           
metric      = record Metric { enableGC = True } [inputFileMsg, apiFileMsg, sensitivityMsg, enableGCMsg, isHintingEnabledMsg, fileMsg] 
           += help "Run the inferencer add display the precision metrics"
           
filtered    = record Filtered { enableGC = True } [inputFileMsg, apiFileMsg, sensitivityMsg, enableGCMsg, isHintingEnabledMsg, labelsMsg] 
           += help "Run the inferencer and display the analysis result for selected labels"
           
interflow   = record Interflow { enableGC = True } [inputFileMsg, apiFileMsg, sensitivityMsg, enableGCMsg, isHintingEnabledMsg]
           += help "Run the inferencer and print the interprocedural flow"

dump        = record Dump {} [inputFileMsg, apiFileMsg]
           += help "Dump the AST"
           
print       = record Print {} [inputFileMsg, apiFileMsg]
           += help "Pretty print the AST"
           
instrument  = record Instrument {} [inputFileMsg, apiFileMsg]
           += help "Instrument the AST"
           
debug       = record Debug {} [inputFileMsg, apiFileMsg]
           += help "Print debug information"
           
test        = record Test { enableGC = True } [inputFileMsg, apiFileMsg, sensitivityMsg, enableGCMsg, isHintingEnabledMsg, fileMsg]
           += help "Test the inferencer by comparing the observed and inferenced facts"

application = modes_ [inference,interflow,benchmark,diff,metric,filtered,dump,print,instrument,debug,test] 
           += program "osta4php"
           += summary "Object Sensitive Typing Analysis 4 PHP"


buildAppConfig :: Mode -> IO AppConfig
buildAppConfig (Inference inputFile apiFile sensitivity enableGC isHintingEnabled) = 
    do nativeAPI <- loadAPI apiFile
       return $ InferenceAppConfig inputFile nativeAPI (selectSensitivitySpec sensitivity) enableGC isHintingEnabled
buildAppConfig (Benchmark inputFile apiFile sensitivity enableGC isHintingEnabled) = 
    do nativeAPI <- loadAPI apiFile
       return $ InferenceAppConfig inputFile nativeAPI (selectSensitivitySpec sensitivity) enableGC isHintingEnabled
buildAppConfig (Diff inputFile apiFile sensitivity enableGC isHintingEnabled) = 
    do nativeAPI <- loadAPI apiFile
       return $ InferenceAppConfig inputFile nativeAPI (selectSensitivitySpec sensitivity) enableGC isHintingEnabled
buildAppConfig (Metric inputFile apiFile sensitivity enableGC isHintingEnabled file) =
    do nativeAPI <- loadAPI apiFile
       observed <- maybe (return Nothing) (\x -> Just <$> readFile x) file
       return $ InferenceAppConfig inputFile nativeAPI (selectSensitivitySpec sensitivity) enableGC isHintingEnabled
buildAppConfig (Filtered inputFile apiFile sensitivity enableGC isHintingEnabled labels) = 
    do nativeAPI <- loadAPI apiFile
       return $ InferenceAppConfig inputFile nativeAPI (selectSensitivitySpec sensitivity) enableGC isHintingEnabled
buildAppConfig (Interflow inputFile apiFile sensitivity enableGC isHintingEnabled) =
    do nativeAPI <- loadAPI apiFile
       return $ InferenceAppConfig inputFile nativeAPI (selectSensitivitySpec sensitivity) enableGC isHintingEnabled
buildAppConfig (Dump inputFile apiFile) = 
    do nativeAPI <- loadAPI apiFile
       return $ DumpAppConfig inputFile nativeAPI
buildAppConfig (Print inputFile apiFile) = 
    do nativeAPI <- loadAPI apiFile
       return $ PrintAppConfig inputFile nativeAPI                                        
buildAppConfig (Instrument inputFile apiFile) = 
    do nativeAPI <- loadAPI apiFile
       return $ InstrumentAppConfig inputFile nativeAPI
buildAppConfig (Debug inputFile apiFile) = 
    do nativeAPI <- loadAPI apiFile
       return $ DebugAppConfig inputFile nativeAPI
buildAppConfig (Test inputFile apiFile sensitivity enableGC isHintingEnabled file) = 
    do nativeAPI <- loadAPI apiFile
       observed <- maybe (return Nothing) (\x -> Just <$> readFile x) file
       return $ TestAppConfig inputFile nativeAPI (selectSensitivitySpec sensitivity) enableGC isHintingEnabled observed


selectSensitivitySpec :: Sensitivity -> Graph -> SensitivitySpec
selectSensitivitySpec OneObj            = oneObj
selectSensitivitySpec OneObjOneH        = oneObjOneH
selectSensitivitySpec TwoPlainOneH      = twoPlainOneH
selectSensitivitySpec TwoFullOneH       = twoFullOneH
selectSensitivitySpec TwoTypeOneH       = twoTypeOneH
selectSensitivitySpec OneTypeOneObjOneH = oneTypeOneObjOneH
selectSensitivitySpec Insensitive       = insensitive

loadAPI :: Maybe FilePath -> IO API.NativeAPI
loadAPI Nothing         = putStrLn "No API File" >> exitFailure
loadAPI (Just filePath) = API.getNativeAPI filePath

printer :: (Monad m, Printable a) => a -> m String
printer = return . render_ 79 . pp


buildApp (Inference inputFile apiFile sensitivity enableGC isHintingEnabled)        text = inferencer text >>= printer
buildApp (Interflow inputFile apiFile sensitivity enableGC isHintingEnabled)        text = inferencer text >>= (return . fst) >>= printer
buildApp (Benchmark inputFile apiFile sensitivity enableGC isHintingEnabled)        text = inferencer text >>= (\result -> return $ result `deepseq` "Finished")
buildApp (Diff      inputFile apiFile sensitivity enableGC isHintingEnabled)        text = differ text >>= printer
buildApp (Metric    inputFile apiFile sensitivity enableGC isHintingEnabled file)   text = metricCalculator text >>= printer
buildApp (Filtered  inputFile apiFile sensitivity enableGC isHintingEnabled labels) text = A.filter labels text >>= printer
buildApp (Dump inputFile apiFile)                                                   text = parser text >>= return . show
buildApp (Print inputFile apiFile)                                                  text = transformer text >>= printer
buildApp (Instrument inputFile apiFile)                                             text = instrumenter text >>= printer
buildApp (Debug inputFile apiFile)                                                  text = debugger text
buildApp (Test inputFile apiFile sensitivity enableGC isHintingEnabled file)        text = tester text >>= printer 

main = do options <- cmdArgs_ application          
          config <- buildAppConfig options
          let app = buildApp options
          ioWrap app config


-- Leave IO monad and enter application monad stack 
ioWrap :: (Text -> App String) -> AppConfig -> IO ()
ioWrap f config = do
    input  <- case AC.inputFile config of
        Nothing   -> getContents
        Just file -> TIO.readFile file
        
    let result = runStack config (f input)  
            
    case result of
        Left exception -> putStrLn (show exception) >> exitFailure
        Right a        -> putStrLn a >> exitSuccess
        

--instance Printable a => Printable (a, AppState) where
--    pp (x,appState) = text "Number of iterations: " >|< pp appState
--                  >-< text "Total number of iterations: " >|< showable (M.fold (+) 0 . counters $ appState)
--                  >-< pp x

