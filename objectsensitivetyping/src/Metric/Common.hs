module Metric.Common where
        
import CCO.Printing        
import Data.Text
import Data.Text.IO
import qualified Prelude as P
import Prelude hiding (readFile)
import System.Directory
import System.FilePath
import System.IO hiding (readFile)
import System.Process
    
import App
import AppAction
import AppConfig
import PHP.Parser
import Native.NativeAPI


--skip "phpgeo" "insensitive" = True
--skip "phpgeo" "1obj"        = True
--skip "phpgeo" "1obj+1H"     = True
skip _        _             = False


printer :: (Monad m, Printable a) => a -> m String
printer = return . render_ 79 . pp

projects apiFile = do 
    raytracer   <- simplify apiFile "projects/raytracer/source.php"
    gaufrette   <- simplify apiFile "projects/gaufrette/source.php"
    phpgeo      <- simplify apiFile "projects/phpgeo/source.php"
    mime        <- simplify apiFile "projects/mime/source.php"
    mvc         <- simplify apiFile "projects/mvc/source.php"
    dijkstra    <- simplify apiFile "projects/dijkstra/source.php"
    floyd       <- simplify apiFile "projects/floyd/source.php"
    interpreter <- simplify apiFile "projects/interpreter/source.php"
    
    return [("raytracer",   raytracer)
           ,("gaufrette",   gaufrette)
           ,("phpgeo",      phpgeo)
           ,("mime",        mime)
           ,("mvc",         mvc)
           ,("dijkstra",    dijkstra)
           ,("floyd",       floyd)
           ,("interpreter", interpreter)
           ]
    
simplify :: FilePath -> FilePath -> IO (Text,String)
simplify apiFile source = do 
    currentDir <- getCurrentDirectory
    let tmpFile = currentDir </> "tmp.php"
    let logFile = currentDir </> "log.txt"
    let instrumentedFile = currentDir </> "instrumented.php"

    pid <- runCommand $ "cat " ++ currentDir </> source ++ " | ./simplifier > " ++ tmpFile
    waitForProcess pid
    result <- readFile tmpFile    

    -- Instrument
    nativeAPI <- getNativeAPI apiFile
    let instrumented = case runStack (InstrumentAppConfig Nothing nativeAPI) (instrumenter result >>= printer) of
                        Left e  -> error . show $ e
                        Right m -> m

    P.writeFile instrumentedFile instrumented

    pid <- runCommand $ "cat " ++ instrumentedFile ++ " | php -d error_reporting=30711 > phplog.txt 2>&1 " 
    waitForProcess pid

    log <- P.readFile (currentDir </> logFile) 

    removeFile logFile
    removeFile tmpFile

    return (result, log)


runMetricCalculator nativeAPI enableGC isHintingEnabled projectName (text, observed) specName spec = 
    if skip projectName specName 
    then Nothing
    else case runStack (MetricAppConfig Nothing nativeAPI spec enableGC isHintingEnabled $ Just observed) (metricCalculator text) of
        Left e  -> error . show $ e
        Right m -> Just m