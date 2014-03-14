{-# LANGUAGE TypeOperators #-}
module Analysis.Lattice.ValueMap where
    
import CCO.Printing hiding (join)   
import Data.List as L
import Data.Map as M
import Data.Set as S
import Framework.Lattice
import PHP.IR
import Analysis.SensitivitySpec
import Analysis.ProgramState
    
type ValueMap loc ps = loc :-> ps

removeContext :: ProgramState ps => ValueMap (Label,Context) ps -> ValueMap Label ps
removeContext = M.mapKeysWith join fst

numberOfDistinctContextElements :: ProgramState ps => ValueMap (Label,Context) ps -> Int
numberOfDistinctContextElements = S.size . S.map snd . M.keysSet


instance (Show loc, Printable ps, ProgramState ps) => Printable (ValueMap loc ps) where
    pp = above . L.map (\(loc, value) -> text (padR 25 '-' (show loc ++ " ")) >|< space >|< pp value) . M.toList
        where
            padR :: Int -> Char -> String -> String
            padR n pad str | length str < n = str ++ replicate (n - length str) pad
                           | otherwise      = take n str