module ResultTable where

import Control.Arrow
import Data.List as L
import Data.IntMap as IM
import Data.Map as M
import Data.Set as S
import Data.Maybe as MB
import CCO.Printing as P
import Analysis.Type
import Analysis.ProgramState
import Analysis.Lattice.AbstractAddress
import Analysis.Lattice.AbstractValue
-- import Analysis.Lattice.AbstractUndefined as Undefined
import Analysis.Lattice.ValueMap
import Analysis.Inject
import Analysis.SensitivitySpec
import PHP.IR as IR hiding (Type)

-- Presents the analysis result in a more suitable format to calculate the precision metrics and to display the diff output

newtype ResultTable a = ResultTable { table :: [(Label, Maybe a)] } deriving (Show,Eq,Ord)

cleanTable :: ResultTable a -> [(Label,a)]
cleanTable = L.map (second fromJust) . L.filter (MB.isJust . snd) . table 

instance Functor ResultTable where
    fmap f = ResultTable . L.map (second $ fmap f) . table

instance Printable a => Printable (ResultTable a) where
    pp = above . L.map (\(l,value) -> showable l >|< text "::" >|< maybe (text "Undefined") pp value) . table

type TypeSetTable        = ResultTable (Set Type)
type AbstractValueTable  = ResultTable AbstractValue
type CallSiteTable       = ResultTable (AbstractValue, MethodName)


-- Returns a table with the analysis result for each assigned variable
toAssignmentTable :: ProgramState ps => Graph -> ValueMap (Label,HContext) ps -> AbstractValueTable
toAssignmentTable graph valueMap = ResultTable $ L.map lookup (IM.toList $ IR.assignments graph)
    where
        lookup (l,v) = (l, fmap (lookupVar v) (M.lookup l $ removeContext valueMap))


toTypeSetTable :: Graph -> AbstractValueTable -> TypeSetTable
toTypeSetTable graph endUser = fmap (typeSet graph) endUser

-- Returns a table with the analysis result for each receiver object tupled together with the method invoked
toCallSiteTable :: ProgramState ps => Graph -> ValueMap (Label,HContext) ps -> CallSiteTable
toCallSiteTable graph valueMap = ResultTable $ L.map lookup (IM.toList $ IR.methodCallSites graph)
    where
        lookup (l,(v,name)) = (l,(fmap (\ps -> (lookupVar "this" ps, name)) (M.lookup l $ removeContext valueMap)))