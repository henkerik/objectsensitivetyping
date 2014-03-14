{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
module Framework.Solve where

import Control.DeepSeq
import qualified Data.Set          as S
import qualified Data.Map          as M
import qualified Data.Map.Lazy     as ML
import qualified Data.List         as L
import qualified Data.Sequence     as SQ
import           CCO.Printing      as P hiding (join)
import           Framework.Lattice
import Control.Applicative
import Control.Monad hiding (join)
import Control.Monad.State hiding (join)
import Debug.Trace




-- padR :: Int -> Char -> String -> String
-- padR n pad str | length str < n = str ++ replicate (n - length str) pad
--                | otherwise      = take n str
-- 
instance (Printable global, Printable (loc :-> local)) => Printable (global, loc :-> local) where
    pp (global, valueMap) = text "Global"   >-< indent 4 (pp global) 
                        >-< text "ValueMap" >-< indent 4 (pp valueMap)
                        
--                         above . L.map (\(loc, value) -> text (padR 25 '-' (show loc ++ " ")) >|< space >|< pp value) . M.toList $ valueMap)



data Work loc = Unary loc loc
              | Binary loc loc loc
              deriving (Eq,Ord,Show)
              
instance NFData a => NFData (Work a) where
    rnf (Unary start end)         = rnf start `seq` rnf end
    rnf (Binary start start' end) = rnf start `seq` rnf start' `seq` rnf end
    
    
              
endLabel (Unary start end)         = end
endLabel (Binary start start' end) = end             
              
startLabel (Unary start end)         = start
startLabel (Binary start start' end) = start'

data Stats loc = Stats { counters :: M.Map (Work loc) Int } deriving (Show)

instance (Show loc, Ord loc) => Printable (Stats loc) where
    pp (Stats counters) = above (M.elems . M.mapWithKey (\label counter -> showable label >|< text " :: " >|< showable counter) $ counters) 
    
    -- . M.mapKeys startLabel $ counters)


initializeStats :: (MonadState (Stats loc) m) => m ()
initializeStats = put $ Stats { counters = M.empty }

updateStats :: (Ord loc, MonadState (Stats loc) m) => Work loc -> m ()
updateStats work = do stats <- get
                      let f Nothing  = Just 1
                          f (Just n) = Just (n + 1)
                      put $ Stats (M.alter f work (counters stats))
              
solve :: (Show loc, Ord loc, Lattice global, Lattice local, Applicative m, Monad m, Printable local, NFData global, NFData local, NFData loc, MonadState (Stats loc) m)
      => (loc -> Bool)                              -- isSkip
      -> (loc -> global -> [Work loc])
      -> (loc -> local -> global -> m global)
      -> (loc -> local -> m local)
      -> (loc -> loc -> local -> local -> m local)
      -> local
      -> global
      -> loc :-> local
      -> [Work loc]
      -> m (  global, loc :-> local)
  
solve isSkip next globalTransfer unaryLocalTransfer binaryLocalTransfer local global valueMap worklist = 
    do initializeStats
       solve' valueMap global (S.fromList worklist)
    where
        lookup ctx m = case M.lookup ctx m of 
            Just l  -> l
            Nothing -> local

        process valueMap work@(Unary start end) = do 
            let context  = lookup start valueMap   
            result <- unaryLocalTransfer start context
            return result 
--            trace (show work ++ "\nBefore\n" ++ (render_ 79 . pp) context ++ "\nAfter:\n" ++ (render_ 79 . pp) result) (return result)
        process valueMap work@(Binary start start' end) = do
            let context  = lookup start valueMap
            let context' = lookup start' valueMap
            binaryLocalTransfer start start' context context'
--            trace (show work) $ binaryLocalTransfer start start' context context'
                
        solve' valueMap global workset = 
            if S.null workset 
            then do valueMap' <- ML.traverseWithKey (\loc context -> unaryLocalTransfer loc context) valueMap
                    return (global, valueMap')
            else do let (work,worklistTail) = S.deleteFindMin workset
                    let end                 = endLabel work
                    let start               = startLabel work
                    let previous            = lookup end valueMap
                    updateStats work
                    effect  <- process valueMap work
                    if not (effect <: previous) || (M.notMember end valueMap && isSkip start)
                        then do global' <- globalTransfer end effect global
                                let worklist' = S.union worklistTail (S.fromList $ next end global') -- ++ worklistTail
                                let valueMap' = M.insertWith join end effect valueMap
                                solve' valueMap' global' worklist'
                                -- solve' (force valueMap') (force global') (force worklist')
                        else solve' valueMap global worklistTail
--                        else solve' valueMap global (force worklistTail)

