{-# LANGUAGE TypeOperators #-}
module Analysis.ProgramState.Experimental where

import Data.Maybe    
import Data.Map as M
import Data.Set as S
import Data.List as L
import CCO.Printing hiding (join)

import Framework.Lattice
import Analysis.Inject
import Analysis.Index
import Analysis.Coerce
import Analysis.Identifier
import Analysis.ProgramState
--import Analysis.Lattice.Stack as Stack

--import Analysis.Lattice.Heap

import Analysis.Lattice.Null as Null
import Analysis.Lattice.Unset as Unset
import Analysis.Lattice.Address

import Analysis.Lattice.Value
import Analysis.Lattice.String
import Analysis.Lattice.ConstantMap

import PHP.IR
import Analysis.SensitivitySpec


data State = Processed
           | Unprocessed
           deriving (Eq,Ord,Show)

type SymbolTable = Identifier :-> ReferenceSet
type ZValueTable = Reference  :-> Value
type Heap        = HContext   :-> Object
type Object      = Index      :-> ReferenceSet


--getObject :: HContext -> Heap -> Object
getObject = M.findWithDefault bottom

--getField :: Index -> Object -> Set Identifier
getField = getFieldWithDefault bottom
getFieldWithDefault def = M.findWithDefault def

data UpdateKind = Weak Reference | Strong Reference

--writeZValue :: [Reference] -> Value -> ZValueTable -> ZValueTable
writeZValue useStrongUpdates references value table = (L.foldr (.) id . L.map process . L.map kind . S.toList $ references) table
    where
        kind ref | (S.size references > 1) = Weak ref
        kind ref | otherwise               = case coerce (M.findWithDefault bottom ref table) of 
            Unset.Bottom -> if useStrongUpdates then Strong ref else Weak ref
            Unset.Top    -> Weak ref

        process (Weak ref)   = M.insertWith join ref value
        process (Strong ref) = M.insert ref value


--readZValue :: [Reference] -> ZValueTable -> Value
readZValue references table | S.size references == 0 = inject Null.Top
                            | otherwise              = L.foldr join bottom 
                                                     . L.map (flip (M.findWithDefault (inject Null.Top)) table) 
                                                     . S.toList
                                                     $ references
    
    

instance Lattice (Set Reference) where
    bottom = S.empty
    join   = S.union
    (<:)   = S.isSubsetOf

data EPS = EPS {
    symbolTable :: SymbolTable,
    zValueTable :: ZValueTable,
    heap        :: Heap,
    constants   :: ConstantMap
} deriving (Eq, Ord, Show)

instance Lattice EPS where
    bottom = EPS bottom bottom bottom bottom
    (EPS symbolTable zValueTable heap cons) `join` (EPS symbolTable' zValueTable' heap' cons') = EPS (symbolTable `join` symbolTable') (zValueTable `join` zValueTable') (heap `join` heap') (cons `join` cons')
    (EPS symbolTable zValueTable heap cons) <:     (EPS symbolTable' zValueTable' heap' cons') = (symbolTable <: symbolTable') && (zValueTable <: zValueTable') && (heap <: heap') && (cons <: cons')

instance Printable (Set Reference) where
    pp set = lbrace >|< sepBy (L.map showable . S.toList $ set) (comma >|< space) >|< rbrace

instance Printable ZValueTable where
    pp table = lbracket >|< space >|< sepBy (M.elems . M.mapWithKey (\identifier value -> showable identifier >|< text " :: " >|< pp value) $ table) (comma >|< space) >|< space >|< rbracket

instance Printable SymbolTable where
    pp table = lbracket >|< space >|< sepBy (M.elems . M.mapWithKey (\identifier references -> showable identifier >|< text " :: " >|< pp references) $ table) (comma >|< space) >|< space >|< rbracket

instance Printable Heap where
    pp heap = lbracket >|< space >|< sepBy (M.elems . M.mapWithKey (\address object -> showable address >|< text " :: " >|< pp object) $ heap) (comma >|< space) >|< space >|< rbracket

instance Printable Object where
    pp fields = lbrace >|< space >|< sepBy (M.elems . M.mapWithKey (\index references -> pp index >|< colon >|< space >|< pp references) $ fields) (comma >|< space) >|< space >|< rbrace

instance Printable EPS where
    pp (EPS symbolTable zValueTable heap cons) = pp symbolTable >-< pp zValueTable >-< pp heap >-< pp cons

instance ProgramState EPS where
--    removeIdent ident st = error "should not be in use any more" 
    removeIdent ident st = st { symbolTable = symbolTable' }
        where
            symbolTable' = M.delete ident (symbolTable st)
    
    insertIdent ident value st = st { symbolTable = symbolTable', zValueTable = zValueTable' }
        where
            symbolTable' = M.alter alterSymbol ident (symbolTable st)
                where
                    alterSymbol Nothing           = Just (S.singleton $ Reference ident)
                    alterSymbol (Just references) = Just references

            references = fromJust . M.lookup ident $ symbolTable'
            zValueTable' = writeZValue True references value (zValueTable st)
                                     
    lookupIdent ident st = readZValue references (zValueTable st)
        where
            references = M.findWithDefault S.empty ident 
                       $ symbolTable st
    

    readField v idx hctx st = (st', readZValue references (zValueTable st'))
        where
            (st', references) = readFieldRef v idx hctx st
        
                                           
    writeField v idx value hctx st = st { symbolTable = symbolTable', zValueTable = zValueTable'', heap = heap' }
        where
            symbolTable' = M.alter alterSymbol (Variable v) (symbolTable st)
               where
                   alterSymbol Nothing           = Just (S.singleton $ Reference (Variable v))
                   alterSymbol (Just references) = Just references

            zValueTable' = S.fold (M.alter alterStore) (zValueTable st) . M.findWithDefault (error "should not happen") (Variable v) $ symbolTable'
               where
                   newValue = inject (fromHContext hctx)
       
                   alterStore Nothing      = Just newValue 
                   alterStore (Just value) = if isAddress value 
                       then Just value
                       else Just (value `join` newValue)

            receiver = lookupIdent (Variable v) (EPS symbolTable' zValueTable' (heap st) (constants st))

            heap' = S.fold (\hctx -> M.alter (alterObject hctx) hctx) (heap st) . coerce $ receiver
               where
                   alterObject hctx Nothing       = Just $ M.singleton idx (S.singleton $ IndexReference idx hctx)
                   alterObject hctx (Just object) = Just $ M.alter (alterField hctx) idx object
       
                   alterField hctx Nothing           = Just (S.singleton $ IndexReference idx hctx)
                   alterField hctx (Just references) = Just references
       
            zValueTable'' = writeZValue False (S.fromList references) value zValueTable'
                where
                    references = do hctx <- S.toList (coerce receiver)
                                    let object = getObject hctx heap'
                                    S.toList $ getField idx object

--    clearHeap                   st = error "Not yet implemented clearHeap" 
--    clearStack                  st = error "Not yet implemented clearStack"
--    varsToParams ps             st = error "Not yet implemented varsToParams"
--    paramsToVars ps             st = error "Not yet implemented paramsToVars"
    insertConstant v value      st = error "Not yet implemented insertConstant"
    lookupConstant name         st = error "Not yet implemented lookupConstant"
    
    

    readRef v st = M.findWithDefault S.empty (Variable v) (symbolTable st)
     

    writeRef v references st = st { symbolTable = symbolTable' }
        where
            symbolTable' = M.insert (Variable v) references (symbolTable st)


    readFieldRef v idx hctx st = (st { symbolTable = symbolTable', zValueTable = zValueTable'', heap = heap' }, S.fromList references)
        where 
            symbolTable' = M.alter alterSymbol (Variable v) (symbolTable st)
               where
                   alterSymbol Nothing           = Just (S.singleton $ Reference (Variable v))
                   alterSymbol (Just references) = Just references

            zValueTable' = S.fold (M.alter alterStore) (zValueTable st) . M.findWithDefault (error "should not happen") (Variable v) $ symbolTable'
               where
                   newValue = inject (fromHContext hctx)
       
                   alterStore Nothing      = Just newValue 
                   alterStore (Just value) = if isAddress value 
                       then Just value
                       else Just (value `join` newValue)
                       
            receiver = lookupIdent (Variable v) (EPS symbolTable' zValueTable' (heap st) (constants st))
            
            heap' = S.fold (\hctx -> M.alter (alterObject hctx) hctx) (heap st) . coerce $ receiver
               where
                   alterObject hctx Nothing       = Just $ M.singleton idx (S.singleton $ IndexReference idx hctx)
                   alterObject hctx (Just object) = Just $ M.alter (alterField hctx) idx object
       
                   alterField hctx Nothing           = Just (S.singleton $ IndexReference idx hctx)
                   alterField hctx (Just references) = Just references
            
            references = do hctx   <- S.toList (coerce receiver)
                            let object = getObject hctx heap'
                            S.toList $ getField idx object
                            
            zValueTable'' = L.foldr (M.alter alterStore) zValueTable' references
                where
                    alterStore Nothing      = Just (inject Null.Top)
                    alterStore (Just value) = Just value
        
    writeFieldRef v idx references hctx st = st { symbolTable = symbolTable', zValueTable = zValueTable', heap = heap' }
        where
            symbolTable' = M.alter alterSymbol (Variable v) (symbolTable st)
               where
                   alterSymbol Nothing           = Just (S.singleton $ Reference (Variable v))
                   alterSymbol (Just references) = Just references

            zValueTable' = S.fold (M.alter alterStore) (zValueTable st) . M.findWithDefault (error "should not happen") (Variable v) $ symbolTable'
               where
                   newValue = inject (fromHContext hctx)
       
                   alterStore Nothing      = Just newValue 
                   alterStore (Just value) = if isAddress value 
                       then Just value
                       else Just (value `join` newValue)

            receiver = lookupIdent (Variable v) (EPS symbolTable' zValueTable' (heap st) (constants st))
            
            heap' = S.fold (\hctx -> M.insertWith join hctx (M.singleton idx references)) (heap st) . coerce $ receiver
    

    
    varsToParams ps st = st { symbolTable = symbolTable' }
        where
            symbolTable' = M.filterWithKey filterKey
                         . M.mapKeys mapKey 
                         . symbolTable 
                         $ st
            
            mapKey (Variable x) = if x `elem` ps then Parameter (fromJust $ elemIndex x ps) else Variable x
            mapKey other        = other
            
            filterKey (Parameter x)         _ = True
            filterKey (StaticVariable ty x) _ = True
            filterKey other                 _ = False

    paramsToVars ps st = finalState
        where
            numberOfParameters = M.size $ M.filterWithKey isParameter (symbolTable st)
                where
                    isParameter (Parameter n) _ = True
                    isParameter _             _ = False
                    
            optionalParameters = L.drop numberOfParameters ps
            initialState = st { symbolTable = symbolTable' }
            
            finalState = L.foldr alterState initialState optionalParameters
                where
                    alterState (v, Just value) st = insertVar v value st
                    alterState (v, Nothing)    st = error "Missing required parameter"
            
            symbolTable' = M.mapKeys mapKey (symbolTable st)
                where
                    mapKey (Parameter n) = Variable $ case ps !! n of (v,value) -> v
                    mapKey other         = other
                    
    clearStack st = preformGCWithHeap st { symbolTable = M.filterWithKey (flip . const $ \x -> isStaticVariable x) $ symbolTable st } 

    clearHeap st = preformGCWithStack $ st { heap = bottom } 

    
data Job = ReferenceJob Reference
         | AddressJob HContext
         deriving (Eq,Ord,Show)
         
    
preformGCWithHeap st = preformGC (startSymbolTable ++ startHeap) st
    where
        startSymbolTable = L.map ReferenceJob . S.toList . M.fold join bottom $ symbolTable st
              
        startHeap = L.map AddressJob . M.keys . heap $ st      
                  
preformGCWithStack st = preformGC startSymbolTable st
    where
        startSymbolTable = L.map ReferenceJob . S.toList . M.fold join bottom $ (symbolTable st)
                    
preformGC start st = st { zValueTable = M.map getValue . M.filter isProcessed $ zValueTable', heap = M.map getValue . M.filter isProcessed $ heap' }
    where
        makeProcessed = (,)  Processed   . snd
        isProcessed   = (==) Processed   . fst 
        isUnprocessed = (==) Unprocessed . fst
        getValue      = snd

        initialHeap        = M.map ((,) Unprocessed) (heap st)
        initialZValueTable = M.map ((,) Unprocessed) (zValueTable st)

        (zValueTable', heap') = process start initialZValueTable initialHeap

        process :: [Job] -> (Reference :-> (State,Value)) -> (HContext :-> (State, Object)) -> (Reference :-> (State,Value), HContext :-> (State, Object))
        process []                        zValueTable heap = (zValueTable, heap)
        process ((ReferenceJob ref):tail) zValueTable heap = if isProcessed entry
            then process tail          zValueTable  heap
            else process (new ++ tail) zValueTable' heap
                where
                    entry = case M.lookup ref zValueTable of
                        Nothing    -> error "Dangling pointer to ZValue store"
                        Just entry -> entry
                        
                    zValueTable' = M.adjust makeProcessed ref zValueTable    
                        
                    new = L.map AddressJob . S.toList . coerce . getValue $ entry

        process ((AddressJob hctx):tail) zValueTable heap = if isProcessed entry
            then process tail          zValueTable heap
            else process (new ++ tail) zValueTable heap' 
                where
                    entry = case M.lookup hctx heap of 
                        Nothing    -> (Processed, undefined) -- error "Dangling pointer to heap"
                        Just entry -> entry
                    
                    heap' :: HContext :-> (State,Object)
                    heap' = M.adjust makeProcessed hctx heap
                    
                    new = L.map ReferenceJob . S.toList . M.fold join bottom . getValue $ entry
