{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Analysis.Typing where

import Control.DeepSeq
import CCO.Printing as P hiding (join)
import Control.Monad.Reader hiding (lift,join)
import Control.Monad.State hiding (lift,join)
import Control.Monad.Error hiding (lift,join)
import Control.Monad.Identity hiding (join)
import Control.Applicative
import Data.List as L hiding (init, and, or)
import Data.Map as M hiding (update)
import Data.IntMap as IM hiding (update)
import Data.Set as S
import Data.Maybe 
import Prelude hiding (init, fromInteger, min, div, mod, not, and, or)
import Framework.Lattice
import Framework.Solve as S

import PHP.IR as IR hiding (allocations)


import qualified Native.NativeAPI as API
import           Native.ToValue
import           Native.Inference

import Analysis.Coerce
import Analysis.Inject
import Analysis.Identifier
import Analysis.Index
import Analysis.SensitivitySpec

import Analysis.Lattice.Sign
import Analysis.Lattice.Object

import Analysis.Lattice.AbstractValue
import Analysis.Lattice.AbstractAddress
import Analysis.Lattice.AbstractInteger
import Analysis.Lattice.AbstractDouble
import Analysis.Lattice.AbstractBool
import Analysis.Lattice.AbstractString
import Analysis.Lattice.AbstractNull as Null
import Analysis.Lattice.AbstractUnset as Unset
-- import Analysis.Lattice.AbstractUndefined as Undefined
import Analysis.Lattice.AbstractNumeric
import Analysis.Lattice.Reachable as Reachable
import qualified Analysis.Filterable as FT
--import Analysis.Allocation
-- import Analysis.Lattice.CallSet

import Analysis.ProgramState as PS
import Analysis.ProgramState.WithReachable
import AppConfig
import Debug.Trace
import App


data DynamicWorkMap = DWM (M.Map (Label,Context) (Set (Work (Label,Context)))) deriving (Eq,Ord)
data DynamicFlowMap = DFM (M.Map Label (Set Label)) deriving (Eq,Ord)

instance Lattice DynamicWorkMap where
    bottom                      = DWM bottom
    join (DWM left) (DWM right) = DWM $ join left right
    (<:) (DWM left) (DWM right) = left <: right

instance Lattice DynamicFlowMap where
    bottom                      = DFM bottom
    join (DFM left) (DFM right) = DFM $ join left right
    (<:) (DFM left) (DFM right) = left <: right

instance NFData DynamicWorkMap where rnf (DWM m) = rnf m
instance NFData DynamicFlowMap where rnf (DFM m) = rnf m

instance Lattice (Set Label) where
    bottom = S.empty
    join   = S.union
    (<:)   = S.isSubsetOf
    
instance Lattice (Set (Work (Label,Context))) where
    bottom = S.empty
    join   = S.union
    (<:)   = S.isSubsetOf
    
instance Printable (Set Label) where
    pp set = sepBy (L.map showable . S.toList $ set) (comma >|< space)
    
instance Printable (Set (Work (Label,Context))) where
    pp set = sepBy (L.map showable . S.toList $ set) (comma >|< space)
    
instance Printable DynamicWorkMap where
    pp (DWM map) = above (M.elems . M.mapWithKey (\loc workset -> showable loc >|< text " :: " >|< pp workset) $ map) 

instance Printable DynamicFlowMap where
    pp (DFM map) = above (M.elems . M.mapWithKey (\l labelset -> showable l >|< text " :: " >|< pp labelset) $ map) 

instance Printable (DynamicWorkMap,DynamicFlowMap) where
    pp (dwm,dfm) = text "DynamicWorkMap"
               >-< indent 4 (pp dwm) 
               >-< text "DynamicFlowMap"
               >-< indent 4 (pp dfm)

solve :: (Applicative m, Lattice local, ProgramState local, MonadError [Char] m, MonadReader AppConfig m, MonadState (Stats (Label,Context)) m, Printable local, NFData local) 
      => local 
      -> Graph 
      -> m ((DynamicWorkMap,DynamicFlowMap), (Label,Context) :-> WithReachable local)
solve phantom p = do lambda <- reader (lambda . flip sensitivitySpecBuilder p)
                     iota <- iota p
                     S.solve 
                        (\(l,ctx) -> case node p l of
                            (SN Skip)  -> True
                            other      -> False)
                        (next p) 
                        (globalTransferWithReachable p)
                        --(\(l,ctx)    -> globalTransfer p (node p l) l ctx) 
                        (localTransferWithReachable p)
                        --(\(l,ctx) st -> localTransfer p (node p l) l ctx st)
                        (binaryLocalTransferWithReachable p)
                        --(\(l,ctx) (l', ctx') -> binaryLocalTransfer p (node p l) (node p l') l ctx) 
                        bottom 
                        bottom 
                        iota 
                        $ next p (init p, lambda) bottom


--globalTransferWithReachable :: loc -> local -> global -> m global
globalTransferWithReachable g (l,ctx) wr@(WR Reachable.Bottom ps) global = return global
globalTransferWithReachable g (l,ctx) wr@(WR Reachable.Top ps)    global = globalTransfer g (node g l) l ctx ps global
                                                                              
--localTransferWithReachable :: Graph -> loc -> local -> m local
localTransferWithReachable g (l,ctx) wr@(WR Reachable.Bottom ps) = return bottom
localTransferWithReachable g (l,ctx) wr@(WR Reachable.Top ps)    = do ps' <- localTransfer g (node g l) l ctx ps  
                                                                      return $ WR Reachable.Top ps'


--binaryLocalTransferWithReachable :: Graph -> loc -> loc -> local -> local -> m local
binaryLocalTransferWithReachable g (l,ctx) (l',ctx') wr@(WR Reachable.Top ps) wr'@(WR Reachable.Top ps') = 
  do
    result <- binaryLocalTransfer g (node g l) (node g l') l ctx ps ps'
    return $ WR Reachable.Top result
binaryLocalTransferWithReachable g (l,ctx) (l',ctx') wr wr' = return bottom


iota :: (Lattice local, ProgramState local, MonadReader AppConfig m)
     => Graph -> m ((Label,Context) :-> WithReachable local)
iota p = do lambda <- reader (lambda . flip sensitivitySpecBuilder p)
            let fields = getStaticFields p 
            let fs  = L.map (\v -> insertVar v $ inject Null.Top) . S.toList . initialVars $ p
            fs' <- mapM (\(FieldDeclaration visibility isStatic ty v value) -> generalizedAddConst p lambda (writeStaticField ty v) (maybe Null id value)) fields
            let ps = L.foldr (.) id (fs ++ fs') bottom
            return $ M.singleton (init p, lambda) $ WR Reachable.Top ps

   
-- TODO Abstract commen behaviour
binaryLocalTransfer :: (ProgramState local, Lattice local, MonadReader AppConfig m)
                    => Graph
                    -> Node
                    -> Node
                    -> Label
                    -> Context
                    -> local
                    -> local
                    -> m local
-- Normal flow                    
binaryLocalTransfer p (SN (FunctionCall v ref n ps)) (SN CallReturn) l ctx st st' = 
    let value = lookupIdent ReturnIdentifier st'
    in return $ maybe id (flip insertVar value) v (st `join` clearStack st')
binaryLocalTransfer p (SN (ConstructorCall v ty ps)) (SN CallReturn) l ctx st st' = 
    let value = lookupIdent ReturnIdentifier st'
    in return $ insertVar v value (st `join` clearStack st')
binaryLocalTransfer p (SN (MethodCall v ref v' n ps)) (SN CallReturn) l ctx st st' = 
    let value = lookupIdent ReturnIdentifier st'
    in return $ maybe id (flip insertVar value) v (st `join` clearStack st')
binaryLocalTransfer p (SN (StaticCall v ref ty n ps)) (SN CallReturn) l ctx st st' = 
    let value = lookupIdent ReturnIdentifier st'
    in return $ maybe id (flip insertVar value) v (st `join` clearStack st')
binaryLocalTransfer p (SN (ResolvedCall v ref ty n ps)) (SN CallReturn) l ctx st st' = 
    let value = lookupIdent ReturnIdentifier st'
    in return $ maybe id (flip insertVar value) v (st `join` clearStack st')
        
-- Exception flow        
binaryLocalTransfer p (SN (FunctionCall v ref n ps)) (SN ExceptionReturn) l ctx st st' = 
    let value = lookupIdent ExceptionIdentifier st'
    in return $ insertIdent ExceptionIdentifier value (st `join` clearStack st')
binaryLocalTransfer p (SN (ConstructorCall v ty ps)) (SN ExceptionReturn) l ctx st st' = 
    let value = lookupIdent ExceptionIdentifier st'
    in return $ insertIdent ExceptionIdentifier value (st `join` clearStack st')
binaryLocalTransfer p (SN (MethodCall v ref v' n ps)) (SN ExceptionReturn) l ctx st st' = 
    let value = lookupIdent ExceptionIdentifier st'
    in return $ insertIdent ExceptionIdentifier value (st `join` clearStack st')
binaryLocalTransfer p (SN (StaticCall v ref ty n ps)) (SN ExceptionReturn) l ctx st st' = 
    let value = lookupIdent ExceptionIdentifier st'
    in return $ insertIdent ExceptionIdentifier value (st `join` clearStack st')
binaryLocalTransfer p (SN (ResolvedCall v ref ty n ps)) (SN ExceptionReturn) l ctx st st' = 
    let value = lookupIdent ExceptionIdentifier st'
    in return $ insertIdent ExceptionIdentifier value (st `join` clearStack st')                
binaryLocalTransfer p first second l ctx st st' = error $ "Unknown binary transfer function with first node: " ++ show first ++ " and second node: " ++ show second
    
    

next p (l,ctx) (DWM dwm, DFM dfm) = S.toList (dynamicInterprocedural `S.union` dynamicIntraprocedural `S.union` static `S.union` binaryCall `S.union` binaryReturn)
    where
        dynamicInterprocedural = M.findWithDefault S.empty (l,ctx) dwm                                                          -- Context may differ
        dynamicIntraprocedural = S.map (\l' -> Unary (l,ctx) (l',ctx)) . M.findWithDefault  S.empty l $ dfm                      -- Same context
        static                 = S.map (\l' -> Unary (l,ctx) (l',ctx)) . IM.findWithDefault S.empty l . flow $ p                -- Same context
        binaryCall             = maybe S.empty (S.map (\(lr,la) -> Binary (l,ctx)  (lr,ctx) (la,ctx))) . IM.lookup l . binaryCallFlow $ p
        binaryReturn           = maybe S.empty (S.map (\(lc,la) -> Binary (lc,ctx) (l,ctx)  (la,ctx))) . IM.lookup l . binaryReturnFlow $ p    -- Same context
        
    


localTransfer :: (Printable local, ProgramState local, Lattice local, MonadReader AppConfig m, MonadError [Char] m)
              => Graph
              -> Node
              -> Label
              -> Context
              -> local
              -> m local
localTransfer p (SN (InstanceOf v v' ty))                 l ctx st = return $ insertVar v (inject anyBool) st
localTransfer p (SN (Clone v t))                          l ctx st = error "Not yet implemented: clone"
localTransfer p (SN Skip)                                 l ctx st = return st
localTransfer p (SN CallReturn)                           l ctx st = return st
localTransfer p (SN CallAfter)                            l ctx st = return st
localTransfer p (SN ExceptionAfter)                       l ctx st = return st
localTransfer p (SN (Cast v ty v'))                       l ctx st = do record <- reader (record . flip sensitivitySpecBuilder p)
                                                                        let value = lookupVar v' st
                                                                        let value' = case ty of
                                                                                         CastArray  -> let addresses = coerce value::AbstractAddress
                                                                                                       in inject $ join addresses (fromHContext (record l ctx))
                                                                                                       
--                                                                                                       if S.null addresses 
  --                                                                                                        then inject (fromHContext (record l ctx))
    --                                                                                                      else inject addresses
                                                                                         CastBool   -> inject (coerce value::AbstractBool)
                                                                                         CastInt    -> inject (coerce value::AbstractInteger)
                                                                                         CastObject -> inject (coerce value::AbstractAddress)
                                                                                         CastReal   -> inject (coerce value::AbstractDouble)
                                                                                         CastString -> inject (coerce value::AbstractString)
                                                                                         CastUnset  -> value
                                                                        return $ insertVar v value' st
                                                                            
localTransfer p (SN (Assign v False v'))                  l ctx st = return $ insertVar v (lookupVar v' st) st
localTransfer p (SN (Define v v'))                        l ctx st = return $ insertConstant v (lookupVar v' st) st
localTransfer p (SN (Lookup v name))                      l ctx st = return $ insertVar v (lookupConstant name st) st 
localTransfer p (SN (Declare v const))                    l ctx st = addConst p ctx (Variable v) const st
    
--    value <- constToValue p const
--  -                                                                      return $ insertVar v value st
localTransfer p (SN (PreIncr v))                          l ctx st = do let value = preIncr (lookupVar v st)
                                                                        return $ insertVar v value st
localTransfer p (SN (PreDecr v))                          l ctx st = do let value = preDecr (lookupVar v st) 
                                                                        return $ insertVar v value st
localTransfer p (SN (BinaryOp op v v' v''))               l ctx st = do let value = binary op (lookupVar v' st) (lookupVar v'' st)
                                                                        return $ insertVar v value st
localTransfer p (SN (UnaryOp op v v'))                    l ctx st = do let value = unary op (lookupVar v' st)
                                                                        return $ insertVar v value st
localTransfer p (SN (Unset v))                            l ctx st = return $ insertVar v (inject Unset.Top `join` inject Null.Top) st
localTransfer p (SN (Return v))                           l ctx st = return $ insertIdent ReturnIdentifier (lookupVar v st) st
localTransfer p (SN (Throw v))                            l ctx st = return $ insertIdent ExceptionIdentifier (lookupVar v st) st
localTransfer p (SN (FunctionCall v ref n ps))            l ctx st = return $ insertIdent ReturnIdentifier (inject Null.Top) . varsToParams ps $ st
localTransfer p (SN (ConstructorCall v ty ps))            l ctx st = do  
    record <- reader (record . flip sensitivitySpecBuilder p)
    let fields = getFields p ty -- trace ("getField ty: " ++ show ty) $ getFields p ty -- Field of ty and all inherited fields
    let hctx   = record l ctx
    
    let st'    = insertIdent ReturnIdentifier (inject hctx) 
               . insertVar "this" (inject hctx) 
               . varsToParams ps 
               $ st
               
    let updateState (FieldDeclaration _ _ _ name value)  = generalizedAddConst p ctx (writeField' hctx $ FieldIndex name) (maybe Null id value)
        updateState (NativeFieldDeclaration _ _ name ty) = let (IV _ _ _ normal key value) = toValue hctx ty
                                                           in return $ writeField' hctx (FieldIndex name) normal
    
    fs <- mapM updateState fields   
    return $ L.foldr (.) id fs st'
    
localTransfer p (SN (MethodCall v ref v' n ps))           l ctx st = return $ insertIdent ReturnIdentifier (inject Null.Top) . insertVar "this" (lookupVar v' st) . varsToParams ps $ st
localTransfer p (SN (StaticCall v ref ty n ps))           l cts st = return $ insertIdent ReturnIdentifier (inject Null.Top) . varsToParams ps $ st
localTransfer p (SN (ResolvedCall v ref ty n ps))         l ctx st = return $ insertIdent ReturnIdentifier (inject Null.Top) . insertVar "this" (lookupVar "this" st) . varsToParams ps $ st
localTransfer p (SN stmt@(NativeFunctionCall v r n ps types))  l ctx st = do 
    functionByName <- reader (API.functionByName . nativeAPI)
    let nativeType = maybe (error $ "Unknown native function: " ++ n ++ ", label: " ++ show l) id . functionByName $ n
    preformTypeInferencing p n (fmap Variable v) (L.map Variable ps) nativeType l ctx st
    

localTransfer p (SN (Entry callableUnit@(Method ty n)))   l ctx st = do isHintingEnabled <- reader isHintingEnabled
                                                                        let ps = convertConstants p isHintingEnabled ctx (params p callableUnit)
                                                                        st' <- paramsToVars ps st
                                                                        -- Introduce some path sensitivity
                                                                        let isCaller = S.member ty . getSubtypes p . typeByAddress p
                                                                        
                                                                        --let isCaller address = case resolve p $ Method (typeByAddress p address) n of
                                                                        --                           Nothing       -> trace ("Nothing with hctx: " ++ show address) False
                                                                        --                           Just resolved -> trace ("Just with: " ++ show resolved ++ ", " ++ show callableUnit) $ callableUnit == resolved
                                                                        let value = inject . S.filter isCaller . coerce $ lookupVar "this" st
                                                                        enableGC <- reader enableGC
                                                                        return $ preformGC enableGC (insertVar "this" value $ S.fold (\v -> insertVar v (inject Null.Top)) st' (S.difference (vars p callableUnit) (S.fromList $ varsInParams ps)))
                                                                        
localTransfer p (SN (Entry callableUnit))                 l ctx st = do isHintingEnabled <- reader isHintingEnabled
                                                                        let ps = convertConstants p isHintingEnabled ctx (params p callableUnit)
                                                                        st' <- paramsToVars ps st
                                                                        enableGC <- reader enableGC
                                                                        return $ preformGC enableGC (S.fold (\v -> insertVar v (inject Null.Top)) st' (S.difference (vars p callableUnit) (S.fromList $ "this":varsInParams ps)))

--                                                                        let a = trace "oke" st
--                                                                        st' <- paramsToVars ps' a
--                                                                        return $ preformGC (S.fold (\v -> insertVar v (inject Null.Top)) st' (S.difference (vars p callableUnit) (S.fromList $ varsInParams ps')))

-- TODO Exstract similarites between exit transfer functions -- TODO Clean up
localTransfer p (SN (Exit callableUnit@(Constructor ty))) l ctx st = let value = lookupVar "this" st
                                                                     in return (removeVar "this" . insertIdent ReturnIdentifier value $ st)
localTransfer p (SN (Exit DefaultConstructor))            l ctx st = let value = lookupVar "this" st
                                                                     in return (removeVar "this" . insertIdent ReturnIdentifier value $ st)
localTransfer p (SN (Exit callableUnit))                  l ctx st = return st
localTransfer p (SN (ExitException callableUnit))         l ctx st = return st
localTransfer p (SN ExceptionReturn)                      l ctx st = return st
localTransfer p (SN (WriteField v f False v'))            l ctx st = do record <- reader (record . flip sensitivitySpecBuilder p)
                                                                        return $ writeField (Variable v) (FieldIndex f) (lookupVar v' st) (record l ctx) st


localTransfer p (SN (ReadField v False v' f))             l ctx st = do record <- reader (record . flip sensitivitySpecBuilder p)
                                                                        let (st', value) = readField (Variable v') (FieldIndex f) (record l ctx) st
                                                                        return $ insertVar v value st'



localTransfer p (SN (WriteArray a idx False v))           l ctx st = do record <- reader (record . flip sensitivitySpecBuilder p)
                                                                        return $ writeField (Variable a) keyIndex   (maybe (inject anyInteger) (flip lookupVar st) idx) (record l ctx)
                                                                               . writeField (Variable a) valueIndex (lookupVar v st) (record l ctx)
                                                                               $ st

                                      
localTransfer p (SN (ReadArray v False a idx))            l ctx st = do record <- reader (record . flip sensitivitySpecBuilder p)
                                                                        let (st', value) = readField (Variable a) valueIndex (record l ctx) st
                                                                        return $ insertVar v value st'
                                                                                                                                                                                                                              

    
                                                                                                                                                              
                                                                        
localTransfer p (SN (WriteStaticField ty v ref  v'))      l ctx st = return $ writeStaticField ty v (lookupVar v' st) st
localTransfer p (SN (ReadStaticField v ref ty v'))        l ctx st = return $ insertVar v (readStaticField ty v' st) st
localTransfer p (EN (Clause ty v s))                      l ctx st = return $ insertVar v (lookupIdent ExceptionIdentifier st) st
localTransfer p (CN (NativeConstructorDecl))              l ctx st = return . removeVar "this" . insertIdent ReturnIdentifier (lookupVar "this" st) $ st -- Needs params to resolve polymorphism
localTransfer p (MN (NativeMethodDecl sttc name ty))      l ctx st = do res <- preformTypeInferencing p name (Just ReturnIdentifier) (getParameters st) ty l ctx st
                                                                        return res -- $ trace ("DONE" ++ name) res

    
    
--    do record <- reader (record . sensitivitySpec)
--                                                                        let hctx = record l ctx
--                                                                        let (IV _ _ _ normal key value) = toValue hctx (API.returnType typeset)
--                                                                        return $ trace ("Native method: " ++ name) (insertIdent ReturnIdentifier normal st) -- Needs receiver object and params, is in this Identifier
localTransfer p (SN (Foreach iter key ref value s))       l ctx st = do record <- reader (record . flip sensitivitySpecBuilder p)
                                                                        let (st', keyValue)    = readField (Variable iter) keyIndex (record l ctx) st
                                                                        let (st'', valueValue) = readField (Variable iter) valueIndex (record l ctx) st'
                                                                        let st''' = maybe st'' (\k -> insertVar k keyValue st'') key
                                                                        return $ insertVar value valueValue st'''
                                                                        
localTransfer p (SN (Assign v True v'))                   l ctx st = throwError "Reference assignment not supported" 
localTransfer p (SN (ReadArray v True a idx))             l ctx st = throwError "Reference read from an array not supported" 
localTransfer p (SN (WriteArray a idx True v))            l ctx st = throwError "Reference assignment to an array not supported"
localTransfer p (SN (ReadField v True v' f))              l ctx st = throwError "Reference read from a field not supported"
localTransfer p (SN (WriteField v f True v'))             l ctx st = throwError "Reference assignment to a field not supported"
localTransfer p unknown                                   l ctx st = throwError $ "Unknow node: " ++ show unknown
 
varsInParams :: (ProgramState st, MonadReader AppConfig m) => [(Var, st -> m st)] -> [Var]
varsInParams = L.map fst


-- preformTypeInferencing :: Maybe Identifier -> [Identifier] -> NativeType NativeTypeSet -> Label -> Context -> st -> st
preformTypeInferencing p name v ps nativeType l ctx st = do 
    record <- reader (record . flip sensitivitySpecBuilder p)
    let hctx = record l ctx
    let inferencedTypes = inference v ps hctx nativeType . L.map (lookupTriple (record l ctx) st) $ ps

    -- trace ("START preformTypeInferencing :: " ++ show name ++ show nativeType ++ ", " ++ (show $ L.map (lookupTriple (record l ctx) st) ps)) 
        
    -- Write back return value and output parameters    
    let process (ident,(normal,key,value)) = writeField ident keyIndex   key   (record l ctx) 
                                           . writeField ident valueIndex value (record l ctx) 
                                           . insertIdent ident normal'
                                           where
                                               normal' = if value /= bottom
                                                         then normal `join` (inject (fromHContext hctx))
                                                         else normal
                                                 
    return $ L.foldr process st inferencedTypes



insertWork = L.foldr (\(loc,work) -> M.insertWith S.union loc $ S.fromList work)

createWork p l ctx Nothing ctx'         = []
createWork p l ctx (Just resolved) ctx' = 
    [-- Entry
     ((l,ctx),                          [Unary (l,ctx)                         (entry p resolved,ctx')         -- Entry: l_c to l_n 
                                        ,Unary (exit p resolved,ctx')          (IR.return' p l,ctx)            -- Immediate return: l_x to l_r
                                        ,Unary (exitException p resolved,ctx') (IR.returnException p l,ctx)])  -- Immediate exceptio return l_xe to l_re
     -- Exit
    ,((exit p resolved,ctx'),           [Unary (exit p resolved,ctx')          (IR.return' p l,ctx)])          -- Exit:  l_x to l_r
     -- Exception Exit
    ,((exitException p resolved,ctx'),  [Unary (exitException p resolved,ctx') (IR.returnException p l,ctx)])  -- Exception Exit: l_xe to l_re
    ] 




globalTransfer :: (ProgramState local, MonadReader AppConfig m)
               => Graph
               -> Node
               -> Label
               -> Context
               -> local
               -> (DynamicWorkMap,DynamicFlowMap)    -- (Map (Label, Context) (Set (Work (Label, Context))), Map Label (Set Label))
               -> m (DynamicWorkMap,DynamicFlowMap)  -- (Map (Label, Context) (Set (Work (Label, Context))), Map Label (Set Label))
globalTransfer p (SN (MethodCall v ref v' n ps)) l ctx st (DWM dwm, DFM dfm) = do 
    merge  <- reader (mergeMethod . flip sensitivitySpecBuilder p)
    let dwm' = insertWork dwm
             . S.fold (++) []
             . S.map (\hctx -> createWork p l ctx (resolve p $ Method (typeByAddress p hctx) n) (merge l hctx ctx))
             . coerce 
             . lookupVar v' 
             $ st
    return (DWM dwm', DFM dfm) 
globalTransfer p (SN (ConstructorCall v ty ps)) l ctx st (DWM dwm, DFM dfm) = do
    merge  <- reader (mergeMethod . flip sensitivitySpecBuilder p)
    record <- reader (record . flip sensitivitySpecBuilder p)
    return (DWM $ insertWork dwm (createWork p l ctx (resolve p $ Constructor ty) (merge l (record l ctx) ctx)), DFM dfm)
globalTransfer p (SN (FunctionCall v ref n ps)) l ctx st (DWM dwm, DFM dfm) = do
    merge  <- reader (mergeFunction . flip sensitivitySpecBuilder p)
    return (DWM $ insertWork dwm $ createWork p l ctx (Just $ Function n) (merge l ctx), DFM dfm)
globalTransfer p (SN (StaticCall v ref ty n ps)) l ctx st (DWM dwm, DFM dfm) = do
    merge  <- reader (mergeFunction . flip sensitivitySpecBuilder p)
    return (DWM $ insertWork dwm $ createWork p l ctx (Just $ StaticMethod ty n) (merge l ctx), DFM dfm)
globalTransfer p (SN (ResolvedCall v ref ty n ps)) l ctx st (DWM dwm, DFM dfm) = do 
    merge  <- reader (mergeMethod . flip sensitivitySpecBuilder p)
    let dwm' = insertWork dwm
             . S.fold (++) []
             . S.map (\hctx -> createWork p l ctx (Just $ Method ty n) (merge l hctx ctx))
             . coerce
             . lookupVar "this"
             $ st
    return (DWM dwm', DFM dfm) 
globalTransfer p (SN ExceptionAfter) l ctx st (DWM dwm, DFM dfm) = do
    let dfm' = S.fold (\l' -> M.insertWith S.union l (S.singleton l')) dfm
             . S.map (\hctx -> resolveCatch p l $ typeByAddress p hctx)
             . coerce 
             . lookupIdent ExceptionIdentifier 
             $ st
    return (DWM dwm, DFM dfm')
globalTransfer p (SN (Throw v)) l ctx st (DWM dwm, DFM dfm) = do
    let dfm' = S.fold (\l' -> M.insertWith S.union l (S.singleton l')) dfm
             . S.map (\hctx -> resolveCatch p l $ typeByAddress p hctx)
             . coerce 
             . lookupVar v 
             $ st
    return (DWM dwm, DFM dfm')
globalTransfer p other l ctx st global = return global

binary :: BinaryOp -> AbstractValue -> AbstractValue -> AbstractValue
binary Plus           left right = inject $ liftNumeric (lift plus) (coerce left) (coerce right)
binary Min            left right = inject $ liftNumeric (lift min)  (coerce left) (coerce right)
binary Mul            left right = inject $ liftNumeric (lift mul)  (coerce left) (coerce right)
binary Div            left right = inject anyInteger `join` inject anyDouble `join` inject anyBool --           (inject $ lift div (coerce left) (coerce right)) `join` (if S.member Zero (coerce right) then inject (S.singleton False) else bottom)
binary Mod            left right = inject anyInteger `join` inject anyBool --           (inject $ lift mod (coerce left) (coerce right)) `join` (if S.member Zero (coerce right) then inject (S.singleton False) else bottom)
binary Concat         left right = inject anyString
binary And            left right = inject $ lift and  (coerce left) (coerce right)
binary Or             left right = inject $ lift or   (coerce left) (coerce right)
binary Xor            left right = inject $ lift xor  (coerce left) (coerce right)
binary SL             left right = inject $ anyInteger
binary SR             left right = inject $ anyInteger
binary BAnd           left right = inject $ anyInteger
binary BOr            left right = inject $ anyInteger
binary BXor           left right = inject $ anyInteger
binary IsEqual        left right = inject $ anyBool
binary IsIdentical    left right = inject $ anyBool
binary IsNotEqual     left right = inject $ anyBool
binary IsNotIdentical left right = inject $ anyBool
binary Less           left right = inject $ anyBool
binary Greater        left right = inject $ anyBool
binary LessEqual      left right = inject $ anyBool
binary GreaterEqual   left right = inject $ anyBool  

unary :: UnaryOp -> AbstractValue -> AbstractValue
unary Not     value = inject $ unaryLift not (coerce value)
unary BNot    value = inject $ anyInteger

preIncr, preDecr :: AbstractValue -> AbstractValue
preIncr value = inject $ liftNumeric (lift plus) (coerce value) (fromInteger 1,bottom)
preDecr value = inject $ liftNumeric (lift min)  (coerce value) (fromInteger 1,bottom)


--lift :: (SignSet -> SignSet -> SignSet) -> AbstractNumeric -> AbstractNumeric -> AbstractNumeric
--lift f (AI left, AD left') (AI right, AD right') = (AI $ f left right, AD $ f left' right')  

--constToValue :: Graph -> Const -> m Value
constToValue p Null                          = return . inject $ Null.Top
constToValue p (Bool b)                      = return . inject $ b                                      -- . S.singleton $ b
constToValue p (Integer n)                   = return . inject $ n                                      -- . fromInteger $ n  ---S.singleton . fromNumeric $ n
constToValue p (Double n)                    = return . inject $ n                                      -- . fromInteger $ n -- TODO change to fromFloat -- S.singleton . fromInteger $ n
constToValue p (SingleQuotedString str)      = return . inject $ str                                    -- . fromString $ str
constToValue p (DoubleQuotedString str)      = return . inject $ str                                    -- . fromString $ str
constToValue p (NativeConstant name)         = do constantByName <- reader (API.constantByName . nativeAPI)
                                                  nativeTerminalTypeToValue (constantByName name)
constToValue p (ClassConstant ty name)       = do let cons = classConstantByName p ty name
                                                  constToValue p cons
constToValue p (NativeClassConstant ty name) = do getNativeClassConstant <- reader (API.getNativeClassConstant . nativeAPI)
                                                  nativeTerminalTypeToValue (getNativeClassConstant ty name)
constToValue p (Array l elems)               = error $ "use addConst if arrays are present"                                             
constToValue p other                         = error $ "Unknown const: " ++ show other


nativeTerminalTypeToValue Nothing   = error "Unknown constant" 
nativeTerminalTypeToValue (Just ty) = do let hctx = error "Native constants should not return objects"
                                         let (IV _ _ _ normal _ _) = toValue hctx ty
                                         return normal

addArrayElem p ctx hctx (ElemWithKey k v)  = do
    m  <- generalizedAddConst p ctx (writeField' hctx valueIndex) v
    m' <- generalizedAddConst p ctx (writeField' hctx keyIndex  ) k
    return $ m . m'
addArrayElem p ctx hctx (ElemWithoutKey v) = do
    generalizedAddConst p ctx (writeField' hctx valueIndex) v
    
generalizedAddConst p ctx f (Array l elems) = do
    record <- reader (record . flip sensitivitySpecBuilder p)
    let hctx  = record l ctx
    let value = (inject . fromHContext) hctx
    ms <- mapM (addArrayElem p ctx hctx) elems
    return $ L.foldl (.) (f value) ms
generalizedAddConst p ctx f other         = do
    value <- constToValue p other
    return $ f value
    
addConst p ctx ident const st = do f <- generalizedAddConst p ctx (insertIdent ident) const 
                                   return $ f st


-- convertConstants :: (MonadReader AppConfig m, ProgramState ps) => Graph -> HContext -> [Param] -> [(Var, st -> m st)]
convertConstants p isEnabled ctx = L.map updateParam
    where 
        -- TODO enable hinting for variables with defaults
        updateParam (Param hint        ref v (Just cons)) = (v, addConst p ctx (Variable v) cons)
        updateParam (Param Nothing     ref v Nothing)     = (v, return)
        updateParam (Param (Just hint) ref v Nothing)     = (v, return) --if isEnabled 
                                                            --then (v, filterVar p v hint)
                                                            --else (v, return)
        
        

filterVar :: (Monad m,ProgramState st) => Graph -> Var -> Type -> st -> m st
filterVar p v ty st = let value = lookupVar v st
                      in return $ insertVar v (FT.filter p ty value) st
                    
lookupTriple hctx st ident = (lookupIdent ident st, snd $ readField ident keyIndex hctx st, snd $ readField ident valueIndex hctx st)