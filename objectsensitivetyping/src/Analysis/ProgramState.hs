module Analysis.ProgramState where

import Data.Set
import PHP.IR    
import Analysis.Lattice.AbstractValue
import Analysis.Identifier
import Analysis.Index
import Analysis.SensitivitySpec
import Framework.Lattice
import Control.Monad.Reader
import AppConfig
    
data Reference = Reference Identifier
               | IndexReference Index HContext 
               deriving (Show, Eq, Ord)
               
type ReferenceSet = Set Reference    

class Lattice st => ProgramState st where
    varsToParams :: [Var] -> st -> st
    paramsToVars :: MonadReader AppConfig m => [(Var, st -> m st)] -> st -> m st
                    
    clearStack   :: st -> st
    clearHeap    :: st -> st
                    
    insertIdent  :: Identifier -> AbstractValue -> st -> st
    lookupIdent  :: Identifier -> st -> AbstractValue
    
    insertConstant :: Var -> AbstractValue -> st -> st
    lookupConstant :: ConstantName -> st -> AbstractValue
    
    writeField   :: Identifier -> Index -> AbstractValue -> HContext -> st -> st
    readField    :: Identifier -> Index -> HContext -> st -> (st, AbstractValue)
    
    -- write using only a hctx
    writeField' :: HContext -> Index -> AbstractValue -> st -> st
    writeField' = error "not yet implemented writeField'"
    
    writeStaticField :: Type -> Var -> AbstractValue -> st -> st
    writeStaticField ty v = insertIdent (StaticVariable ty v)
    readStaticField  :: Type -> Var -> st -> AbstractValue
    readStaticField ty v = lookupIdent (StaticVariable ty v)
                 
    insertVar    :: Var -> AbstractValue -> st -> st
    insertVar v = insertIdent (Variable v)
    lookupVar    :: Var -> st -> AbstractValue
    lookupVar v = lookupIdent (Variable v)
    
    removeIdent :: Identifier -> st -> st
    removeIdent = error "not yet implemented removeVar"
    
    removeVar :: Var -> st -> st
    removeVar v = removeIdent (Variable v)
    
    -- Deprecated: compose readRef and writeRef
--    insertRef :: Var -> Var -> st -> st
--    insertRef = error "not yet implemented insertRef"
    
    readRef :: Var -> st -> ReferenceSet
    readRef = error "not yet implemented readRef"
    writeRef :: Var -> ReferenceSet -> st -> st
    writeRef = error "not yet implemented writeRef"

    readFieldRef :: Var -> Index -> HContext -> st -> (st, ReferenceSet)
    readFieldRef = error "not yet implemented readFieldRef"
    writeFieldRef :: Var -> Index -> ReferenceSet -> HContext -> st -> st
    writeFieldRef = error "not yet implemented writeFieldRef"
    
    preformGC :: Bool -> st -> st
    preformGC = const id
    
    getParameters :: st -> [Identifier]
    