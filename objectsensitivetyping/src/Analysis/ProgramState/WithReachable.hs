module Analysis.ProgramState.WithReachable where

import Analysis.Lattice.Reachable as Reachable
import Analysis.ProgramState
import Analysis.Identifier
import Framework.Lattice
import Control.DeepSeq        
import CCO.Printing as P hiding (join)


data WithReachable ps = WR Reachable ps deriving (Ord,Eq,Show)

instance Lattice ps => Lattice (WithReachable ps) where
    bottom = WR bottom bottom
    (WR reachable ps) <:     (WR reachable' ps') = (reachable <: reachable') && (ps <: ps')
    (WR reachable ps) `join` (WR reachable' ps') = WR (reachable `join` reachable') (ps `join` ps')

instance Printable ps => Printable (WithReachable ps) where 
    pp (WR Reachable.Bottom ps) = text "Unreachable"
    pp (WR Reachable.Top    ps) = text "Reachable: " >|< pp ps

instance NFData ps => NFData (WithReachable ps) where
    rnf (WR Reachable.Top ps)    = rnf ps
    rnf (WR Reachable.Bottom ps) = rnf ps


instance Functor WithReachable where
    fmap f (WR reachable a) = WR reachable (f a)

instance ProgramState ps => ProgramState (WithReachable ps) where
    varsToParams xs = fmap (varsToParams xs)
    
    paramsToVars = error "paramsToVars not implemented for WithReachable"

    clearStack = fmap clearStack
    clearHeap  = fmap clearHeap

    insertIdent ident value = fmap (insertIdent ident value)
    lookupIdent ident (WR reachable ps) = lookupIdent ident ps

    insertConstant var value = fmap (insertConstant var value)
    lookupConstant name (WR reachable ps) = lookupConstant name ps
    
    writeField ident idx value hctx = fmap (writeField ident idx value hctx)
    readField   = error "readField not implemented for WithReachable"

    -- write using only a hctx
    writeField' hctx idx value = fmap (writeField' hctx idx value)
    
    removeIdent ident = fmap (removeIdent ident)
    removeVar v = removeIdent (Variable v)
    
    preformGC enable = fmap (preformGC enable)
    getParameters (WR reachable ps) = getParameters ps
