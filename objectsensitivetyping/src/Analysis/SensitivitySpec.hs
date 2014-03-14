module Analysis.SensitivitySpec where

import PHP.IR
import PHP.Interface
import Data.Maybe

type Context  = [Label]    
type HContext = [Label]

typeByAddress g []    = error "Unable to resolve empty heap context"
typeByAddress g (x:_) = typeByLabel g x
    
data SensitivitySpec = SensitivitySpec {
      lambda        :: Context
    , record        :: Label -> Context -> HContext
    , mergeMethod   :: Label -> HContext -> Context -> Context
    , mergeFunction :: Label -> Context -> Context
}

-- Builders

fullObjectSensitivityBuilder contextDepth heapDepth = SensitivitySpec {
    lambda        = [],
    record        = \l ctx      -> take heapDepth (l:ctx),
    mergeMethod   = \l hctx ctx -> take contextDepth hctx,
    mergeFunction = \l ctx      -> take contextDepth (l:ctx)
}

plainObjectSensitivityBuilder contextDepth heapDepth = SensitivitySpec {
    lambda        = [],
    record        = \l ctx      -> take heapDepth (l:ctx),
    mergeMethod   = \l hctx ctx -> take contextDepth (head hctx:ctx),
    mergeFunction = \l ctx      -> take contextDepth (l:ctx)
}

{-
typeSensitivityBuilder contextDepth heapDepth graph = SensitivitySpec {
    lambda        = [],
    record        = \l ctx      -> take heapDepth (l:ctx),
    mergeMethod   = \l hctx ctx -> take contextDepth $ (allocatorTypeByAllocationLabel graph $ head hctx):tail hctx,
    mergeFunction = \l ctx      -> take contextDepth (l:ctx)
}
-}

twoTypeOneH graph = SensitivitySpec {
    lambda        = [],
    record        = \l ctx      -> take 2 (l:ctx),
    mergeMethod   = \l hctx ctx -> (fromJust (allocatorTypeByAllocationLabel graph $ head hctx)):(tail hctx),
    mergeFunction = \l ctx      -> take 2 (l:ctx)
}

oneTypeOneObjOneH graph = SensitivitySpec {
    lambda        = [],
    record        = \l ctx      -> take 2 (l:ctx),
    mergeMethod   = \l hctx ctx -> case tail hctx of 
        []   -> [head hctx]
        [l'] -> [head hctx, fromMaybe l' (allocatorTypeByAllocationLabel graph l')],
    mergeFunction = \l ctx      -> take 2 (l:ctx) -- error $ "not yet supported merge for functions" --ctx
}

-- Variations

insensitive = const SensitivitySpec {
    lambda        = [],
    record        = \l ctx      -> [l],
    mergeMethod   = \l hctx ctx -> [],
    mergeFunction = \l ctx      -> []
}

oneObj            = const (plainObjectSensitivityBuilder 1 1)  -- full and plain coincide for contextDepth = 1
oneObjOneH        = const (plainObjectSensitivityBuilder 1 2)  -- full and plain coincide for contextDepth = 1
twoPlainOneH      = const (plainObjectSensitivityBuilder 2 2)
twoFullOneH       = const (fullObjectSensitivityBuilder 2 2)
--twoTypeOneH       = typeSensitivityBuilder 2 2


sensitivities = [ ("insensitive",  insensitive)
                , ("1obj",         oneObj)
                , ("1obj+1H",      oneObjOneH)
                , ("2plain+1H",    twoPlainOneH)
                , ("2type+1H",     twoTypeOneH)
                , ("1type1obj+1H", oneTypeOneObjOneH)
                , ("2full+1H",     twoFullOneH)
                ]
