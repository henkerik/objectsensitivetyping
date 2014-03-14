module PHP.Interface where
    
import Prelude hiding (init)
import PHP.IR
import CCO.Printing as P hiding (render, join) 
import qualified Data.IntMap as IM
import qualified Data.List as L
import qualified Data.Set as S

class Optional a b r where
    opt :: (a -> b) -> a -> r

instance Optional a b b where
    opt = id
    
instance Optional a b (a -> b) where
    opt = const
    
execute :: (Optional (FunctionName -> Bool, ConstantName -> Bool, Type -> Bool, Type -> ClassConstantName -> Bool, [NativeClassDecl],  FunctionName -> S.Set Type, Program -> Graph) Syn_Program r) => Program -> r
execute p = opt (execute' p) (const False, const False, const False, \a -> \b -> False, [], const S.empty, error "Should not be called") 

execute' p (isNativeFunction, isNativeConstant, isNativeConstructor, isNativeClassConstant, nativeClasses, returnTypesByFunction, graphF) = wrap_Program (sem_Program p) inh
    where
        inh = Inh_Program 
              { 
                  labels_Inh_Program = 5, -- The first three labels are used for the default constructor and the fourth label is used as a drain for uncaught exceptions, the fifth is used a an allocator type label for a type sensitive analysis where objects are allocated in the main program stmt
                  isNativeFunction_Inh_Program      = isNativeFunction,
                  isNativeConstant_Inh_Program      = isNativeConstant,
                  isNativeConstructor_Inh_Program   = isNativeConstructor,
                  isNativeClassConstant_Inh_Program = isNativeClassConstant,
                  returnTypesByFunction_Inh_Program = returnTypesByFunction,
                  nativeClasses_Inh_Program         = nativeClasses,
                  graphF_Inh_Program                = graphF
              }

graph :: Program -> Graph
graph p = Graph { 
      init               = maybe (error "Init unknown") id . init_Syn_Program . execute $ p
    , final              = maybe (error "Final unknown") id . final_Syn_Program . execute $ p
    , flow               = IM.fromListWith S.union . L.map (\(a,b) -> (a, S.singleton b)) . flow_Syn_Program . execute $ p
    , binaryCallFlow     = binaryCallFlow_Syn_Program . execute $ p
    , binaryReturnFlow   = binaryReturnFlow_Syn_Program . execute $ p
    , nodes              = nodes_Syn_Program . execute $ p
    , returnSel          = return_Syn_Program . execute $ p
    , returnExceptionSel = returnException_Syn_Program . execute $ p
    , allocations        = allocations_Syn_Program . execute $ p
    , allocators         = allocators_Syn_Program . execute $ p
    , classes            = classes_Syn_Program . execute $ p
    , functions          = functions_Syn_Program . execute $ p
    , initialVars        = vars_Syn_Program . execute $ p
    , clauseStacks       = clauseStacks_Syn_Program . execute $ p
    , assignments        = assignments_Syn_Program . execute $ p
    , methodCallSites    = methodCallSites_Syn_Program . execute $ p
    , callNodes          = callNodes_Syn_Program . execute $ p
    , entryNodes         = entryNodes_Syn_Program . execute $ p
  }


isCallLabel g l  = S.member l (callNodes g)
isEntryLabel g l = S.member l (entryNodes g) 

instrument :: Program -> Program
instrument = instrumented_Syn_Program . execute


render :: Doc -> String
render = render_ 1000 

instance Printable Program where
    pp = pp_Syn_Program . execute
    
transform p isNativeFunction isNativeConstant isNativeConstructor isNativeClassConstant nativeClasses returnTypesByFunction = 
    let tmp = first_Syn_Program $ execute p (isNativeFunction, isNativeConstant, isNativeConstructor, isNativeClassConstant, nativeClasses, returnTypesByFunction, graph)
    in second_Syn_Program $ execute tmp (isNativeFunction, isNativeConstant, isNativeConstructor, isNativeClassConstant, nativeClasses, returnTypesByFunction, graph)
    

allocatorTypeByAllocationLabel :: Graph -> Label -> Maybe Label
allocatorTypeByAllocationLabel g l = IM.lookup l . allocators $ g