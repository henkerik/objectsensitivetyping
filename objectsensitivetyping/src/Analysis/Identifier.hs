module Analysis.Identifier where
    
import Control.DeepSeq        
import PHP.IR    
    
data Identifier = Variable Var
                | StaticVariable Type Var
                | Parameter Int
                | ReturnIdentifier
                | ExceptionIdentifier
                deriving (Ord,Eq)

instance NFData Identifier where
    rnf (Variable var)          = rnf var
    rnf (StaticVariable ty var) = rnf ty `seq` rnf var
    rnf (Parameter n)           = rnf n
    rnf ReturnIdentifier        = ()
    rnf ExceptionIdentifier     = ()

instance Show Identifier where
    show (Variable v)          = v
    show (StaticVariable ty v) = ty ++ "::" ++ v
    show (Parameter n)         = "#" ++ show n
    show ReturnIdentifier      = "*"
    show ExceptionIdentifier   = "#"
    
    
isReturn ReturnIdentifier      = True
isReturn other                 = False
-- 
-- isException ExceptionIdentifier = True
-- isException other               = False

isStaticVariable (StaticVariable ty v) = True
isStaticVariable other                 = False

isVariable (Variable v) = True
isVariable other        = False

isParameter (Parameter n) = True
isParameter other         = False

isThisIdentifier (Variable v) | v == "this" = True
                              | otherwise   = False
isThisIdentifier other                      = False