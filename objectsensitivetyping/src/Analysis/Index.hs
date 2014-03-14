module Analysis.Index where
    
import Control.DeepSeq    
import Data.Char
import CCO.Printing
import PHP.IR
    
type IndexVar = Int
    
data Index = FieldIndex FieldName
           | VarIndex IndexVar 
           | ArrayIndex String -- Old, replaced by a more general VarIndex
           | Default           -- Old
           deriving (Eq,Ord,Show)
           
instance Printable Index where
    pp Default           = lbracket >|< rbracket
    pp (FieldIndex name) = text name
    pp (ArrayIndex name) = text name
    pp (VarIndex n)      = text $ '@':[chr (n + 97)]
    
instance NFData Index where
    rnf (FieldIndex name) = rnf name
    rnf (VarIndex var)    = rnf var
    rnf (ArrayIndex name) = rnf name
    rnf (Default)         = ()    
    
keyIndex   = VarIndex 0
valueIndex = VarIndex 1    