module Analysis.Type where

import Data.Set as S
import Data.List as L
import Data.Map as M
import CCO.Printing
import Analysis.Resource
import PHP.IR hiding (Type, Bool, Null, Double, Integer)

data Type = String
          | Integer
          | Double
          | Bool
          | Null
          | Undefined -- Indicates dead code
          | Resource Resource
          | Object String
          deriving (Eq,Ord)

isObject :: Type -> Bool
isObject (Object _) = True
isObject otherwise  = False          

instance Show Type where
    show String        = "string"
    show Integer       = "integer"
    show Bool          = "boolean"
    show Null          = "null"
    show Undefined     = "undefined"
    show Double        = "double"
    show (Resource rs) = "resource[" ++ show rs ++ "]"
    show (Object ty)   = "object[" ++ ty ++ "]"

instance Printable (Set Type) where
    pp = braces . flip sepBy (comma >|< space) . L.map showable . S.toList
    
class TypeSet a where
    typeSet :: Graph -> a -> Set Type
    
    