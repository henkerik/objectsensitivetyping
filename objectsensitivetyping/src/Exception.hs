module Exception where
    
import Control.Monad.Trans.Error    
    
data Exception = ParseException String 
               | UnknownException
               | MessageException String
               deriving (Show)
               
instance Error Exception where
    noMsg      = UnknownException
    strMsg msg = MessageException msg