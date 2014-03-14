module Analysis.Lattice.AbstractString where

import Data.Set as S
import Data.List as L
import Data.Char
import Control.DeepSeq        
import Framework.Lattice
import Analysis.Type
import qualified Analysis.Type as Ty
import CCO.Printing as Pr
    
data AbstractString = Bottom 
                    | Value String
                    | Top deriving (Show, Eq, Ord)

instance Lattice AbstractString where
    bottom = Bottom
    
    join Top          other         = Top
    join other        Top           = Top
    join (Value left) (Value right) = Top
    join Bottom       other         = other
    join other        Bottom        = other
    
instance Printable AbstractString where 
    pp Bottom      = Pr.empty
    pp (Value str) = text str 
    pp Top         = text "⊤"

instance NFData AbstractString where
    rnf Bottom      = ()
    rnf (Value str) = rnf str
    rnf Top         = ()
    
    
instance TypeSet AbstractString where
    typeSet g Bottom = S.empty
    typeSet g other  = S.singleton Ty.String    
    
anyString = Top

fromString = Value
    
toString Top         = Nothing
toString (Value str) = Just [str]
toString Bottom      = Just []

parseToInteger :: String -> Integer
parseToInteger ('-':str) = -1 * (parseToInteger str)
parseToInteger str       = fromIntegral . sum . zipWith (\x y -> 10 ^ x * y) [0..] . reverse . L.map digitToInt . takeWhile isDigit $ str


{-

module Analysis.Lattice.String where

import Control.DeepSeq
import Data.Char
import Data.Set as S
import Data.List as L
import CCO.Printing as P
import Framework.Lattice
import Prelude hiding (String)

data String = Top
            | 
            | Enum (Set [Char])
            deriving (Show, Eq, Ord)
            
            
anyString = Top

fromString = Enum . S.singleton
--fromString str | (and . map isDigit) str = Enum . sum . zipWith (\x y -> 10 ^ x * y) [1..] . reverse . map digitToInt $ str 
--               | otherwise               = NotUIntString str 



toString Top        = Nothing
toString (Enum set) = Nothing

toString1 Top        = Nothing
toString1 (Enum set) = Just $ S.toList set




instance NFData String where
    rnf Top        = ()
    rnf (Enum set) = rnf set

instance Printable String where
    pp Top        = text "⊤"
    pp (Enum set) = sepBy (L.map showable . S.toList $ set) (comma >|< space)
    
instance Lattice String where
    bottom = Enum S.empty
    
    join Top         other        = Top
    join other       Top          = Top
    join (Enum left) (Enum right) = Enum $ S.union left right
    
    
    (<:) (Enum left) (Enum right) = left `S.isSubsetOf` right
    (<:) (Enum left) Top          = True
    (<:) Top         (Enum right) = False
    (<:) Top         Top          = True
    
{-

data String = Top
            | Bottom
            | AnyUIntString
            | AnyNotUIntString
            | UIntString Int
            | NotUIntString [Char]
            deriving (Show,Eq,Ord)
            
        
anyString = Top        
        
fromString str | (and . map isDigit) str = UIntString . sum . zipWith (\x y -> 10 ^ x * y) [1..] . reverse . map digitToInt $ str 
               | otherwise               = NotUIntString str
               
toString Top               = Nothing 
toString Bottom            = Nothing
toString AnyUIntString     = Nothing
toString AnyNotUIntString  = Nothing
toString (UIntString n)    = Just $ show n
toString (NotUIntString v) = Just v        

--parseToInteger :: String -> Integer
parseToInteger ('-':str) = -1 * (parseToInteger str)
parseToInteger str       = sum . zipWith (\x y -> 10 ^ x * y) [1..] . reverse . map digitToInt . takeWhile isDigit $ str

instance Printable String where
    pp Top               = text "⊤"
    pp Bottom            = P.empty
    pp AnyUIntString     = text "[N]"
    pp AnyNotUIntString  = text "[S]"
    pp (UIntString n)    = showable n
    pp (NotUIntString s) = text "\"" >|< text s >|< text "\""

instance Lattice String where
    bottom = Bottom
    
    join Top                _                             = Top
    join _                  Top                           = Top
    
    join AnyUIntString      AnyNotUIntString              = Top
    join AnyNotUIntString   AnyUIntString                 = Top

    join (UIntString n)     AnyNotUIntString              = Top
    join (UIntString n)     (NotUIntString s)             = Top
    join AnyNotUIntString   (UIntString n)                = Top
    join (NotUIntString s)  (UIntString n)                = Top

    join (NotUIntString s)  AnyUIntString                 = Top
    join AnyUIntString      (NotUIntString s)             = Top


    join AnyUIntString      AnyUIntString                 = AnyUIntString
    join (UIntString n)     AnyUIntString                 = AnyUIntString
    join AnyUIntString      (UIntString n)                = AnyUIntString
    join (UIntString n)     (UIntString m)    | n == m    = UIntString n
                                              | otherwise = AnyUIntString
                                           
    join AnyNotUIntString   AnyNotUIntString              = AnyNotUIntString
    join (NotUIntString s)  AnyNotUIntString              = AnyNotUIntString
    join AnyNotUIntString   (NotUIntString s)             = AnyNotUIntString
    join (NotUIntString s)  (NotUIntString v) | s == v    = NotUIntString s
                                              | otherwise = AnyNotUIntString

    join Bottom             other                         = other
    join other              Bottom                        = other
    
-}

-}