{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}


module Evaluator.DValue     
 where


import Data.Dynamic
import Data.Maybe
import Control.Applicative
import Data.List


data DValue = DArray [DValue] |  
	      DOrray DValue | 
	      DString String  |
	      DNum Double     |
	      DBool Bool      | 
	      DObj [(String, DValue)] | 
	      Dyn Dynamic     | 
	      DNot | 
	      DNums [Double] |
	      DStrings [String]  
	      deriving (Typeable)

data Tvalue = Arr | Num | Str | Boo | Obj 

data DState = DState {
      dvalue :: DValue
    , string :: String
    } deriving (Show)   

type EitherDValue = Either String DValue
type EitherDValues = Either String [DValue]



commentStack :: String -> DState -> DState
commentStack s d = DState (dvalue d) (s ++ string d)


instance Eq DValue where
	(==) (DNum d1) (DNum d2) = d1 == d2

	
instance Ord DValue where
	(>=) (DNum d1) (DNum d2) = d1 >= d2
	(>) (DNum d1) (DNum d2) = d1 > d2
	(<) (DNum d1) (DNum d2) = d2 >= d1
	(<=) (DNum d1) (DNum d2) = d2 > d1
	compare (DNum d1) (DNum d2) = compare  d1  d2
	compare x (DNot) = GT
	compare (DNot) (x) = LT
	compare _ _ = EQ

instance Show DValue where
	show (DNum d) = show d
	show (DString d) = show  d
	show (DArray []) = "[]" 
	show (DArray d) = "[" ++ foldr (++) [] ((show $ head $ d) : fmap (("," ++) . show) (tail d)) ++ "]"
	show (DOrray x) = show $ x
	show (Dyn d) = show d	
	show (DNot)  = show ""
	show (DObj ds)  = "{" ++ printTag ds ++ "}"
	show (DNums x)  =  show x
 	

printTag :: [(String, DValue)] -> String
printTag  [] = ""
printTag (d:[]) = (show $ fst d) ++ ":" ++ (show $ snd d) 
printTag (d:ds) = (show $ fst d) ++ ":" ++ (show $ snd d)   ++ "," ++ printTag ds



fromDValue :: DValue -> Dynamic 
fromDValue (DNum d ) = toDyn (d)
fromDValue (x) = toDyn $ (read $ (show x) :: [Double]) 

fromNum ::  DValue -> Double
fromNum (DNum d) = d 

fromString :: DValue -> String
fromString (DString x) = x

vNum :: DValue -> Either String Double
vNum (DNum d) = Right d
vNum (DArray ds) =	Left $ "Bad type, need a num , got a array"
vNum _  = Left $ "Bad type, need num, but got something else"

vNums :: DValue -> Either String [Double]
vNums (DArray ds) = mapM vNum ds 
vNums (DNums ds) = Right $ ds 

vStrings :: DValue -> Either String [String]
--vNums (DArray ds) = mapM vNum ds 
vStrings (DStrings ds) = Right $ ds
 
vString :: DValue -> Either String String
vString (DString d) = Right d
vString _ =	Left "Bad type.."


convertTable :: DValue ->DValue 
convertTable (DObj d) = DObj $ d
convertTable (DArray ds) = DArray $ map DArray $ transpose $ map convertArray ds
convertTable (DNums ds) =DArray $ map DNum ds
convertTable d = DArray [ DArray $ [d]]

convertArray :: DValue -> [DValue]
convertArray (DArray ds) = ds
convertArray (DNums ds) = map DNum ds
convertArray (d) = [d] 

