{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}


module Evaluator.DValue (
    DValue(..),
    fromDValue,
    typeOfDValue,
    compareDValueType,
    execFunc
    ) where

import Data.List
import Data.Dynamic
import Data.Maybe
import Data.Typeable


data DValue = DArray [DValue] | DString String | DNum Double | DBool Bool | DCom String | DFunction (String,[DValue]) | DObj [(String,DValue)] | Dyn Dynamic | DNot
    deriving (Typeable)

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
	show (DArray d) = "[" ++ foldr (++) [] ((show $ head d) : fmap (("," ++) . show) (tail d)) ++ "]"
	show (Dyn d) = show d	
	show (DNot)  = show ""
	show (DObj ds)  = "{" ++ printTag ds ++ "}"

 	

printTag :: [(String, DValue)] -> String
printTag  [] = ""
printTag (d:[]) = (fst d) ++ ":" ++ (show $ snd d) 
printTag (d:ds) = (fst d) ++ ":" ++ (show $ snd d)   ++ "," ++ printTag ds



fromDValue :: DValue -> Dynamic 
fromDValue (DNum d ) = toDyn (d)
fromDValue (x) = toDyn $ (read $ (show x) :: [Double]) 

-------------------------------
func :: Double -> Double -> Double
func x y = x+y  

f = toDyn (func)
d = DNum 3.0
c = DNum 4.0
r = manyAppFunc f [d,t]
t = DArray [d,c]
x = applyDyn f d
y = applyDyn f t


fromD :: DValue -> Dynamic
fromD (Dyn d) = d 

--------------------------------

execFunc :: (Dynamic , String) -> [DValue] -> Either String DValue
execFunc (f,s) d = Right $ manyAppFunc f d 

manyAppFunc :: Dynamic -> [DValue] -> DValue
manyAppFunc f (d:[]) = convertDyn  (applyDyn f d)
manyAppFunc f (d:ds) = manyAppFunc (fromD $ applyDyn f d) ds


applyDyn :: Dynamic -> DValue -> DValue
applyDyn f (DNum d) = Dyn $ dynApp f (toDyn d)
applyDyn f (Dyn d) = Dyn $ dynApp f d
applyDyn f a@(DArray d)  = if (((show $ dynTypeRep f) !! 0) == '[') 
				then	 Dyn $ dynApp (f) (fromDValue a)
				else	 DArray $ applyArray (f) (d)

applyArray :: Dynamic -> [DValue] -> [DValue]
applyArray f (d:[]) = [Dyn $ dynApp (f) ( fromDValue d)]
applyArray f (d:ds) = [Dyn $ dynApp (f) ( fromDValue d)] ++  applyArray f ds

convertDyn :: DValue -> DValue
convertDyn (Dyn d) =  case show $ dynTypeRep  d of
				x | x == "Double" -> DNum $ (fromJust $ fromDynamic d)
				x | x == "Int"	-> DNum $ (fromJust $ fromDynamic d)
convertDyn (DArray d) = DArray $ map convertDyn d

-----------------------------

fromNum ::  DValue -> Double
fromNum (DNum d) = d 



typeOfDValue :: DValue -> String
typeOfDValue (DNum d) = "Double"
typeOfDValue (DArray d)  = "[Double]" 


compareDValueType :: DValue -> String -> Either String DValue
compareDValueType  (DArray d) x =  if ((( "[" ++ typeOfDValue (d!!0)) ++ "]") == x) then (Right $ DArray d) 
					else ( Left "Bad argument pass to the function" ) ;
compareDValueType (d) x = if (( typeOfDValue (d)) == x) then (Right d) 
					else ( Left "Bad argument pass to the function" ) ;

