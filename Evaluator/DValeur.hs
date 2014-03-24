{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}


module Evaluator.DValeur (
    DValeur(..),
    fromDValeur,
    typeOfDValeur,
    compareDValeurType,
    execFunc
    ) where

import Data.List
import Data.Dynamic
import Data.Maybe
import Data.Typeable


data DValeur = DArray [DValeur] | DString String | DNum Double | DBool Bool | DCom String | DFunction (String,[DValeur]) | Dyn Dynamic | DNot
    deriving (Typeable)

instance Eq DValeur where
	(==) (DNum d1) (DNum d2) = d1 == d2
	
instance Ord DValeur where
	(>=) (DNum d1) (DNum d2) = d1 >= d2
	(>) (DNum d1) (DNum d2) = d1 > d2
	compare (DNum d1) (DNum d2) = compare  d1  d2
	compare x (DNot) = GT
	compare (DNot) (x) = LT

instance Show DValeur where
	show (DNum d) = show d
	show (DString d) = show  d
	show (DArray d) = "[" ++ foldr (++) [] ((show $ head d) : fmap (("," ++) . show) (tail d)) ++ "]"
	show (Dyn d) = show d	
	show (DNot)  = show ""	

fromDValeur :: DValeur -> Dynamic 
fromDValeur (DNum d ) = toDyn (d)
fromDValeur (x) = toDyn $ (read $ (show x) :: [Double]) 

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


fromD :: DValeur -> Dynamic
fromD (Dyn d) = d 

--------------------------------

execFunc :: (Dynamic , String) -> [DValeur] -> Either String DValeur
execFunc (f,s) d = Right $ manyAppFunc f d 

manyAppFunc :: Dynamic -> [DValeur] -> DValeur
manyAppFunc f (d:[]) = convertDyn  (applyDyn f d)
manyAppFunc f (d:ds) = manyAppFunc (fromD $ applyDyn f d) ds


applyDyn :: Dynamic -> DValeur -> DValeur
applyDyn f (DNum d) = Dyn $ dynApp f (toDyn d)
applyDyn f (Dyn d) = Dyn $ dynApp f d
applyDyn f a@(DArray d)  = if (((show $ dynTypeRep f) !! 0) == '[') 
				then	 Dyn $ dynApp (f) (fromDValeur a)
				else	 DArray $ applyArray (f) (d)

applyArray :: Dynamic -> [DValeur] -> [DValeur]
applyArray f (d:[]) = [Dyn $ dynApp (f) ( fromDValeur d)]
applyArray f (d:ds) = [Dyn $ dynApp (f) ( fromDValeur d)] ++  applyArray f ds

convertDyn :: DValeur -> DValeur
convertDyn (Dyn d) =  case show $ dynTypeRep  d of
				x | x == "Double" -> DNum $ (fromJust $ fromDynamic d)
				x | x == "Int"	-> DNum $ (fromJust $ fromDynamic d)
convertDyn (DArray d) = DArray $ map convertDyn d

-----------------------------

fromNum ::  DValeur -> Double
fromNum (DNum d) = d 



typeOfDValeur :: DValeur -> String
typeOfDValeur (DNum d) = "Double"
typeOfDValeur (DArray d)  = "[Double]" 


compareDValeurType :: DValeur -> String -> Either String DValeur
compareDValeurType  (DArray d) x =  if ((( "[" ++ typeOfDValeur (d!!0)) ++ "]") == x) then (Right $ DArray d) 
					else ( Left "Bad argument pass to the function" ) ;
compareDValeurType (d) x = if (( typeOfDValeur (d)) == x) then (Right d) 
					else ( Left "Bad argument pass to the function" ) ;

