{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}


module Evaluator.DValue     
 where


import Data.Dynamic
import Data.Maybe



data DValue = DArray [DValue] | DString String | DNum Double | DBool Bool | DCom String | DFunction (String,[DValue]) | DObj [(String,DValue)] | Dyn Dynamic | DNot  deriving (Typeable)

data Tvalue = Arr | Num | Str | Boo | Obj 

   

type EitherDValue = Either String DValue
type EitherDValues = Either String [DValue]

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
	show (Dyn d) = show d	
	show (DNot)  = show ""
	show (DObj ds)  = "{" ++ printTag ds ++ "}"

 	

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
vNum _ =	Left "Cmon"

vString :: DValue -> Either String String
vString (DString d) = Right d
vString _ =	Left "Cmon"


-------------------------------

fromD :: DValue -> Dynamic
fromD (Dyn d) = d 

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



