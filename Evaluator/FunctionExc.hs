
module Evaluator.FunctionExc (
	applyToNative,
	applyToDValue,	
	convertArgsToDyn,
	applyToDyn,
	apply,
	toTest,
	convertMaybe,
	compareArguments,
	splitArguments,
)where


import Evaluator.DValue
import Data.List
import Data.Dynamic
import Data.Maybe
import Data.List.Split
import Control.Applicative


compareFunction :: [String] -> [DValue] -> Either String [DValue]
compareFunction s d = if ((length s) - (length d) == 0) then
					  	compareArguments s d  else
					  	Left "Wrong number of argument"	

compareArguments :: [String] -> [DValue] -> Either String [DValue]
compareArguments [] _ = Right []
compareArguments _ [] = Right []
compareArguments (s:sx) (d:dx) = (:) <$>  (compareDValueType d s) <*> (compareArguments sx dx)


splitArguments :: String -> [String]
splitArguments s = init $ splitOn " -> " s

lastArguments :: String -> String
lastArguments s = last $ splitOn " -> " s 

manyDynApp :: Dynamic -> [Dynamic] -> Dynamic
manyDynApp f (d:[]) =   (dynApp f d)
manyDynApp f (d:ds) = manyDynApp (dynApp f d) ds


apply ::  Dynamic -> [Dynamic] -> DValue
apply f a = case fromDynamic $ manyDynApp f a of
				Just x -> DNum x

applyArray ::  Dynamic -> [Dynamic] -> DValue
applyArray f a = case fromDynamic $ manyDynApp f a of
				Just x -> DArray $ map DNum x 


applyToDyn :: String -> Dynamic -> [Dynamic]  ->  DValue
applyToDyn s f a = case (s !! 0) of
					'D' ->  apply f a
					'[' ->  applyArray f a 
				    

applyToNative ::  (Dynamic, String) -> [DValue] -> Either String DValue
applyToNative fx args =   (applyToDyn (lastArguments (snd fx))  $  (fst fx)) <$> (convertArgsToDyn (snd fx) args)
					 		


convertArgsToDyn :: String ->  [DValue] -> Either String [Dynamic]
convertArgsToDyn f args = (convertMaybe <$>  
							(compareFunction (splitArguments $ (f))  (args)) ) 

convertMaybe :: [DValue] -> [Dynamic]
convertMaybe d = fmap fromDValue $ d   

toTest :: Dynamic -> Dynamic -> [Double]
toTest f d = fromJust $ fromDynamic $ dynApp f d


applyToDValue ::  (Dynamic, String) -> [DValue] -> Either String DValue
applyToDValue fx args = if ((nbArgs+1)==(length args)) then (Left "Bad number of argument") else
						 applyToDynDValue (fst fx) dynDValue	
						 	where nbArgs = length (splitArguments (snd fx))
						 	      dynDValue = map toDyn args

applyToDynDValue :: Dynamic -> [Dynamic] -> Either String DValue
applyToDynDValue f args =  case fromDynamic $ (manyDynApp f args) of
							 Just x -> Right x
