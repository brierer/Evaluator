
module Evaluator.FunctionExc (
	applyToNative,
	applyToDValeur,	
	convertArgsToDyn,
	applyToDyn,
	apply,
	toTest,
	convertMaybe,
	compareArguments,
	splitArguments,
)where


import Evaluator.DValeur
import Data.List
import Data.Dynamic
import Data.Maybe
import Data.List.Split
import Control.Applicative


compareFunction :: [String] -> [DValeur] -> Either String [DValeur]
compareFunction s d = if ((length s) - (length d) == 0) then
					  	compareArguments s d  else
					  	Left "Wrong number of argument"	

compareArguments :: [String] -> [DValeur] -> Either String [DValeur]
compareArguments [] _ = Right []
compareArguments _ [] = Right []
compareArguments (s:sx) (d:dx) = (:) <$>  (compareDValeurType d s) <*> (compareArguments sx dx)


splitArguments :: String -> [String]
splitArguments s = init $ splitOn " -> " s

lastArguments :: String -> String
lastArguments s = last $ splitOn " -> " s 

manyDynApp :: Dynamic -> [Dynamic] -> Dynamic
manyDynApp f (d:[]) =   (dynApp f d)
manyDynApp f (d:ds) = manyDynApp (dynApp f d) ds


apply ::  Dynamic -> [Dynamic] -> DValeur
apply f a = case fromDynamic $ manyDynApp f a of
				Just x -> DNum x

applyArray ::  Dynamic -> [Dynamic] -> DValeur
applyArray f a = case fromDynamic $ manyDynApp f a of
				Just x -> DArray $ map DNum x 


applyToDyn :: String -> Dynamic -> [Dynamic]  ->  DValeur
applyToDyn s f a = case (s !! 0) of
					'D' ->  apply f a
					'[' ->  applyArray f a 
				    

applyToNative ::  (Dynamic, String) -> [DValeur] -> Either String DValeur
applyToNative fx args =   (applyToDyn (lastArguments (snd fx))  $  (fst fx)) <$> (convertArgsToDyn (snd fx) args)
					 		


convertArgsToDyn :: String ->  [DValeur] -> Either String [Dynamic]
convertArgsToDyn f args = (convertMaybe <$>  
							(compareFunction (splitArguments $ (f))  (args)) ) 

convertMaybe :: [DValeur] -> [Dynamic]
convertMaybe d = fmap fromDValeur $ d   

toTest :: Dynamic -> Dynamic -> [Double]
toTest f d = fromJust $ fromDynamic $ dynApp f d


applyToDValeur ::  (Dynamic, String) -> [DValeur] -> Either String DValeur
applyToDValeur fx args = if ((nbArgs+1)==(length args)) then (Left "Bad number of argument") else
						 applyToDynDValeur (fst fx) dynDValeur	
						 	where nbArgs = length (splitArguments (snd fx))
						 	      dynDValeur = map toDyn args

applyToDynDValeur :: Dynamic -> [Dynamic] -> Either String DValeur
applyToDynDValeur f args =  case fromDynamic $ (manyDynApp f args) of
							 Just x -> Right x
