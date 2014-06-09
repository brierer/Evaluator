
module Evaluator.FunctionApp.SemiDirectApp(
        applyToDValue,        
)where


import Evaluator.DValue
import Data.Dynamic
import Data.List.Split


applyToDValue ::  (Dynamic, String) -> [DValue] -> Either String DValue
applyToDValue fx args = if ((nbArgs+1)==(length args)) 
                        then (Left "Bad number of argument") 
                        else applyToDynDValue (fst fx) dynDValue        
                                                         where nbArgs = length (splitArguments (snd fx))
                                                               dynDValue = map toDyn args

applyToDynDValue :: Dynamic -> [Dynamic] -> Either String DValue
applyToDynDValue f args =  case fromDynamic $ (manyDynApp f args) of
                                                         Just x -> Right x

manyDynApp :: Dynamic -> [Dynamic] -> Dynamic
manyDynApp f (d:[]) = (dynApp f d)
manyDynApp f (d:ds) = manyDynApp (dynApp f d) ds

splitArguments :: String -> [String]
splitArguments s = init $ splitOn " -> " s

lastArguments :: String -> String
lastArguments s = last $ splitOn " -> " s 




