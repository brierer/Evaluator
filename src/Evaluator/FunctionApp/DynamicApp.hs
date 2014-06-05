
module Evaluator.FunctionApp.DynamicApp(
                execFunc
)where


import Evaluator.DValue
import Data.Dynamic
import Data.Maybe


execFunc :: (Dynamic , String) -> [DValue] -> Either String DValue
execFunc (f,s) d = Right $ manyAppFunc f d 

manyAppFunc :: Dynamic -> [DValue] -> DValue
manyAppFunc f (d:[]) = convertDyn  (applyDyn f d)
manyAppFunc f (d:ds) = manyAppFunc (fromD $ applyDyn f d) ds


applyDyn :: Dynamic -> DValue -> DValue
applyDyn f (DNum d) = Dyn $ dynApp f (toDyn d)
applyDyn f (Dyn d) = Dyn $ dynApp f d
applyDyn f a@(DArray d)  = if (((show $ dynTypeRep f) !! 0) == '[') 
                                then         Dyn $ dynApp (f) (fromDValue a)
                                else         DArray $ applyArray (f) (d)

applyArray :: Dynamic -> [DValue] -> [DValue]
applyArray f (d:[]) = [Dyn $ dynApp (f) ( fromDValue d)]
applyArray f (d:ds) = [Dyn $ dynApp (f) ( fromDValue d)] ++  applyArray f ds


fromD :: DValue -> Dynamic
fromD (Dyn d) = d 

convertDyn :: DValue -> DValue
convertDyn (Dyn d) =  case show $ dynTypeRep  d of
                                x | x == "Double" -> DNum $ (fromJust $ fromDynamic d)
                                x | x == "Int"        -> DNum $ (fromJust $ fromDynamic d)
convertDyn (DArray d) = DArray $ map convertDyn d

