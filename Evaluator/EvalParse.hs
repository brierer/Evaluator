module Evaluator.EvalParse
 where

import Evaluator.DValue
import Evaluator.FunctionApp.SemiDirectApp
import Evaluator.FunctionApp.DynamicApp
import Evaluator.EqParser
import Evaluator.FunctionApp.FunctionDV
import Data.Maybe
import Data.Dynamic
import Control.Applicative
import qualified Data.Map as M



evalParse :: String -> String
evalParse s = case  (run bloc s) of
				Right x ->
				 case snd (evalEqs $ (x)) of
					Right y -> show y
					Left  y  ->  y
				Left x  -> show x 			


------------------------------------


evalEqs :: [(String, DValue)] -> (String, EitherDValue) 
evalEqs (d) = evalEq (M.fromList d) (head d)

evalEq :: M.Map String DValue -> (String, DValue) -> (String, EitherDValue) 
evalEq  m (s, ds ) = (s, evalOne m ds)

evalOne :: M.Map String DValue ->  DValue -> EitherDValue
evalOne m (DFunction (d,ds)) = (evalFunction (d) (ds) m)
evalOne m (DArray ds)  = evalArr m ds 
evalOne m d       = Right d  

evalArg :: M.Map String DValue ->  [DValue]  -> EitherDValues
evalArg  m (d) =  mapM (evalOne m) d

evalArr :: M.Map String DValue -> [DValue]  -> EitherDValue
evalArr m (d)  =  DArray <$> mapM (evalOne m)  d


evalFunction ::   String -> [DValue] -> M.Map String DValue -> EitherDValue
evalFunction f [] m = findEq f m
evalFunction f ds m = if  (isNativeFunction f)
					  then (execFunc $ findFuncNative f) =<< (evalArg m ds) 
					  else (applyToDValue $ findFunc f) =<< (evalArg m ds) 

findEq :: String  ->  M.Map String DValue  -> EitherDValue
findEq s m = (evalOne m $ fromJust (M.lookup s m))
