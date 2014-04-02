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



evalParse :: String -> (String,String)
evalParse s = case  (run bloc s) of
				Right x ->
				 case (evalEqs $ (x)) of
					(v, Right y) -> (v , (show y))
					(v, Left  y) -> (v , (show y))
				Left x  -> ("" ,show x) 			


------------------------------------


evalEqs :: [(String, DValue)] -> (String, EitherDValue) 
evalEqs (d) = evalEq (M.fromList d) (head d)

evalEq :: M.Map String DValue -> (String, DValue) -> (String, EitherDValue) 
evalEq  m (s, ds ) = evalOne m ds

evalOne :: M.Map String DValue ->  DValue -> (String,EitherDValue) 
evalOne m (DFunction (d,ds)) = evalFunction (d) (ds) m
evalOne m (DArray ds)  = evalArr m ds
evalOne m d       = ("",Right d) 

evalMany ::  M.Map String DValue ->  [DValue] -> EitherDValues 
evalMany m d =  mapM (\d -> snd $ evalOne m d)  d

evalArg :: M.Map String DValue ->  [DValue]  -> EitherDValues
evalArg  m (d) =  evalMany m d

evalArr :: M.Map String DValue -> [DValue]  -> (String, EitherDValue) 
evalArr m (d)  =  ("",DArray <$> evalMany m  d)


evalFunction ::   String -> [DValue] -> M.Map String DValue -> (String, EitherDValue) 
evalFunction f [] m = (f,findEq f m)
evalFunction f ds m = (f,if  (isNativeFunction f)
					  then (execFunc $ findFuncNative f) =<< (evalArg m ds) 
					  else (applyToDValue $ findFunc f) =<< (evalArg m ds)) 

findEq :: String  ->  M.Map String DValue  -> EitherDValue
findEq s m = snd $ (evalOne m $ fromJust (M.lookup s m))
