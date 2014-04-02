module Evaluator.EvalParse
 where

import Evaluator.DValue
import Evaluator.FunctionApp.SemiDirectApp
import Evaluator.FunctionApp.DynamicApp
import Evaluator.EqParser
import Evaluator.FunctionApp.FunctionDV
import Evaluator.FunctionApp.DirectApp
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
				Left x  -> ("" ,x) 			


------------------------------------


evalEqs :: [(String, Pvalue)] -> (String, EitherDValue) 
evalEqs (d) = evalEq (M.fromList d) (head d)

evalEq :: M.Map String Pvalue -> (String, Pvalue) -> (String, EitherDValue) 
evalEq  m (s, ds ) = evalOne m ds

evalOne :: M.Map String Pvalue ->  Pvalue -> (String,EitherDValue) 
evalOne m (Pfunction (d,ds)) = evalFunction (d) (ds) m
evalOne m (Parray ds)  = evalArr m ds
evalOne m (Pobj ds)  = evalArr m (map snd $ ds)
evalOne m d       = ("",Right $ p_to_Dvalue d) 

p_to_Dvalue :: Pvalue -> DValue 
p_to_Dvalue (Pnum x) = DNum x
p_to_Dvalue (Pstring x) = DString x
p_to_Dvalue (Pbool x) = DBool x



evalMany ::  M.Map String Pvalue ->  [Pvalue] -> EitherDValues 
evalMany m d =  mapM (\d -> snd $ evalOne m d)  d

evalArg :: M.Map String Pvalue ->  [Pvalue]  -> EitherDValues
evalArg  m (d) =  evalMany m d

evalArr :: M.Map String Pvalue -> [Pvalue]  -> (String, EitherDValue) 
evalArr m (d)  =  ("",DArray <$> evalMany m  d)


evalFunction ::   String -> [Pvalue] -> M.Map String Pvalue -> (String, EitherDValue) 
evalFunction f [] m = (f,findEq f m)
evalFunction f ds m = (f,if  (isSemiDirectFunction f)
					  then (applyToDValue $ findFunc f) =<< (evalArg m ds) 
					  else (applyOn $ f) =<< (evalArg m ds)) 

findEq :: String  ->  M.Map String Pvalue  -> EitherDValue
findEq s m = snd $ (evalOne m $ fromJust (M.lookup s m))
