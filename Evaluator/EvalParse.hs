module Evaluator.EvalParse(
   evalParse,
   evalParse1,
   evalOne,
   evalArr,
   table,
   mean, 
 ) where

import Evaluator.DValue
import Evaluator.FunctionExc
import Evaluator.EqParser
import Evaluator.FunctionDV

import Data.Maybe
import Data.Dynamic
import Data.List
import Control.Applicative
import Statistics.Distribution.Normal as D
import qualified Data.Map as M



evalParse :: String -> String
evalParse s = case  (run bloc s) of
				Right x ->
				 case snd (evalEqs $ (x)) of
					Right y -> show y
					Left  y  ->  y
				Left x  -> show x 			

evalParse1 :: String -> String
evalParse1 s = case  (run bloc s) of
				Right x -> fst (evalEqs  $ (x)) 
				Left  x -> show x

evalEqs :: [(String, DValue)] -> (String, Either String DValue) 
evalEqs (d) = evalEq (M.fromList d) (head d)

evalEq :: M.Map String DValue -> (String, DValue) -> (String, Either String DValue) 
evalEq  m (s, ds ) = (s, evalOne m ds)

evalOne :: M.Map String DValue ->  DValue -> Either String DValue
evalOne m (DFunction (d,ds)) = evalFunction (d) (ds) m
evalOne m (DArray ds)  = evalArr m ds 
evalOne m d       = Right d  

evalArg :: M.Map String DValue ->  [DValue]  -> Either String [DValue]
evalArg  m (d) =  mapM (evalOne m) d

evalArr :: M.Map String DValue -> [DValue]  -> Either String DValue
evalArr m (d)  =  DArray <$> mapM (evalOne m)  d


evalFunction ::   String -> [DValue] -> M.Map String DValue -> Either String DValue
evalFunction f [] m = findEq f m
evalFunction f ds m = if  (isNativeFunction f)
					  then (execFunc $ findFuncNative f) =<< (evalArg m ds) 
					  else (applyToDValue $ findFunc f) =<< (evalArg m ds) 

findEq :: String  ->  M.Map String DValue  -> Either String DValue
findEq s m = (evalOne m $ fromJust (M.lookup s m))

findFuncNative :: String -> (Dynamic, String)
findFuncNative s = fromJust $ lookup s functionNative -- bug

findFunc :: String -> (Dynamic, String)
findFunc s = fromJust $ lookup s function -- bug

isNativeFunction :: String -> Bool
isNativeFunction f = case  (lookup f functionNative) of
						Nothing -> False
						_ -> True



functionNative = [
			 ("mean", (toDyn (mean), show $ typeOf(mean))),
			 ("normalDistr", (toDyn D.normalDistr, show $ typeOf(D.normalDistr))) ,
			 ("annuity", (toDyn annuity, show $ typeOf(annuity))),
			 ("add", (toDyn add, show $ typeOf(add))),
			 ("sum", (toDyn addV, show $ typeOf(addV))),
 			 ("sums", (toDyn sum2, show $ typeOf(sum2))),
			 ("multi", (toDyn multiplyV, show $ typeOf(multiplyV))),
			 ("slide", (toDyn slide, show $ typeOf(slide))),
			 ("nTimes", (toDyn nTimes, show $ typeOf(nTimes))),
			 ("c", (toDyn c, show $ typeOf(c)))
			]

function = [ ("table", (toDyn table, show $ typeOf(table))),
	     ("plotLine", (toDyn plotLine, show $ typeOf(plotLine))),
	     ("gt", (toDyn gt, show $ typeOf(gt))),
  	     ("sort", (toDyn sortD, show $ typeOf(sortD))),
 	     ("sortTable", (toDyn sortTable, show $ typeOf(sortTable))),
	     ("take", (toDyn takeD, show $ typeOf(takeD))),
  	     ("col", (toDyn col, show $ typeOf(col)))
	   ]

