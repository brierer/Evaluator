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
import Data.Either


evalParse :: String -> (String,String)
evalParse s = case  (run bloc s) of
				Right x ->
				 case (evalEqs $ (x)) of
					Right y -> (string y , (show $ dvalue y))
					Left  y -> (y , y)
				Left x  -> ("" ,x) 			


------------------------------------


evalEqs :: [(String, Pvalue)] -> Either String DState 
evalEqs (d) = evalEq (M.fromList d) (head d)

evalEq :: M.Map String Pvalue -> (String, Pvalue) ->  Either String DState
evalEq  m (s, ds ) = evalOne' m ds


evalOne' :: M.Map String Pvalue -> Pvalue -> Either String DState
--evalOne' d = identity d --Eval (\v -> Right $ (d, p_to_Dvalue v))
evalOne' m (Pfunction (d,ds)) = evalFunction' (d) (ds) m
evalOne' m (Parray ds)  = evalArr' m ds
evalOne' m (Pobj ds)  = evalObj' m ds
evalOne' m p = Right $ DState { dvalue = p_to_Dvalue p, string = ""}



evalOne :: M.Map String Pvalue ->  Pvalue -> (String,EitherDValue) 
evalOne m (Pfunction (d,ds)) = evalFunction (d) (ds) m
evalOne m (Parray ds)  = evalArr m ds
evalOne m (Pobj ds)  = evalObj m ds
evalOne m d       = ("",Right $ p_to_Dvalue d) 

p_to_Dvalue :: Pvalue -> DValue 
p_to_Dvalue (Pnum x) = DNum x
p_to_Dvalue (Pstring x) = DString x
p_to_Dvalue (Pbool x) = DBool x

tupleIt' :: String -> DState -> (String,DValue) 
tupleIt' x y = (x,dvalue y)

tupleIt :: a -> b -> (a,b) 
tupleIt x y = (x,y)


evalObjTuples ::  M.Map String Pvalue ->  [(String,Pvalue)] -> Either String [(String,DValue)]
evalObjTuples m (ds) = (evalObjFst ds) <$> (evalMany  m (map snd ds))

evalObjTuples' ::  M.Map String Pvalue ->  [(String,Pvalue)] -> Either String [(String,DState)]
evalObjTuples' m (ds) =zipWith tupleIt (map fst ds) <$> (evalMany'  m (map snd ds))

evalObjFst :: [(String,Pvalue)] -> ([DValue] -> [(String,DValue)])
evalObjFst ds = (zipWith tupleIt (map fst ds))

convertTupleDState :: [(String,DState)] -> [(String,DValue)]
convertTupleDState (d:[]) = [(fst d, dvalue $ snd d)]
convertTupleDState (d:ds) = (fst d, dvalue $ snd d) : convertTupleDState ds

evalObj' :: M.Map String Pvalue ->  [(String,Pvalue)] -> Either String DState
evalObj' m (ds) =  f <$> convertTupleDState <$> evalObjTuples' m ds
		           where f  = (\x -> DState { dvalue = DObj x, string = ""})

evalObj :: M.Map String Pvalue ->  [(String,Pvalue)] -> (String, EitherDValue)
evalObj m (ds) =  ("", DObj <$> evalObjTuples m ds)

evalMany ::  M.Map String Pvalue ->  [Pvalue] -> EitherDValues 
evalMany m d =  mapM (\x -> snd $ evalOne m x)  d

evalMany' ::  M.Map String Pvalue ->  [Pvalue] -> Either String [DState] 
evalMany' m d =  mapM  (evalOne' m)  d

evalArg :: M.Map String Pvalue ->  [Pvalue]  -> EitherDValues
evalArg  m (d) =  evalMany m d

evalArg' :: M.Map String Pvalue ->  [Pvalue]  -> Either String [DState]
evalArg'  m (d) =  evalMany' m d

evalArr' :: M.Map String Pvalue -> [Pvalue]  -> Either String DState
evalArr' m (d)  =  f   <$> evalMany m d
		           where f  = (\x -> DState { dvalue = DArray x, string = ""})


evalArr :: M.Map String Pvalue -> [Pvalue]  -> (String, EitherDValue) 
evalArr m (d)  =  ("",DArray <$> evalMany m  d)


evalFunction ::   String -> [Pvalue] -> M.Map String Pvalue -> (String, EitherDValue) 
evalFunction f [] m = (f,findEq f m)
evalFunction f ds m = (f,if  (isSemiDirectFunction f)
					  then (applyToDValue $ findFunc f) =<< (evalArg m ds) 
					  else (applyOn $ f) =<< (evalArg m ds)) 

evalFunction' ::   String -> [Pvalue] -> M.Map String Pvalue -> Either String DState
evalFunction' f [] m = findEq' f m 
evalFunction' f ds m = g <$>
					  (if  (isSemiDirectFunction f)
					  then (applyToDValue $ findFunc f) =<< (evalArrDValue m ds) 
					  else (applyOn $ f) =<< (evalArrDValue m ds))
					  where g  = (\x -> DState { dvalue = x, string = f})

evalArrDValue m ds = listOfDValue (evalArg' m ds)

listOfDValue :: Either String [DState] -> Either String [DValue]
listOfDValue ds = map dvalue <$> ds 
 

findEq' :: String  ->  M.Map String Pvalue  -> Either String DState
findEq' s m = (evalOne' m $ fromJust (M.lookup s m))

findEq :: String  ->  M.Map String Pvalue  -> EitherDValue
findEq s m = snd $ (evalOne m $ fromJust (M.lookup s m))
