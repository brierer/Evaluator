{-# LANGUAGE ScopedTypeVariables #-}
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
import Control.Monad.Writer
import Control.Monad.Trans.Maybe
import Control.Monad.Error
import Control.Exception as Except
import System.Environment   


type EvaluatedValue = ErrorT String (Writer [String]) DValue
type EvaluatedValues = ErrorT String (Writer [String]) [DValue]


data EvalStatut = GoodEval | BadEval | ErrorEval 
data EvalResult = EvalResult EvalStatut String String

instance Show EvalStatut where
	show GoodEval = "'ok'"
	show BadEval = "'tko'"
	show ErrorEval = "'ko'"


instance Show EvalResult where 
	show (EvalResult statut res stack) = "{'statut':" ++ show statut ++ ",'res':" ++ res ++ ",'stack':" ++ stack ++ "}"


runParse s = case  (run bloc s) of 
		Right x -> evalParse x
		Left  x -> return $ show $ x

evalParse :: [(String,Pvalue)] -> IO String
evalParse x = do	
                let res =  runWriter $ runErrorT $ evalEqs $ (x)
	        Except.catch (case res of
					(Right x,w) -> goodEval x w 
			       		(Left  x,w) -> badEval x w
			      ) (errorEval $ snd $ res) 			

goodEval :: (Show a, Show w) => a -> w -> IO String 
goodEval a w  =do
		let s = show $  EvalResult GoodEval (show a) (show w)  
		putStrLn $ s
		return $ s
 
	
badEval :: (Show a, Show w) =>   a -> w -> IO String
badEval a w  = do 
	       let s =  show $ EvalResult BadEval (show a) (show w)  
	       putStrLn s 	
	       return $ s
 
errorEval :: Show w =>  [w] -> Except.SomeException -> IO String
errorEval w e=  do
		s <- (safePrint $ w)   
		return $ show $  EvalResult ErrorEval ("["++ s ++ "]") (show e)   


safePrint :: Show w => [w] -> IO String
safePrint (s:[]) = do
		   putStrLn $ show s
		   Except.catch (return $ show s) (\(x :: SomeException) -> return "'Error'") 
safePrint (s:ss) = do
		   putStrLn $ show s
		   one <-  Except.catch (return $ show s) (\(x :: SomeException) -> return "'Error'") --
		   many <- Except.catch (safePrint ss) (\(x :: SomeException) -> return "'Error'") 
		   return $ one ++ "," ++ many
 

------------------------------------


evalEqs :: [(String, Pvalue)] -> EvaluatedValue
evalEqs (d) = evalEq (M.fromList d) (head d)

evalEq :: M.Map String Pvalue -> (String, Pvalue) -> EvaluatedValue
evalEq  m (s, ds ) = tell [s] >> evalOne m ds

evalOne :: M.Map String Pvalue -> Pvalue -> EvaluatedValue
evalOne m (Pfunction (d)) = evalFunction (d) m
evalOne m (Parray ds)  = evalArr m ds
evalOne m (Pobj ds)  = evalObj m ds
evalOne m p =do 
	     tell ["Value"]	
	     ErrorT (return (Right $ p_to_Dvalue p)) 


evalFunction ::   (String, [Pvalue]) -> M.Map String Pvalue -> EvaluatedValue
evalFunction (f,[]) m = tell [f] >> findEq f m
evalFunction (f,ds) m =  do
		       tell ["Eval Function:"]		
		       tell [f]  
		       --tell ["Ok Args:"]
		       x <- evalArg m ds
		       tell ["Ok Args:"]
		       tell ["Ok eval:"] 
		       res <- ErrorT $ return $ ((if  (isSemiDirectFunction f)
					       then (applyToDValue (findFunc f) x) 
     			  		       else (applyOn  f x)))   		       
		       return  $ res

findEq :: String  ->  M.Map String Pvalue  -> EvaluatedValue
findEq s m = (evalOne m $ fromJust (M.lookup s m))


evalObjTuples ::  M.Map String Pvalue ->  [(String,Pvalue)] -> ErrorT String (Writer [String]) [(String,DValue)]
evalObjTuples m (ds) = (zip (map fst ds)) <$> (evalMany  m (map snd ds))



evalObj :: M.Map String Pvalue ->  [(String, Pvalue)] -> EvaluatedValue
evalObj m d = do
	 	x <- evalObjTuples m d
		return $ DObj x
		

evalMany ::  M.Map String Pvalue ->  [Pvalue] -> EvaluatedValues
evalMany m d = mapM (evalOne m )  d

evalArr :: M.Map String Pvalue -> [Pvalue]  -> EvaluatedValue
evalArr m (d)  = tell ["Array:"] >> DArray <$> evalMany m  d

evalArg :: M.Map String Pvalue ->  [Pvalue]  -> EvaluatedValues
evalArg  m (d) =  evalMany m d

p_to_Dvalue :: Pvalue -> DValue 
p_to_Dvalue (Pnum x) = DNum x
p_to_Dvalue (Pstring x) = DString x
p_to_Dvalue (Pbool x) = DBool x

{-
evalOne' :: M.Map String Pvalue -> Pvalue -> Either String DState
evalOne' m (Pfunction (d,ds)) = evalFunction' (d) (ds) m
evalOne' m (Parray ds)  = evalArr' m ds
evalOne' m (Pobj ds)  = evalObj' m ds
evalOne' m p = Right $ DState { dvalue = p_to_Dvalue p, string = show p}



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

tupleIt :: a -> EvaluatedValue -> (a,b) 
tupleIt x y = (x,y)


evalObjTuples ::  M.Map String Pvalue ->  [(String,Pvalue)] -> Either String [(String,DValue)]
evalObjTuples m (ds) = (evalObjFst ds) <$> (evalMany  m (map snd ds))

evalObjTuples' ::  M.Map String Pvalue ->  [(String,Pvalue)] -> Either String [(String,DState)]
evalObjTuples' m (ds) =zipWith tupleIt (map fst ds) <$> (evalMany'  m (map snd ds))

evalObjFst :: [(String,Pvalue)] -> ([DValue] -> [(String,DValue)])
evalObjFst ds = (zipWith tupleIt (map fst ds))


evalObj' :: M.Map String Pvalue ->  [(String,Pvalue)] -> Either String DState
evalObj' m (ds) =  f <$> convertTupleDState <$> tuples
		           where 
		           	tuples = evalObjTuples' m ds		
		           	f  = (\x -> DState { dvalue = DObj x, string = concatStringTuple tuples})
		                   



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
evalArr' m (d)  =  f   <$> convertListOfDState  many
		           where 
		           many = evalMany' m d
		           f  = (\x -> DState { dvalue = DArray x, string = concatString many})


evalArr :: M.Map String Pvalue -> [Pvalue]  -> (String, EitherDValue) 
evalArr m (d)  =  ("",DArray <$> evalMany m  d)


evalFunction ::   String -> [Pvalue] -> M.Map String Pvalue -> (String, EitherDValue) 
evalFunction f [] m = (f,findEq f m)
evalFunction f ds m = (f,if  (isSemiDirectFunction f)
					  then (applyToDValue $ findFunc f) =<< (evalArg m ds) 
					  else (applyOn $ f) =<< (evalArg m ds)) 

evalFunction' ::   String -> [Pvalue] -> M.Map String Pvalue -> Either String DState
evalFunction' f [] m = commentStack f <$> findEq' f m 
evalFunction' f ds m = g <$>
					  (if  (isSemiDirectFunction f)
					  then (applyToDValue $ findFunc f) =<< (convertListOfDState arguments) 
					  else (applyOn $ f) =<< (convertListOfDState arguments))
					  where 
					  	arguments = (evalArg' m ds)
					  	g  = (\x -> DState { dvalue = x, string = f ++ (concatString arguments)})

evalArrDValue m ds = convertListOfDState (evalArg' m ds)



findEq' :: String  ->  M.Map String Pvalue  -> Either String DState
findEq' s m = (evalOne' m $ fromJust (M.lookup s m))

findEq :: String  ->  M.Map String Pvalue  -> EitherDValue
findEq s m = snd $ (evalOne m $ fromJust (M.lookup s m))
-}
