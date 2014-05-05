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
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Aeson.Encode.Pretty 

type EvaluatedValue = ErrorT String (Writer [StackInfo]) DValue
type EvaluatedValues = ErrorT String (Writer [StackInfo]) [DValue]

data StackInfo = StackInfo (StackInfoType,String)
data StackInfoType = Function | Array | Argument | Equation | Bool | Number | Str deriving ( Show )

data EvalStatut = GoodEval | BadEval | ErrorEval 
data EvalResult = EvalResult EvalStatut String String

instance Show StackInfo where
	show (StackInfo (info,msg)) = "{" ++ (show $ show info) ++ ":" ++ show msg ++ "}"

instance Show EvalStatut where
	show GoodEval = "\"ok\""
	show BadEval = "\"tko\""
	show ErrorEval = "\"ko\""


instance Show EvalResult where 
	show (EvalResult statut res stack) = "{\"statut\":" ++ show statut ++ ",\"res\":" ++ res ++ ",\"stack\":" ++ stack ++ "}"


runParse s = case  (run bloc s) of 
		Right x -> do
			   result <- evalParse $ convertAllToPureValue x
			   return $ "{'parse':" ++ (show $ encode x) ++ ",'eval':" ++ result ++ "}" 
		Left  x -> return $ show $ x

evalParse :: [(String,Pvalue)] -> IO String
evalParse x = do
		putStrLn "Start Eval:\n"	
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
		return $ show $  EvalResult ErrorEval (show $ "Error") ("[" ++ s ++ "]")   


safePrint :: Show w => [w] -> IO String
safePrint (s:[]) = do
		   putStrLn $ show s
		   Except.catch (return $ show s) (\(x :: SomeException) -> return "{\"err\":\"Error\"}") 
safePrint (s:ss) = do
		   putStrLn $ show s
		   one <-  Except.catch (return $ show s) (\(x :: SomeException) -> return "{\"err\":\"Error\"}") --
		   many <- Except.catch (safePrint ss) (\(x :: SomeException) -> return "{\"err\":\"Error\"}") 
		   return $ one ++ "," ++ many
 

------------------------------------


evalEqs :: [(String, Pvalue)] -> EvaluatedValue
evalEqs (d) = evalEq (M.fromList d) (head d)

evalEq :: M.Map String Pvalue -> (String, Pvalue) -> EvaluatedValue
evalEq  m (s, ds ) = tell [StackInfo (Equation,s)] >> evalOne m ds

evalOne :: M.Map String Pvalue -> Pvalue -> EvaluatedValue
evalOne m (Pfunction (d)) = evalFunction (d) m
evalOne m (Parray ds)  = evalArr m ds
evalOne m (Pobj ds)  = evalObj m ds
evalOne m p =do 
	     --tell [StackInfo (Number,show p)]	
	     ErrorT (return (Right $ p_to_Dvalue p)) 


evalFunction ::   (String, [Pvalue]) -> M.Map String Pvalue -> EvaluatedValue
evalFunction (f,[]) m = tell [StackInfo (Equation , f ++ "")] >> findEq f m
evalFunction (f,ds) m =  do
		       tell [StackInfo (Function,f ++ "(")]
		       tell [StackInfo (Argument,"[")]
		       x <- evalArg m ds
		       tell [StackInfo (Argument,"]")]
		       tell [StackInfo (Function,")")] 
		       res <- ErrorT $ return $ ((if  (isSemiDirectFunction f)
					       then (applyToDValue (findFunc f) x) 
     			  		       else (applyOn  f x)))   		       
		       return  $ res

findEq :: String  ->  M.Map String Pvalue  -> EvaluatedValue
findEq s m = (evalOne m $ fromJust (M.lookup s m))


evalObjTuples ::  M.Map String Pvalue ->  [(String,Pvalue)] -> ErrorT String (Writer [StackInfo]) [(String,DValue)]
evalObjTuples m (ds) = (zip (map fst ds)) <$> (evalMany  m (map snd ds))



evalObj :: M.Map String Pvalue ->  [(String, Pvalue)] -> EvaluatedValue
evalObj m d = do
	 	x <- evalObjTuples m d
		return $ DObj x
		

evalMany ::  M.Map String Pvalue ->  [Pvalue] -> EvaluatedValues
evalMany m d = mapM (evalOne m )  d

evalArr :: M.Map String Pvalue -> [Pvalue]  -> EvaluatedValue
evalArr m (d)  = tell [StackInfo (Array,"[")] >> DArray <$> evalMany m  d

evalArg :: M.Map String Pvalue ->  [Pvalue]  -> EvaluatedValues
evalArg  m (d) =  evalMany m d

p_to_Dvalue :: Pvalue -> DValue 
p_to_Dvalue (Pnum x) = DNum x
p_to_Dvalue (Pstring x) = DString x
p_to_Dvalue (Pbool x) = DBool x


