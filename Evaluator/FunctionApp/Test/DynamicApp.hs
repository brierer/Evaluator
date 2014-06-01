{-# LANGUAGE ScopedTypeVariables #-}

module Evaluator.FunctionApp.Test.DynamicApp
where


import Evaluator.FunctionApp.SemiDirectApp
import Evaluator.FunctionApp.FunctionDV
import Evaluator.DValue
import Control.Exception as Except

x = applyToDValue (findFunc "gt") [DString "salut" , DNum 4.0] 
y = applyToDValue (findFunc "gt") [DString "salut" , DNum 4.0] 



test = do
		 Except.catch  ( do 
				 putStrLn $ show $ x
				 putStrLn $ show $ y) printErr


printErr :: SomeException -> IO ()
printErr e =  do
        case fromException e of
                Just (x:: PatternMatchFail) -> putStrLn "I caught the exception" >> print x
                nothing -> return ()
