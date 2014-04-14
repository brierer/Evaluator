{-# LANGUAGE ScopedTypeVariables #-}

import Evaluator.EvalParse
import System.Environment   
import Data.List 
import Control.Exception as Except

main = do
		 args <- return ["x=table([[2,3]],{a:2})"]
--(getArgs) 
		 s <- runParse $ (args)  !! 0
	  	 Except.catch (putStrLn (s)) (fatalError)



fatalError :: SomeException -> IO ()
fatalError e = putStrLn "An error when evaluating" 		 



