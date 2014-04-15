{-# LANGUAGE ScopedTypeVariables #-}

import Evaluator.EvalParse
import System.Environment   
import Data.List 
import Control.Exception as Except

main = do
		 args <- return ["show=[[[addition(2,true)]]]"]
--(getArgs) 
		 s <- runParse $ (args)  !! 0
	  	 Except.catch (putStrLn (s)) (fatalError)



fatalError :: SomeException -> IO ()
fatalError e = putStrLn "An error when evaluating" 		 



