{-# LANGUAGE ScopedTypeVariables #-}

import Evaluator.EvalParse
import System.Environment   
import Data.List 
import Control.Exception as Except

main = do
		 args <- (getArgs) 
		 Except.catch  (putStrLn $ evalParse $ args !! 0) printErr


test f x = f x 
test2 f (x:y:[]) = f x y 

printErr :: SomeException -> IO ()
printErr e =  do
        case fromException e of
                Just (x:: PatternMatchFail) -> putStrLn "I caught the exception" >> print x
                nothing -> return ()
