{-# LANGUAGE ScopedTypeVariables #-}

import Evaluator.EvalParse
import System.Environment   
import Data.List 
import Control.Exception as Except

main = do
		 args <- (getArgs) 
		 let  result = evalParse $ args !! 0
		 Except.catch  (putStrLn $ snd $ result)  (printErr $ fst result )


test f x = f x 
test2 f (x:y:[]) = f x y 

printErr :: String -> SomeException -> IO ()
printErr s e =  do
         	putStrLn $ "Error:" ++ s
