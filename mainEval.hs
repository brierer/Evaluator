import Evaluator.EvalParse
import System.Environment   
import Data.List 

main = do
		 args <- (getArgs) 
	  	 putStrLn $ evalParse $ args !! 0 
