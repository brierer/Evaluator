{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Exception   as Except
import           Data.List
import           Evaluator.EvalParse
import           System.Environment

main = do
                 args <- return ["show=[1,[2,plustwo(1),2.3],3]"]
--(getArgs)
                 s <- runParse $ (args)  !! 0
                 Except.catch (putStrLn (s)) (fatalError)



fatalError :: SomeException -> IO ()
fatalError e = putStrLn "An error when evaluating"

