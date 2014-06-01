{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}


module Evaluator.DValue_test     
 where

import Evaluator.EvalParse
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

args1 =  [Pnum 3.0, Pfunction fy]
args2 = [Pnum 3.0, Pbool True]
f1 = ("addition",args1)
f2 = ("addition",args2)
fy = ("y",[])

m = M.fromList $ [("x",Pfunction f1),("y",Pfunction f2)]


evalFunction_test_f2 = evalFunction f2 m
evalArg_test = evalArg m args2
applyOn_test = do
		x <- evalArg m args2	
		ErrorT $ return $applyOn (fst f2) (x)

