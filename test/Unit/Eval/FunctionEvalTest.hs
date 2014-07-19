{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Unit.Eval.FunctionEvalTest where

import Prelude hiding (any)

import Data.EvalError
import Test.Framework

import Common.Eval.FunctionEvalUtils
import Unit.Eval.FunctionEvalUtils

{-# ANN module "HLint: ignore Use camelCase" #-}
{-# ANN module "HLint: ignore Reduce duplication" #-}

test_NbArgs = do     assertEqual (Left $ ArgCountMismatch (1,1) "f" 0 1) $ runFuncWith "f(0)"   $ nbArgEntry "f" 0
                     assertEqual (Left $ ArgCountMismatch (1,1) "f" 0 2) $ runFuncWith "f(0,1)" $ nbArgEntry "f" 0
                     assertEqual (Left $ ArgCountMismatch (1,1) "f" 1 0) $ runFuncWith "f()"    $ nbArgEntry "f" 1
                     assertEqual (Left $ ArgCountMismatch (1,1) "f" 1 2) $ runFuncWith "f(0,1)" $ nbArgEntry "f" 1
                     assertEqual (Left $ ArgCountMismatch (1,1) "f" 2 0) $ runFuncWith "f()"    $ nbArgEntry "f" 2
                     assertEqual (Left $ ArgCountMismatch (1,1) "f" 2 1) $ runFuncWith "f(0)"   $ nbArgEntry "f" 2













                    