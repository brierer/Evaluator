{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Prop.Eval.FunctionEvalTest where

import Data.List hiding (any)
import Prelude   hiding (any)

import qualified Prelude as P

import Control.Monad.State
import Data.EvalError
import Eval.Function
import Test.Framework

import Common.Eval.FunctionEvalUtils
import Prop.Eval.FunctionEvalUtils
import Unit.Parser.MonolithicParserUtils

{-# ANN module "HLint: ignore Use camelCase" #-}

prop_NbArgs (NbArgs p s n m) = n /= m ==> Left (ArgCountMismatch p s n m) == evalStateT (marshall $ mkFunc' p s $ replicate m mockArg) (nbArgEntry s n)













