{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Unit.Eval.FunctionEvalUtils where

import Prelude hiding (any)

import Control.Monad.State
import Eval.Function
import Parser.Monolithic

import Common.Parser.MonolithicParserUtils

runFuncWith s = evalStateT .marshall $ unsafeParse funcT s

