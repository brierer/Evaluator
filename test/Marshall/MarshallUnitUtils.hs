{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Marshall.MarshallUnitUtils where

import Prelude hiding (any)

import Control.Monad.State
import Eval.Marshall
import Eval.Parser

import Parser.ParserUtils

runFuncWith s = evalStateT .marshall $ unsafeParse funcT s

