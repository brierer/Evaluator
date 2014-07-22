{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Marshall.MarshallUnitUtils where

import Prelude hiding (any)

import Eval.Marshall
import Eval.Parser

import Parser.ParserUtils

runFuncWith s = marshallWith $ unsafeParse funcT s

