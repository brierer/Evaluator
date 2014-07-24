module Engine.EngineUnitUtils where

import Data.Eval
import Eval.Engine
import Eval.Marshall
import Eval.Parser

import Parser.ParserUtils

runWith s fs = marshallWith (unsafeParse funcT s) $ funcs ++ fs 
run  x = runWith x []

constF x = Func $ \_ _ -> return x
