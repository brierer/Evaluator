module Engine.EngineUnitUtils where

import Eval.Engine
import Eval.Marshall
import Eval.Parser

import Engine.EngineUtils
import Parser.ParserUtils

runWith s fs = marshallWith (unsafeParse funcT s) $ funcs ++ map (uncurry mkF) fs 
run  x = runWith x []
