module Engine.EngineUnitUtils where

import Data.Eval
import Eval.Engine
import Eval.Marshall
import Eval.Parser

import Parser.ParserUtils

runWith s fs = marshallWith (unsafeParse funcT s) $ funcs ++ map (uncurry mkF) fs 
run  x = runWith x []

mkF s x = (s,[],Func $ \_ _ -> return x)
