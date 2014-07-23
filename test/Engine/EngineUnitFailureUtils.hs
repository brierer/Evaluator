module Engine.EngineUnitFailureUtils where

import Data.Eval
import Data.Type
import Eval.Engine
import Eval.Marshall
import Eval.Parser

import Parser.ParserUtils

runWith s fs = marshallWith (unsafeParse funcT s) $ funcs ++ fs 
run  x = runWith x []

constF x = Func $ \_ _ -> return x

showable   = [LeafTable,LeafPlot]
atom       = [LeafStr,LeafNum,LeafBool,LeafNull]
tableOrArr = [LeafTable,NodeArr] 
