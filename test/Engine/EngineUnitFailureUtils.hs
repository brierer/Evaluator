module Engine.EngineUnitFailureUtils where

import Data.Eval
import Data.EvalError
import Data.Type
import Eval.Engine
import Eval.Marshall
import Eval.Parser

import Parser.ParserUtils

runWith s fs = marshallWith (unsafeParse funcT s) $ funcs ++ fs 
run  x = runWith x []

constF x = Func $ \_ _ -> return x

leafTypeMismatch x y = Left . TypeMismatch . TMLeaf x y
nodeTypeMismatch     = Left . TypeMismatch . TMNode
orTypeMismatch x y   = Left . TypeMismatch . orTMT x y

orTMT p ts t2 = TMNode $ map (flip (TMLeaf p) t2) ts

showable   = [LeafTable,LeafPlot]
atom       = [LeafStr,LeafNum,LeafBool,LeafNull]
tableOrArr = [LeafTable,NodeArr] 