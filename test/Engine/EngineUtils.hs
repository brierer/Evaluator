module Engine.EngineUtils where

import Data.Eval
import Data.Type

showable   = [LeafTable,LeafPlot]
atom       = [LeafStr,LeafNum,LeafBool,LeafNull]
tableOrArr = [LeafTable,NodeArr]

mkF s x = (s,[],Func $ \_ _ -> return x)