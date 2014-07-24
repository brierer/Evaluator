module Engine.EngineUnitFailureUtils where

import Data.Type

showable   = [LeafTable,LeafPlot]
atom       = [LeafStr,LeafNum,LeafBool,LeafNull]
tableOrArr = [LeafTable,NodeArr] 
