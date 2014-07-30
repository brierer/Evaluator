module Engine.EngineUnitSuccessUtils where

import Data.Eval
import Data.ExpObj
import Data.ExpToken

eval :: a -> Eval a
eval = return

mkDesc :: Pos -> Double -> Double -> Double -> Double -> Double -> Double -> ExpObj
mkDesc q co su me va sk ku = let p = Calc q in TableO p [map (StrO p) ["count","sum","mean","variance","skewness","kurtosis"],map (NumO p)[co,su,me,va,sk,ku]] []
