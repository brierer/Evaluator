module Engine.EngineUnitSuccessUtils where

import Data.Eval
import Data.ExpObj
import Data.ExpToken

eval :: a -> Eval a
eval = return

mkDesc :: Pos -> Double -> Double -> Double -> Double -> Double -> Double -> ExpObj
mkDesc p co su me va sk ku = TableO p [[StrO p "count",StrO p "sum",StrO p "mean",StrO p "variance",StrO p "skewness",StrO p "kurtosis"],[NumO p co,NumO p su,NumO p me,NumO p va,NumO p sk,NumO p ku]] []
