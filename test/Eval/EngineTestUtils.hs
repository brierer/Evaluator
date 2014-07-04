module Eval.EngineTestUtils where

import qualified Eval.FunctionEvalTestUtils as FU (w2)

import Control.Applicative                        ((<$>),(<*>))
import Control.Monad.Trans                        (lift)
import Data.Eval                                  (EvalError(..),ExpObj(..),Func(..))
import Data.Token                                 (IdToken(..),ExpToken(..))
import Eval.Engine                                (funcs)
import Eval.FunctionEvalTestUtils                 (ExpOA,ExpTS,isTable,isPlot,p0, applyFunc)
import Parser.MonolithicParserTestUtils           (Unto,to,uns)
import Test.Framework                             ((==>))

ws2 = FU.w2

fs = flip map funcs $ \(n,(typeValidators,_)) -> (n,(typeValidators, Func $ \_ _ -> lift $ success n))

mk p ts = let es = uns ts; arg = ArrayT p FU.w2 es in (es,arg)

{-# ANN mk' "HLint: ignore Eta reduce" #-}
mk' ts = mk p0 ts

oneArrayOfNum g1ras w1as = let es = uns g1ras; arg = ArrayO p0 es; (xs,g1) = addFunc "arrayOfNum" arg; (w1s,w1) = mk' w1as in (xs,g1,w1s,w1)

addFunc  fn r = let xs' = (fn,([],Func $ \_ _ -> return r)):fs;    g = FuncT "" (IdT p0 ("","") fn) [] in (xs',g)
addFunc' fn r = let xs' = (fn,([],Func $ \_ _ -> return r)):funcs; g = FuncT "" (IdT p0 ("","") fn) [] in (xs',g)

success n = return $ StrO p0 $ "Mocked Success ["++n++"]"

toArray (x,y) e = ArrayT p0 FU.w2 $ replicate ((x+y) `mod` 100) e

tablesAndPlots :: [ExpOA] -> [ExpOA]
tablesAndPlots xs = let ys = uns xs :: [ExpObj] in  map to (filter ((||) <$> isTable <*> isPlot) ys)

emptySortColCase name pa pt n g2ass w2'ass = 
  let (g2ss,g2) = mk' g2ass; (_,w2) = mk pa ([] :: [ExpTS]); (_,w2') = mk' (w2'ass ++ [to w2]); in any (not.emptyArray) g2ss ==>
    Left (IllegalEmpty pa) == applyFunc fs pt name [n, w2 ] &&
    Left (IllegalEmpty pa) == applyFunc fs pt name [n, w2'] &&
    success name          == applyFunc fs pt name [n, g2 ]

emptyArray (ArrayT _ _ es) = null es
emptyArray e               = error $ "Eval.EngineTestUtils [Unexpected pattern ["++show e++"]]" 