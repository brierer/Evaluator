module Eval.EngineTestUtils where

import Control.Monad.Trans              (lift)
import Data.Eval                        (ExpObj(..),Func(..))
import Data.Token                       (IdToken(..),ExpToken(..))
import Eval.Engine                      (funcs)
import Eval.FunctionEvalTestUtils       (p0,w2)
import Parser.MonolithicParserTestUtils (uns)

fs = flip map funcs $ \(n,(typeValidators,_)) -> (n,(typeValidators, Func $ \_ _ -> success n))

mk ts = let es = uns ts; arg = ArrayT p0 w2 es in (es,arg)

oneArrayOfNum g1ras w1as = let es = uns g1ras; arg = ArrayO p0 es; (xs,g1) = addFunc "arrayOfNum" arg; (w1s,w1) = mk w1as in (xs,g1,w1s,w1)

addFunc fn r = let xs' = (fn,([],Func $ \_ _ -> return r)):fs; g = FuncT "" (IdT p0 ("","") fn) [] in (xs',g)

success n = lift $ Right $ StrO p0 $ "Mocked Success ["++n++"]"