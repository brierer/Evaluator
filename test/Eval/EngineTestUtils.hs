module Eval.EngineTestUtils where

import Data.Eval                        (FuncEntry)
import Data.Token                       (IdToken(..),ExpToken(..))
import Eval.Engine                      (funcs)
import Eval.Function                    (Marshallable)
import Eval.FunctionEvalTestUtils       (p0)
import Parser.MonolithicParserTestUtils (uns)

fs :: Marshallable a => [FuncEntry a]
fs = map noCall funcs where
  noCall (n,(typeValidators,_)) = (n,(typeValidators,\_ -> error $ "Function ["++n++"] was called erroneously"))

mk ts = let es = uns ts; arg = ArrayT p0 ("","") es in (es,arg)
addFunc xs fn r = let xs' = (fn,([],\_ -> return r)):xs; g = FuncT "" (IdT p0 ("","") fn) [] in (xs',g)

