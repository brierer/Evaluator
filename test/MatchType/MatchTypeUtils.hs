module MatchType.MatchTypeUtils where

import Control.Monad.State
import Eval.MatchType

matchWith   e t = evalStateT $ matchType t e
simpleMatch e t = matchWith e t []