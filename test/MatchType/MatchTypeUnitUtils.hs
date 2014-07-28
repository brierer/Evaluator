{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module MatchType.MatchTypeUnitUtils where

import Prelude hiding (any)

import Control.Arrow  (second)

import Control.Monad.State
import Data.Eval
import Data.EvalError
import Data.ExpToken
import Data.ExpObj
import Data.Maybe
import Eval.Parser

import MatchType.MatchTypeUtils
import Parser.ParserPropUtils
import Parser.ParserUtils

matchTypeParse t s = matchTypeParseWith t s []
matchTypeParseWith t s fs = simpleMatch (unsafeMarshall fs $ unsafeParse expT s) t

unsafeMarshall fs (FuncT _ (IdT p _ n) es) = let (_,func) = unsafeLookup n fs in unsafeRight $ evalStateT (invoke func p $ map (unsafeMarshall fs) es) []
unsafeMarshall fs (ArrT p _ es)            = ArrO p $ map (unsafeMarshall fs) es
unsafeMarshall fs (ObjT p _ ps)            = ObjO p $ map (second (unsafeMarshall fs).un) ps
unsafeMarshall _  (StrT p _ v)             = StrO p v
unsafeMarshall _  (NumT p _ _ v)           = NumO p v
unsafeMarshall _  (BoolT p _ v)            = BoolO p v
unsafeMarshall _  (NullT p _)              = NullO p
unsafeMarshall _  x                        = error $ "MatchTypeUnitUtils::unsafeMarshall [Unexpected pattern ["++show x++"]]"

nullary n x = [(n,[],Func $ \_ _ -> return x)]

instance Unto PairToken (String,ExpToken) where un (PairT (IdT _ _ x) y) = (x,y); to = error "functionEvalTestUnit::to<PairToken,(String,ExpToken)> [Should not be called]"

unsafeLookup n fs = fromMaybe (error $ "MatchTypeUnitUtils::unsafeLookup [Failed lookup for ["++show n++"]]") $ lookup n $ map (\(x,y,z)->(x,(y,z))) fs

leafTypeMismatch x y = Left . TypeMismatch . TMLeaf x y
nodeTypeMismatch     = Left . TypeMismatch . TMNode
