{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Unit.Eval.TypeEvalUtils where

import Prelude hiding (any)

import Control.Arrow  (second)

import Control.Monad.State
import Data.Eval
import Data.ExpToken
import Data.ExpObj
import Eval.Function
import Parser.Monolithic

import Common.Parser.MonolithicParserUtils
import Prop.Parser.MonolithicParserUtils

matchTypeParse t s = matchTypeParseWith t s []
matchTypeParseWith t s fs = flip evalStateT [] $ matchType t $ unsafeMarshall fs $ unsafeParse expT s

unsafeMarshall fs (FuncT _ (IdT p _ n) es) = let Just (_,Func func) = lookup' n fs in unsafeRight $ evalStateT (func p $ map (unsafeMarshall fs) es) [] 
unsafeMarshall fs (ArrT p _ es)            = ArrO p $ map (unsafeMarshall fs) es
unsafeMarshall fs (ObjT p _ ps)            = ObjO p $ map (second (unsafeMarshall fs).un) ps
unsafeMarshall _  (StrT p _ v)             = StrO p v
unsafeMarshall _  (NumT p _ _ v)           = NumO p v
unsafeMarshall _  (BoolT p _ v)            = BoolO p v
unsafeMarshall _  (NullT p _)              = NullO p
unsafeMarshall _  x                        = error $ "FunctionEvalTestUnit::unsafeMarshall [Unexpected pattern ["++show x++"]]"

nullary n x = [(n,[],Func $ \_ _ -> return x)]

instance Unto PairToken (String,ExpToken) where un (PairT (IdT _ _ x) y) = (x,y); to = error "functionEvalTestUnit::to<PairToken,(String,ExpToken)> [Should not be called]"

lookup' n fs = lookup n $ map (\(x,y,z)->(x,(y,z))) fs

