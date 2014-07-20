module Common.Eval.FunctionEvalUtils where

import Prelude hiding (any)

import Control.Monad.Trans
import Data.Eval
import Data.ExpObj

import Common.Parser.MonolithicParserUtils

nbArgEntry s n = [(s,replicate n mockArg,error "FunctionEvalUtils::nbArgEntry::func [Should not be called]")]
mockArg        = error "FunctionEvalUtils::mockArg [Should not be called]"

singleTypeEntry s t = [(s,[t],  error "FunctionEvalUtils::singleTypeEntry::func [Should not be called]")]
doubleTypeEntry s t = [(s,[t,t],error "FunctionEvalUtils::doubleTypeEntry::func [Should not be called]")]

noArgFunc  s f = [(s,[],Func $ \p _ -> return $ f p)]
noArgFunc' s v = [(s,[],Func $ \_ _ -> return v)]

mockSuccess     = Right $ NullO p0
mockSuccessFunc = Func $ \_ _ -> lift mockSuccess

