module Marshall.MarshallUtils where

import Prelude hiding (any)

import Control.Monad.Trans
import Data.Eval
import Data.ExpObj

import Parser.ParserUtils

nbArgEntry s n = [(s,replicate n mockArg,error "FunctionEvalUtils::nbArgEntry::func [Should not be called]")]
mockArg        = error "FunctionEvalUtils::mockArg [Should not be called]"

singleTypeEntry s t = [(s,[t],  error "FunctionEvalUtils::singleTypeEntry::func [Should not be called]")]
doubleTypeEntry s t = [(s,[t,t],error "FunctionEvalUtils::doubleTypeEntry::func [Should not be called]")]

noArgFunc  s f = [(s,[],Func $ \p _ -> return $ f p)]
noArgFunc' s v = [(s,[],Func $ \_ _ -> return v)]

mockSuccess     = Right nullO
mockSuccessFunc = Func $ \_ _ -> lift mockSuccess

tableO = TableO p0
plotO  = PlotO p0
arrO   = ArrO p0
objO   = ObjO p0
strO   = StrO p0
numO   = NumO p0
boolO  = BoolO p0
nullO  = NullO p0