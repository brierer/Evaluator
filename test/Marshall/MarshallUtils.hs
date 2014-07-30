module Marshall.MarshallUtils where

import Prelude hiding (any)

import Control.Monad.Trans
import Data.Eval
import Data.ExpObj

import Parser.ParserUtils

nbArgEntry s n = [(s,replicate n mockArg,error "MarshallUtils::nbArgEntry::func [Should not be called]")]
mockArg        = error "MarshallUtils::mockArg [Should not be called]"

singleTypeEntry s t = [(s,[t],  error "MarshallUtils::singleTypeEntry::func [Should not be called]")]
doubleTypeEntry s t = [(s,[t,t],error "MarshallUtils::doubleTypeEntry::func [Should not be called]")]

noArgFunc  s f = [(s,[],Func $ \p _ -> return $ f p)]
noArgFunc' s v = [(s,[],Func $ \_ _ -> return v)]

mockSuccess     = Right nullO
mockSuccessFunc = Func $ \_ _ -> lift mockSuccess

tableO = mkTableO p0
plotO  = mkPlotO p0
arrO   = mkArrO p0
objO   = mkObjO p0
strO   = mkStrO p0
numO   = mkNumO p0
boolO  = mkBoolO p0
nullO  = mkNullO p0

mkTableO = TableO .Calc
mkPlotO  = PlotO  .Calc
mkArrO   = ArrO   .Calc
mkObjO   = ObjO   .Calc
mkStrO   = StrO   .Upd
mkNumO   = NumO   .Upd
mkBoolO  = BoolO  .Upd
mkNullO  = NullO  .Upd

mkNumO'  = NumO .Calc