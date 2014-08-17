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

tableO = mkTableC p0
plotO  = mkPlotC p0
arrO   = mkArrC p0
objO   = mkObjC p0
strO   = mkStrU p0
numO   = mkNumU p0
boolO  = mkBoolU p0
nullO  = mkNullU p0

mkTableC = TableO .Calc
mkPlotC  = PlotO  .Calc
mkArrC   = ArrO   .Calc
mkObjC   = ObjO   .Calc
mkStrC   = StrO   .Calc
mkNumC   = NumO   .Calc
mkBoolC  = BoolO  .Calc
mkNullC  = NullO  .Calc

mkTableU = TableO .Upd
mkPlotU  = PlotO  .Upd
mkArrU   = ArrO   .Upd
mkObjU   = ObjO   .Upd
mkStrU   = StrO   .Upd
mkNumU   = NumO   .Upd
mkBoolU  = BoolO  .Upd
mkNullU  = NullO  .Upd

