module Eval.Engine
( funcs
, showF
, multiF
, meanF
, descF
, tableF
, nTimesF
, takeF
, sortF
, plotLineF
) where

import Prelude hiding (exp,null)

import Control.Monad.Trans (lift)
import Data.Eval           (EvalError(..),ExpObj(..),EvalFunc,FuncEntry,Func(..))
import Data.List           (genericLength)
import Data.Token          (Pos)
import Eval.Function       (arrayOf,table,plot,array,obj,num,(<|>))

funcs :: [FuncEntry]
funcs =   -- 1 arg functions
        [ ("show",       ([arrayOf $ table <|> plot                                ], Func showF))
        , ("multi",      ([arrayOf num                                             ], Func multiF))
        , ("mean",       ([arrayOf num                                             ], Func meanF))
        , ("descriptive",([arrayOf num                                             ], Func descF))
          -- 2 arg functions                                                          
        , ("table",      ([arrayOf array            , obj                          ], Func tableF))
        , ("nTimes",     ([num                      , num                          ], Func nTimesF))
        , ("take",       ([num                      , table <|> array              ], Func takeF))
        , ("sort",       ([num                      , table <|> arrayOf array      ], Func sortF))
        , ("col",        ([num                      , table <|> arrayOf array      ], Func colF))
          -- 3 arg functions
        , ("plotLine",   ([arrayOf num              , arrayOf num             , obj], Func plotLineF))
        ]

{-| Funcs -}
showF      :: Pos -> [ExpObj] -> EvalFunc ExpObj 
multiF     :: Pos -> [ExpObj] -> EvalFunc ExpObj 
meanF      :: Pos -> [ExpObj] -> EvalFunc ExpObj 
descF      :: Pos -> [ExpObj] -> EvalFunc ExpObj 
tableF     :: Pos -> [ExpObj] -> EvalFunc ExpObj 
nTimesF    :: Pos -> [ExpObj] -> EvalFunc ExpObj 
takeF      :: Pos -> [ExpObj] -> EvalFunc ExpObj 
sortF      :: Pos -> [ExpObj] -> EvalFunc ExpObj 
colF       :: Pos -> [ExpObj] -> EvalFunc ExpObj 
plotLineF  :: Pos -> [ExpObj] -> EvalFunc ExpObj 

showF  p [x]           = return $ ObjO p [("result",x)];                                         showF  _ xs = error $ "Engine::showF  [Unexpected pattern ["++show xs++"]]"
multiF p [ArrayO _ ns] = return $ NumO p $ product $ getNums ns;                                 multiF _ xs = error $ "Engine::multiF [Unexpected pattern ["++show xs++"]]"

meanF  _ [ArrayO p []] = lift $ Left $ IllegalEmptyArray p "mean"
meanF  p [ArrayO _ ns] = return $ NumO p $ let vs = getNums ns in product vs / genericLength vs; meanF  _ xs = error $ "Engine::meanF  [Unexpected pattern ["++show xs++"]]"


descF        = error "Eval.Function::descF      [Not Implemented]"
tableF       = error "Eval.Function::tableF     [Not Implemented]"
nTimesF      = error "Eval.Function::ntimesF    [Not Implemented]"
takeF        = error "Eval.Function::takeF      [Not Implemented]"
sortF        = error "Eval.Function::sortF      [Not Implemented]"
colF         = error "Eval.Function::colF       [Not Implemented]"
plotLineF    = error "Eval.Function::plotLineF  [Not Implemented]"

getNums :: [ExpObj] -> [Double]
getNums = map (\(NumO _ x)-> x)






        