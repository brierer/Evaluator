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

import Data.Eval      (ExpObj(..),EvalFunc,FuncEntry,Func(..))
--import Data.List      (genericLength)
import Data.Token     (Pos)
import Eval.Function  (table,plot,array,obj,num,arrayOf,nonEmpty,(<|>))

funcs :: [FuncEntry]
funcs = -- 1 arg functions
        [ ("show",       ([arrayOf $ table <|> plot                      ], Func showF))
        , ("multi",      ([nonEmpty $ arrayOf num                        ], Func multiF))
        , ("mean",       ([nonEmpty $ arrayOf num                        ], Func meanF))
        , ("descriptive",([nonEmpty $ arrayOf num                        ], Func descF))
          -- 2 arg functions                                                              
        , ("table",      ([tableArg                , obj                 ], Func tableF))
        , ("nTimes",     ([num                     , num                 ], Func nTimesF))
        , ("take",       ([num                     , table <|> array     ], Func takeF))
        , ("sort",       ([num                     , table <|> tableArg  ], Func sortF))
        , ("col",        ([num                     , table <|> tableArg  ], Func colF))
          -- 3 arg functions                       
        , ("plotLine",   ([arrayOf num             , arrayOf num    , obj], Func plotLineF))
        ] where tableArg = nonEmpty $ arrayOf $ nonEmpty array

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

--showF  p [x]           = return $ ObjO p [("result",x)];                                         showF  _ xs = error $ "Engine::showF  [Unexpected pattern ["++show xs++"]]"
--multiF p [ArrayO q ns] = return $ NumO p $ product $ getNums ns;                                 multiF _ xs = error $ "Engine::multiF [Unexpected pattern ["++show xs++"]]"
--meanF  p [ArrayO q ns] = return $ NumO p $ let vs = getNums ns in product vs / genericLength vs; meanF  _ xs = error $ "Engine::meanF  [Unexpected pattern ["++show xs++"]]"

showF        = error "Eval.Function::showF      [Not Implemented]"
multiF       = error "Eval.Function::multiF     [Not Implemented]"
meanF        = error "Eval.Function::meanF      [Not Implemented]"
descF        = error "Eval.Function::descF      [Not Implemented]"
tableF       = error "Eval.Function::tableF     [Not Implemented]"
nTimesF      = error "Eval.Function::ntimesF    [Not Implemented]"
takeF        = error "Eval.Function::takeF      [Not Implemented]"
sortF        = error "Eval.Function::sortF      [Not Implemented]"
colF         = error "Eval.Function::colF       [Not Implemented]"
plotLineF    = error "Eval.Function::plotLineF  [Not Implemented]"

--getNums :: [ExpObj] -> [Double]
--getNums = map (\(NumO _ x)-> x)






        