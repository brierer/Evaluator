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

import Data.Eval           (FuncEntry,Func)
import Eval.Function       (arrayOf,table,plot,array,obj,num,(<|>))

funcs :: [FuncEntry]
funcs =   -- 1 arg functions
        [ ("show",       ([arrayOf $ table <|> plot                          ], showF))
        , ("multi",      ([arrayOf num                                       ], multiF))
        , ("mean",       ([arrayOf num                                       ], meanF))
        , ("descriptive",([arrayOf num                                       ], descF))
          -- 2 arg functions                                                   
        , ("table",      ([arrayOf array            , obj                    ], tableF))
        , ("nTimes",     ([num                      , num                    ], nTimesF))
        , ("take",       ([num                      , table <|> array        ], takeF))
        , ("sort",       ([num                      , table <|> arrayOf array], sortF))
        , ("col",        ([num                      , table <|> arrayOf array], colF))
          -- 3 arg functions
        , ("plotLine",   ([arrayOf num              , arrayOf num             , obj], plotLineF))
        ]

{-| Funcs -}
showF      :: Func 
multiF     :: Func 
meanF      :: Func 
descF      :: Func 
tableF     :: Func 
nTimesF    :: Func 
takeF      :: Func 
sortF      :: Func 
colF       :: Func 
plotLineF  :: Func 

showF      = error "Eval.Function::showF      [Not Implemented]"
multiF     = error "Eval.Function::multiF     [Not Implemented]"
meanF      = error "Eval.Function::meanF      [Not Implemented]"
descF      = error "Eval.Function::descF      [Not Implemented]"
tableF     = error "Eval.Function::tableF     [Not Implemented]"
nTimesF    = error "Eval.Function::ntimesF    [Not Implemented]"
takeF      = error "Eval.Function::takeF      [Not Implemented]"
sortF      = error "Eval.Function::sortF      [Not Implemented]"
colF       = error "Eval.Function::colF       [Not Implemented]"
plotLineF  = error "Eval.Function::plotLineF  [Not Implemented]"
        