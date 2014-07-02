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
import Eval.Function       (Marshallable,arrayOf,table,plot,array,obj,num,(<|>))

funcs :: Marshallable a => [FuncEntry a]
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
showF      :: Marshallable a => Func a 
multiF     :: Marshallable a => Func a 
meanF      :: Marshallable a => Func a 
descF      :: Marshallable a => Func a 
tableF     :: Marshallable a => Func a 
nTimesF    :: Marshallable a => Func a 
takeF      :: Marshallable a => Func a 
sortF      :: Marshallable a => Func a 
colF       :: Marshallable a => Func a 
plotLineF  :: Marshallable a => Func a 

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
        