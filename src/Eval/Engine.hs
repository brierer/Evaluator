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
        [ ("show",       ([arrayOf $ table' <|> plot'                                             ], showF))
        , ("multi",      ([arrayOf num                                                            ], multiF))
        , ("mean",       ([arrayOf num                                                            ], meanF))
        , ("descriptive",([arrayOf num                                                            ], descF))
          -- 2 arg functions                                                                      
        , ("table",      ([arrayOf array'            , obj'                                       ], tableF))
        , ("nTimes",     ([num                       , num                                        ], nTimesF))
        , ("take",       ([num                       , table' <|> array'                          ], takeF))
        , ("sort",       ([num                       , table funcs <|> arrayOf (array funcs)      ], sortF))
        , ("col",        ([num                       , table funcs <|> arrayOf (array funcs)      ], colF))
          -- 3 arg functions
        , ("plotLine",   ([arrayOf num               , arrayOf num                          , obj'], plotLineF))
        ] where table' = table funcs; plot' = plot funcs; array' = array funcs; obj' = obj funcs

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

showF _     = error "Eval.Function::showF      [Not Implemented]"
multiF _    = error "Eval.Function::multiF     [Not Implemented]"
meanF _     = error "Eval.Function::meanF      [Not Implemented]"
descF _     = error "Eval.Function::descF      [Not Implemented]"
tableF _    = error "Eval.Function::tableF     [Not Implemented]"
nTimesF _   = error "Eval.Function::ntimesF    [Not Implemented]"
takeF _     = error "Eval.Function::takeF      [Not Implemented]"
sortF _     = error "Eval.Function::sortF      [Not Implemented]"
colF _      = error "Eval.Function::colF       [Not Implemented]"
plotLineF _ = error "Eval.Function::plotLineF  [Not Implemented]"
        