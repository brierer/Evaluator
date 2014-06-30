module Eval.Engine
( funcs
, showF
, multiF
, meanF
, descF
, tableF
, nTimesF
, takeF
, sortTableF
, plotLineF
) where

import Prelude hiding (exp,null)

import Data.Eval           (FuncEntry,Func)
import Eval.Function       (Marshallable,arrayOf,table,plot,array,obj,num,(<|>))

funcs :: Marshallable a => [FuncEntry a]
funcs =   -- 1 arg functions
        [ ("show",       ([arrayOf $ table funcs <|> plot funcs], showF))
        , ("multi",      ([arrayOf num], multiF))
        , ("mean",       ([arrayOf num], meanF))
        , ("descriptive",([arrayOf num], descF))
          -- 2 arg functions
        , ("table",      ([array',obj'], tableF))
        , ("nTimes",     ([num,num],    nTimesF))
        , ("take",       ([num,array'], takeF))
        , ("sortTable",  ([num,array'], sortTableF))
          -- 3 arg functions
        , ("plotLine",   ([array',array',obj'],plotLineF))
        ] where array' = array funcs; obj' = obj funcs

{-| Funcs -}
showF      :: Func
multiF     :: Func
meanF      :: Func
descF      :: Func
tableF     :: Func
nTimesF    :: Func
takeF      :: Func
sortTableF :: Func
plotLineF  :: Func

showF _      = error "Eval.Function::showF  [Not Implemented]"
multiF _     = error "Eval.Function::multiF [Not Implemented]"
meanF _      = error "Eval.Function::meanF  [Not Implemented]"
descF _      = error "Eval.Function::descF  [Not Implemented]"
tableF _     = error "Eval.Function::tableF     [Not Implemented]"
nTimesF _    = error "Eval.Function::ntimesF    [Not Implemented]"
takeF _      = error "Eval.Function::takeF      [Not Implemented]"
sortTableF _ = error "Eval.Function::sortTableF [Not Implemented]"
plotLineF _  = error "Eval.Function::plotLineF [Not Implemented]"
        