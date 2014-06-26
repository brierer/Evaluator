module Eval.Engine where

import Prelude hiding (exp,null)

import Data.Eval           (TypeValidator,Func)
import Eval.Function       (Marshallable,matrix,array,obj,num)

funcs :: Marshallable a => 
        [(String,       ([TypeValidator a],Func))]
funcs =   -- 1 arg functions
        [ ("show",       ([array'], showF))
        , ("multi",      ([array'], multiF))
        , ("mean",       ([array'], meanF))
        , ("descriptive",([array'], descF))
          -- 2 arg functions
        , ("table",      ([matrix,obj'], tableF))
        , ("nTimes",     ([num,num],    nTimesF))
        , ("take",       ([num,matrix], takeF))
        , ("sortTable",  ([num,matrix], sortTableF))
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
        