{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Prop.Eval.FunctionEvalTest where

import Data.List     hiding (any)
import Prelude       hiding (any)

import qualified Prelude as P

import Control.Monad.State
import Data.EvalError
import Data.Type
import Eval.Function
import Eval.Type
import Test.Framework

import Common.Eval.FunctionEvalUtils

import Prop.Eval.FunctionEvalUtils

import Unit.Parser.MonolithicParserUtils

{-# ANN module "HLint: ignore Use camelCase"#-}

prop_NbArgs (NbArgs p s n m) = n /= m ==> Left (ArgCountMismatch p s n m) == evalStateT (marshall $ mkFunc' p s $ replicate m mockArg) (nbArgEntry s n)

prop_ArrLitFailure  s (ArrLitFailure  es e i) = caseLitFailure NodeArr  arr  s es e i
prop_ObjLitFailure  s (ObjLitFailure  es e i) = caseLitFailure NodeObj  obj  s es e i
prop_StrLitFailure  s (StrLitFailure  es e i) = caseLitFailure LeafStr  Str  s es e i    
prop_NumLitFailure  s (NumLitFailure  es e i) = caseLitFailure LeafNum  Num  s es e i
prop_BoolLitFailure s (BoolLitFailure es e i) = caseLitFailure LeafBool Bool s es e i
prop_NullLitFailure s (NullLitFailure es e i) = caseLitFailure LeafNull Null s es e i

prop_ArrOfLitFailure s (ArrOfLitFailure es e i t) = caseOfLitFailure t s es e i ArrOf
prop_ObjOfLitFailure s (ObjOfLitFailure es e i t) = caseOfLitFailure t s es e i ObjOf
prop_OrLitFailure    s (OrLitFailure    es e i t) = caseOrLitFailure t s es e i

prop_TableFuncFailure s (TableFuncFailure ts e i) = caseFuncFailure LeafTable Table s ts e i
prop_PlotFuncFailure  s (PlotFuncFailure  ts e i) = caseFuncFailure LeafPlot  Plot  s ts e i
prop_ArrFuncFailure   s (ArrFuncFailure   ts e i) = caseFuncFailure NodeArr   arr   s ts e i
prop_ObjFuncFailure   s (ObjFuncFailure   ts e i) = caseFuncFailure NodeObj   obj   s ts e i
prop_StrFuncFailure   s (StrFuncFailure   ts e i) = caseFuncFailure LeafStr   Str   s ts e i    
prop_NumFuncFailure   s (NumFuncFailure   ts e i) = caseFuncFailure LeafNum   Num   s ts e i
prop_BoolFuncFailure  s (BoolFuncFailure  ts e i) = caseFuncFailure LeafBool  Bool  s ts e i
prop_NullFuncFailure  s (NullFuncFailure  ts e i) = caseFuncFailure LeafNull  Null  s ts e i

prop_ArrOfFuncFailure s (ArrOfFuncFailure ts e i t) = caseOfFuncFailure t s ts e i ArrOf
prop_ObjOfFuncFailure s (ObjOfFuncFailure ts e i t) = caseOfFuncFailure t s ts e i ObjOf
prop_OrFuncFailure    s (OrFuncFailure    ts e i t) = caseOrFuncFailure t s ts e i

