{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Prop.Eval.FunctionEvalTest where

import Prelude hiding (any)

import Control.Monad.State
import Data.EvalError
import Data.ExpObj
import Eval.Function
import Test.Framework

import Common.Eval.FunctionEvalUtils
import Prop.Eval.FunctionEvalUtils
import Unit.Parser.MonolithicParserUtils

{-# ANN module "HLint: ignore Use camelCase" #-}

prop_NbArgs (NbArgs p s n m) = n /= m ==> Left (ArgCountMismatch p s n m) == evalStateT (marshall $ mkFunc' p s $ replicate m mockArg) (nbArgEntry s n)

prop_TableTypeError (TableTypeError  e) = Left (TypeMismatch (getPos e) (Leaf Table) (getRoot e)) == evalStateT (matchType Table  e) []
prop_PlotTypeError  (PlotTypeError   e) = Left (TypeMismatch (getPos e) (Leaf Plot)  (getRoot e)) == evalStateT (matchType Plot   e) []
prop_ArrTypeError   (ArrTypeError    e) = Left (TypeMismatch (getPos e)  NodeArr     (getRoot e)) == evalStateT (matchType arr    e) []
prop_ObjTypeError   (ObjTypeError    e) = Left (TypeMismatch (getPos e)  NodeObj     (getRoot e)) == evalStateT (matchType obj    e) []
prop_StrTypeError   (StrTypeError    e) = Left (TypeMismatch (getPos e) (Leaf Str)   (getRoot e)) == evalStateT (matchType Str    e) []
prop_NumTypeError   (NumTypeError    e) = Left (TypeMismatch (getPos e) (Leaf Num)   (getRoot e)) == evalStateT (matchType Num    e) []
prop_BoolTypeError  (BoolTypeError   e) = Left (TypeMismatch (getPos e) (Leaf Bool)  (getRoot e)) == evalStateT (matchType Bool   e) []
prop_NullTypeError  (NullTypeError   e) = Left (TypeMismatch (getPos e) (Leaf Null)  (getRoot e)) == evalStateT (matchType Null   e) []

