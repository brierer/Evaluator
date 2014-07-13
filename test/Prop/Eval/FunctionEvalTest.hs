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

prop_TableTypeFailure (TableTypeFailure  e) = Left (TypeMismatch (getPos e) (Leaf Table) (getRoot e)) == evalStateT (matchType Table  e) []
prop_PlotTypeFailure  (PlotTypeFailure   e) = Left (TypeMismatch (getPos e) (Leaf Plot)  (getRoot e)) == evalStateT (matchType Plot   e) []
prop_ArrTypeFailure   (ArrTypeFailure    e) = Left (TypeMismatch (getPos e)  NodeArr     (getRoot e)) == evalStateT (matchType arr    e) []
prop_ObjTypeFailure   (ObjTypeFailure    e) = Left (TypeMismatch (getPos e)  NodeObj     (getRoot e)) == evalStateT (matchType obj    e) []
prop_StrTypeFailure   (StrTypeFailure    e) = Left (TypeMismatch (getPos e) (Leaf Str)   (getRoot e)) == evalStateT (matchType Str    e) []
prop_NumTypeFailure   (NumTypeFailure    e) = Left (TypeMismatch (getPos e) (Leaf Num)   (getRoot e)) == evalStateT (matchType Num    e) []
prop_BoolTypeFailure  (BoolTypeFailure   e) = Left (TypeMismatch (getPos e) (Leaf Bool)  (getRoot e)) == evalStateT (matchType Bool   e) []
prop_NullTypeFailure  (NullTypeFailure   e) = Left (TypeMismatch (getPos e) (Leaf Null)  (getRoot e)) == evalStateT (matchType Null   e) []

prop_TableTypeSuccess (TableOA  e) = Right e == evalStateT (matchType Table  e) []
prop_PlotTypeSuccess  (PlotOA   e) = Right e == evalStateT (matchType Plot   e) []
prop_ArrTypeSuccess   (ArrOA    e) = Right e == evalStateT (matchType arr    e) []
prop_ObjTypeSuccess   (ObjOA    e) = Right e == evalStateT (matchType obj    e) []
prop_StrTypeSuccess   (StrOA    e) = Right e == evalStateT (matchType Str    e) []
prop_NumTypeSuccess   (NumOA    e) = Right e == evalStateT (matchType Num    e) []
prop_BoolTypeSuccess  (BoolOA   e) = Right e == evalStateT (matchType Bool   e) []
prop_NullTypeSuccess  (NullOA   e) = Right e == evalStateT (matchType Null   e) []

