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

prop_TableTypeFailure (TableTypeFailure  e) = Left (TypeMismatch (getPos e) (Leaf Table) (getRoot e)) == simpleMatch e Table
prop_PlotTypeFailure  (PlotTypeFailure   e) = Left (TypeMismatch (getPos e) (Leaf Plot)  (getRoot e)) == simpleMatch e Plot 
prop_ArrTypeFailure   (ArrTypeFailure    e) = Left (TypeMismatch (getPos e)  NodeArr     (getRoot e)) == simpleMatch e arr  
prop_ObjTypeFailure   (ObjTypeFailure    e) = Left (TypeMismatch (getPos e)  NodeObj     (getRoot e)) == simpleMatch e obj  
prop_StrTypeFailure   (StrTypeFailure    e) = Left (TypeMismatch (getPos e) (Leaf Str)   (getRoot e)) == simpleMatch e Str  
prop_NumTypeFailure   (NumTypeFailure    e) = Left (TypeMismatch (getPos e) (Leaf Num)   (getRoot e)) == simpleMatch e Num  
prop_BoolTypeFailure  (BoolTypeFailure   e) = Left (TypeMismatch (getPos e) (Leaf Bool)  (getRoot e)) == simpleMatch e Bool 
prop_NullTypeFailure  (NullTypeFailure   e) = Left (TypeMismatch (getPos e) (Leaf Null)  (getRoot e)) == simpleMatch e Null 

prop_TableTypeSuccess (TableOA  e) = Right e == simpleMatch e Table
prop_PlotTypeSuccess  (PlotOA   e) = Right e == simpleMatch e Plot 
prop_ArrTypeSuccess   (ArrOA    e) = Right e == simpleMatch e arr  
prop_ObjTypeSuccess   (ObjOA    e) = Right e == simpleMatch e obj  
prop_StrTypeSuccess   (StrOA    e) = Right e == simpleMatch e Str  
prop_NumTypeSuccess   (NumOA    e) = Right e == simpleMatch e Num  
prop_BoolTypeSuccess  (BoolOA   e) = Right e == simpleMatch e Bool 
prop_NullTypeSuccess  (NullOA   e) = Right e == simpleMatch e Null 

prop_AnySuccess  (ExpOA e) = Right e                                                   == simpleMatch e any
prop_NoneFailure (ExpOA e) = Left (TypeMismatch (getPos e) (getRoot none) (getRoot e)) == simpleMatch e none
