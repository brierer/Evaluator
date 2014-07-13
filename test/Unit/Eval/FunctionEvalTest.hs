{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Unit.Eval.FunctionEvalTest where

import Prelude hiding (any)

import Data.EvalError
import Data.ExpObj
import Eval.Function
import Test.Framework

import Common.Eval.FunctionEvalUtils
import Unit.Eval.FunctionEvalUtils

{-# ANN module "HLint: ignore Use camelCase" #-}

test_NbArgs = do
  assertEqual (Left $ ArgCountMismatch (1,1) "f" 0 1) $ runFuncWith "f(0)"   $ nbArgEntry "f" 0
  assertEqual (Left $ ArgCountMismatch (1,1) "f" 0 2) $ runFuncWith "f(0,1)" $ nbArgEntry "f" 0
  assertEqual (Left $ ArgCountMismatch (1,1) "f" 1 0) $ runFuncWith "f()"    $ nbArgEntry "f" 1
  assertEqual (Left $ ArgCountMismatch (1,1) "f" 1 2) $ runFuncWith "f(0,1)" $ nbArgEntry "f" 1
  assertEqual (Left $ ArgCountMismatch (1,1) "f" 2 0) $ runFuncWith "f()"    $ nbArgEntry "f" 2
  assertEqual (Left $ ArgCountMismatch (1,1) "f" 2 1) $ runFuncWith "f(0)"   $ nbArgEntry "f" 2

test_TableTypeError = do assertEqual (Left $ TypeMismatch (1,1) (Leaf Table) (Leaf Plot))  $ matchTypeParseWith Table "f()" $ nullary "f" $ PlotO  (1,1) [] []
                         assertEqual (Left $ TypeMismatch (1,1) (Leaf Table)  NodeArr)     $ matchTypeParse     Table "[]"
                         assertEqual (Left $ TypeMismatch (1,1) (Leaf Table)  NodeObj)     $ matchTypeParse     Table "{}"
                         assertEqual (Left $ TypeMismatch (1,1) (Leaf Table) (Leaf Str))   $ matchTypeParse     Table "\"\""
                         assertEqual (Left $ TypeMismatch (1,1) (Leaf Table) (Leaf Num))   $ matchTypeParse     Table "0"
                         assertEqual (Left $ TypeMismatch (1,1) (Leaf Table) (Leaf Bool))  $ matchTypeParse     Table "true"
                         assertEqual (Left $ TypeMismatch (1,1) (Leaf Table) (Leaf Null))  $ matchTypeParse     Table "null"
                                                                                        
test_PlotTypeError = do  assertEqual (Left $ TypeMismatch (1,1) (Leaf Plot)  (Leaf Table)) $ matchTypeParseWith Plot "f()" $ nullary "f" $ TableO (1,1) [] []
                         assertEqual (Left $ TypeMismatch (1,1) (Leaf Plot)   NodeArr)     $ matchTypeParse     Plot "[]"
                         assertEqual (Left $ TypeMismatch (1,1) (Leaf Plot)   NodeObj)     $ matchTypeParse     Plot "{}"
                         assertEqual (Left $ TypeMismatch (1,1) (Leaf Plot)  (Leaf Str))   $ matchTypeParse     Plot "\"\""
                         assertEqual (Left $ TypeMismatch (1,1) (Leaf Plot)  (Leaf Num))   $ matchTypeParse     Plot "0"
                         assertEqual (Left $ TypeMismatch (1,1) (Leaf Plot)  (Leaf Bool))  $ matchTypeParse     Plot "true"
                         assertEqual (Left $ TypeMismatch (1,1) (Leaf Plot)  (Leaf Null))  $ matchTypeParse     Plot "null"
                                                                                              
test_ArrTypeError = do   assertEqual (Left $ TypeMismatch (1,1)  NodeArr     (Leaf Table)) $ matchTypeParseWith arr "f()" $ nullary "f" $ TableO (1,1) [] []
                         assertEqual (Left $ TypeMismatch (1,1)  NodeArr     (Leaf Plot))  $ matchTypeParseWith arr "f()" $ nullary "f" $ PlotO  (1,1) [] []
                         assertEqual (Left $ TypeMismatch (1,1)  NodeArr      NodeObj)     $ matchTypeParse     arr "{}"
                         assertEqual (Left $ TypeMismatch (1,1)  NodeArr     (Leaf Str))   $ matchTypeParse     arr "\"\""
                         assertEqual (Left $ TypeMismatch (1,1)  NodeArr     (Leaf Num))   $ matchTypeParse     arr "0"
                         assertEqual (Left $ TypeMismatch (1,1)  NodeArr     (Leaf Bool))  $ matchTypeParse     arr "true"
                         assertEqual (Left $ TypeMismatch (1,1)  NodeArr     (Leaf Null))  $ matchTypeParse     arr "null"
                                                                                           
                         assertEqual (Left $ TypeMismatch (1,1)  NodeArr     (Leaf Table)) $ matchTypeParseWith (ArrOf arr) "[f()]" $ nullary "f" $ TableO (1,1) [] []
                         assertEqual (Left $ TypeMismatch (1,1)  NodeArr     (Leaf Plot))  $ matchTypeParseWith (ArrOf arr) "[f()]" $ nullary "f" $ PlotO  (1,1) [] []
                         assertEqual (Left $ TypeMismatch (1,2)  NodeArr      NodeObj)     $ matchTypeParse     (ArrOf arr) "[{}]"
                         assertEqual (Left $ TypeMismatch (1,2)  NodeArr     (Leaf Str))   $ matchTypeParse     (ArrOf arr) "[\"\"]"
                         assertEqual (Left $ TypeMismatch (1,2)  NodeArr     (Leaf Num))   $ matchTypeParse     (ArrOf arr) "[0]"
                         assertEqual (Left $ TypeMismatch (1,2)  NodeArr     (Leaf Bool))  $ matchTypeParse     (ArrOf arr) "[true]"
                         assertEqual (Left $ TypeMismatch (1,2)  NodeArr     (Leaf Null))  $ matchTypeParse     (ArrOf arr) "[null]"
                                                                                           
test_ObjTypeError = do   assertEqual (Left $ TypeMismatch (1,1)  NodeObj     (Leaf Table)) $ matchTypeParseWith obj "f()" $ nullary "f" $ TableO (1,1) [] []
                         assertEqual (Left $ TypeMismatch (1,1)  NodeObj     (Leaf Plot))  $ matchTypeParseWith obj "f()" $ nullary "f" $ PlotO  (1,1) [] []
                         assertEqual (Left $ TypeMismatch (1,1)  NodeObj      NodeArr)     $ matchTypeParse     obj "[]"
                         assertEqual (Left $ TypeMismatch (1,1)  NodeObj     (Leaf Str))   $ matchTypeParse     obj "\"\""
                         assertEqual (Left $ TypeMismatch (1,1)  NodeObj     (Leaf Num))   $ matchTypeParse     obj "0"
                         assertEqual (Left $ TypeMismatch (1,1)  NodeObj     (Leaf Bool))  $ matchTypeParse     obj "true"
                         assertEqual (Left $ TypeMismatch (1,1)  NodeObj     (Leaf Null))  $ matchTypeParse     obj "null"
                                                                                           
                         assertEqual (Left $ TypeMismatch (1,1)  NodeObj     (Leaf Table)) $ matchTypeParseWith (ObjOf obj) "{x:f()}" $ nullary "f" $ TableO (1,1) [] []
                         assertEqual (Left $ TypeMismatch (1,1)  NodeObj     (Leaf Plot))  $ matchTypeParseWith (ObjOf obj) "{x:f()}" $ nullary "f" $ PlotO  (1,1) [] []
                         assertEqual (Left $ TypeMismatch (1,4)  NodeObj      NodeArr)     $ matchTypeParse     (ObjOf obj) "{x:[]}"
                         assertEqual (Left $ TypeMismatch (1,4)  NodeObj     (Leaf Str))   $ matchTypeParse     (ObjOf obj) "{x:\"\"}"
                         assertEqual (Left $ TypeMismatch (1,4)  NodeObj     (Leaf Num))   $ matchTypeParse     (ObjOf obj) "{x:0}"
                         assertEqual (Left $ TypeMismatch (1,4)  NodeObj     (Leaf Bool))  $ matchTypeParse     (ObjOf obj) "{x:true}"
                         assertEqual (Left $ TypeMismatch (1,4)  NodeObj     (Leaf Null))  $ matchTypeParse     (ObjOf obj) "{x:null}"
                                                                                           
test_StrTypeError = do   assertEqual (Left $ TypeMismatch (1,1) (Leaf Str)   (Leaf Table)) $ matchTypeParseWith Str "f()" $ nullary "f" $ TableO (1,1) [] []
                         assertEqual (Left $ TypeMismatch (1,1) (Leaf Str)   (Leaf Plot))  $ matchTypeParseWith Str "f()" $ nullary "f" $ PlotO  (1,1) [] []
                         assertEqual (Left $ TypeMismatch (1,1) (Leaf Str)    NodeArr)     $ matchTypeParse     Str "[]"
                         assertEqual (Left $ TypeMismatch (1,1) (Leaf Str)    NodeObj)     $ matchTypeParse     Str "{}"
                         assertEqual (Left $ TypeMismatch (1,1) (Leaf Str)   (Leaf Num))   $ matchTypeParse     Str "0"   
                         assertEqual (Left $ TypeMismatch (1,1) (Leaf Str)   (Leaf Bool))  $ matchTypeParse     Str "true"
                         assertEqual (Left $ TypeMismatch (1,1) (Leaf Str)   (Leaf Null))  $ matchTypeParse     Str "null"
                                                                                           
test_NumTypeError = do   assertEqual (Left $ TypeMismatch (1,1) (Leaf Num)   (Leaf Table)) $ matchTypeParseWith Num "f()" $ nullary "f" $ TableO (1,1) [] []
                         assertEqual (Left $ TypeMismatch (1,1) (Leaf Num)   (Leaf Plot))  $ matchTypeParseWith Num "f()" $ nullary "f" $ PlotO  (1,1) [] []
                         assertEqual (Left $ TypeMismatch (1,1) (Leaf Num)    NodeArr)     $ matchTypeParse     Num "[]"
                         assertEqual (Left $ TypeMismatch (1,1) (Leaf Num)    NodeObj)     $ matchTypeParse     Num "{}"
                         assertEqual (Left $ TypeMismatch (1,1) (Leaf Num)   (Leaf Str))   $ matchTypeParse     Num "\"\""      
                         assertEqual (Left $ TypeMismatch (1,1) (Leaf Num)   (Leaf Bool))  $ matchTypeParse     Num "true"   
                         assertEqual (Left $ TypeMismatch (1,1) (Leaf Num)   (Leaf Null))  $ matchTypeParse     Num "null"   
                                                                                           
test_BoolTypeError = do  assertEqual (Left $ TypeMismatch (1,1) (Leaf Bool)  (Leaf Table)) $ matchTypeParseWith Bool "f()" $ nullary "f" $ TableO (1,1) [] []
                         assertEqual (Left $ TypeMismatch (1,1) (Leaf Bool)  (Leaf Plot))  $ matchTypeParseWith Bool "f()" $ nullary "f" $ PlotO  (1,1) [] []
                         assertEqual (Left $ TypeMismatch (1,1) (Leaf Bool)   NodeArr)     $ matchTypeParse     Bool "[]"
                         assertEqual (Left $ TypeMismatch (1,1) (Leaf Bool)   NodeObj)     $ matchTypeParse     Bool "{}"      
                         assertEqual (Left $ TypeMismatch (1,1) (Leaf Bool)  (Leaf Str))   $ matchTypeParse     Bool "\"\""
                         assertEqual (Left $ TypeMismatch (1,1) (Leaf Bool)  (Leaf Num))   $ matchTypeParse     Bool "0"   
                         assertEqual (Left $ TypeMismatch (1,1) (Leaf Bool)  (Leaf Null))  $ matchTypeParse     Bool "null"   
                                                                                           
test_NullTypeError = do  assertEqual (Left $ TypeMismatch (1,1) (Leaf Null)  (Leaf Table)) $ matchTypeParseWith Null "f()" $ nullary "f" $ TableO (1,1) [] []
                         assertEqual (Left $ TypeMismatch (1,1) (Leaf Null)  (Leaf Plot))  $ matchTypeParseWith Null "f()" $ nullary "f" $ PlotO  (1,1) [] []
                         assertEqual (Left $ TypeMismatch (1,1) (Leaf Null)   NodeArr)     $ matchTypeParse     Null "[]"
                         assertEqual (Left $ TypeMismatch (1,1) (Leaf Null)   NodeObj)     $ matchTypeParse     Null "{}"
                         assertEqual (Left $ TypeMismatch (1,1) (Leaf Null)  (Leaf Str))   $ matchTypeParse     Null "\"\""
                         assertEqual (Left $ TypeMismatch (1,1) (Leaf Null)  (Leaf Num))   $ matchTypeParse     Null "0"
                         assertEqual (Left $ TypeMismatch (1,1) (Leaf Null)  (Leaf Bool))  $ matchTypeParse     Null "true"

