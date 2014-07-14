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
{-# ANN module "HLint: ignore Reduce duplication" #-}

test_NbArgs = do
  assertEqual (Left $ ArgCountMismatch (1,1) "f" 0 1) $ runFuncWith "f(0)"   $ nbArgEntry "f" 0
  assertEqual (Left $ ArgCountMismatch (1,1) "f" 0 2) $ runFuncWith "f(0,1)" $ nbArgEntry "f" 0
  assertEqual (Left $ ArgCountMismatch (1,1) "f" 1 0) $ runFuncWith "f()"    $ nbArgEntry "f" 1
  assertEqual (Left $ ArgCountMismatch (1,1) "f" 1 2) $ runFuncWith "f(0,1)" $ nbArgEntry "f" 1
  assertEqual (Left $ ArgCountMismatch (1,1) "f" 2 0) $ runFuncWith "f()"    $ nbArgEntry "f" 2
  assertEqual (Left $ ArgCountMismatch (1,1) "f" 2 1) $ runFuncWith "f(0)"   $ nbArgEntry "f" 2

test_TableType = do assertEqual (Left $ TypeMismatch (1,1) (Leaf Table) (Leaf Plot))  $ matchTypeParseWith Table "f()" $ nullary "f" $ PlotO  (1,1) [] []
                    assertEqual (Left $ TypeMismatch (1,1) (Leaf Table)  NodeArr)     $ matchTypeParse     Table "[]"
                    assertEqual (Left $ TypeMismatch (1,1) (Leaf Table)  NodeObj)     $ matchTypeParse     Table "{}"
                    assertEqual (Left $ TypeMismatch (1,1) (Leaf Table) (Leaf Str))   $ matchTypeParse     Table "\"\""
                    assertEqual (Left $ TypeMismatch (1,1) (Leaf Table) (Leaf Num))   $ matchTypeParse     Table "0"
                    assertEqual (Left $ TypeMismatch (1,1) (Leaf Table) (Leaf Bool))  $ matchTypeParse     Table "true"
                    assertEqual (Left $ TypeMismatch (1,1) (Leaf Table) (Leaf Null))  $ matchTypeParse     Table "null"
                    assertEqual (Right $ TableO (1,1) [] [])                          $ matchTypeParseWith Table "f()" $ nullary "f" $ TableO (1,1) [] []

test_PlotType = do  assertEqual (Left $ TypeMismatch (1,1) (Leaf Plot)  (Leaf Table)) $ matchTypeParseWith Plot "f()" $ nullary "f" $ TableO (1,1) [] []
                    assertEqual (Left $ TypeMismatch (1,1) (Leaf Plot)   NodeArr)     $ matchTypeParse     Plot "[]"
                    assertEqual (Left $ TypeMismatch (1,1) (Leaf Plot)   NodeObj)     $ matchTypeParse     Plot "{}"
                    assertEqual (Left $ TypeMismatch (1,1) (Leaf Plot)  (Leaf Str))   $ matchTypeParse     Plot "\"\""
                    assertEqual (Left $ TypeMismatch (1,1) (Leaf Plot)  (Leaf Num))   $ matchTypeParse     Plot "0"
                    assertEqual (Left $ TypeMismatch (1,1) (Leaf Plot)  (Leaf Bool))  $ matchTypeParse     Plot "true"
                    assertEqual (Left $ TypeMismatch (1,1) (Leaf Plot)  (Leaf Null))  $ matchTypeParse     Plot "null"
                    assertEqual (Right $ PlotO (1,1) [] [])                           $ matchTypeParseWith Plot "f()" $ nullary "f" $ PlotO  (1,1) [] []

test_ArrType = do   assertEqual (Left $ TypeMismatch (1,1)  NodeArr     (Leaf Table)) $ matchTypeParseWith arr "f()" $ nullary "f" $ TableO (1,1) [] []
                    assertEqual (Left $ TypeMismatch (1,1)  NodeArr     (Leaf Plot))  $ matchTypeParseWith arr "f()" $ nullary "f" $ PlotO  (1,1) [] []
                    assertEqual (Left $ TypeMismatch (1,1)  NodeArr      NodeObj)     $ matchTypeParse     arr "{}"
                    assertEqual (Left $ TypeMismatch (1,1)  NodeArr     (Leaf Str))   $ matchTypeParse     arr "\"\""
                    assertEqual (Left $ TypeMismatch (1,1)  NodeArr     (Leaf Num))   $ matchTypeParse     arr "0"
                    assertEqual (Left $ TypeMismatch (1,1)  NodeArr     (Leaf Bool))  $ matchTypeParse     arr "true"
                    assertEqual (Left $ TypeMismatch (1,1)  NodeArr     (Leaf Null))  $ matchTypeParse     arr "null"
                    assertEqual (Right $ ArrO (1,1) [])                               $ matchTypeParse     arr "[]"

                    assertEqual (Left $ TypeMismatch (1,2)  NodeArr     (Leaf Table)) $ matchTypeParseWith (ArrOf arr) "[f()]" $ nullary "f" $ TableO (1,2) [] []
                    assertEqual (Left $ TypeMismatch (1,2)  NodeArr     (Leaf Plot))  $ matchTypeParseWith (ArrOf arr) "[f()]" $ nullary "f" $ PlotO  (1,2) [] []
                    assertEqual (Left $ TypeMismatch (1,2)  NodeArr      NodeObj)     $ matchTypeParse     (ArrOf arr) "[{}]"
                    assertEqual (Left $ TypeMismatch (1,2)  NodeArr     (Leaf Str))   $ matchTypeParse     (ArrOf arr) "[\"\"]"
                    assertEqual (Left $ TypeMismatch (1,2)  NodeArr     (Leaf Num))   $ matchTypeParse     (ArrOf arr) "[0]"
                    assertEqual (Left $ TypeMismatch (1,2)  NodeArr     (Leaf Bool))  $ matchTypeParse     (ArrOf arr) "[true]"
                    assertEqual (Left $ TypeMismatch (1,2)  NodeArr     (Leaf Null))  $ matchTypeParse     (ArrOf arr) "[null]"
                    assertEqual (Right $ ArrO (1,1) [ArrO (1,2) []])                  $ matchTypeParse     (ArrOf arr) "[[]]"

                    assertEqual (Right $ ArrO (1,1) [TableO (1,2) [] []])             $ matchTypeParseWith arr "[f()]" $ nullary "f" $ TableO (1,2) [] []
                    assertEqual (Right $ ArrO (1,1) [PlotO (1,2) [] []])              $ matchTypeParseWith arr "[f()]" $ nullary "f" $ PlotO  (1,2) [] []
                    assertEqual (Right $ ArrO (1,1) [ArrO (1,2) []])                  $ matchTypeParse     arr "[[]]"
                    assertEqual (Right $ ArrO (1,1) [ObjO (1,2) []])                  $ matchTypeParse     arr "[{}]"
                    assertEqual (Right $ ArrO (1,1) [StrO (1,2) ""])                  $ matchTypeParse     arr "[\"\"]"
                    assertEqual (Right $ ArrO (1,1) [NumO (1,2) 0])                   $ matchTypeParse     arr "[0]"
                    assertEqual (Right $ ArrO (1,1) [BoolO (1,2) True])               $ matchTypeParse     arr "[true]"
                    assertEqual (Right $ ArrO (1,1) [NullO (1,2)])                    $ matchTypeParse     arr "[null]"

test_ObjType = do   assertEqual (Left $ TypeMismatch (1,1)  NodeObj     (Leaf Table)) $ matchTypeParseWith obj "f()" $ nullary "f" $ TableO (1,1) [] []
                    assertEqual (Left $ TypeMismatch (1,1)  NodeObj     (Leaf Plot))  $ matchTypeParseWith obj "f()" $ nullary "f" $ PlotO  (1,1) [] []
                    assertEqual (Left $ TypeMismatch (1,1)  NodeObj      NodeArr)     $ matchTypeParse     obj "[]"
                    assertEqual (Left $ TypeMismatch (1,1)  NodeObj     (Leaf Str))   $ matchTypeParse     obj "\"\""
                    assertEqual (Left $ TypeMismatch (1,1)  NodeObj     (Leaf Num))   $ matchTypeParse     obj "0"
                    assertEqual (Left $ TypeMismatch (1,1)  NodeObj     (Leaf Bool))  $ matchTypeParse     obj "true"
                    assertEqual (Left $ TypeMismatch (1,1)  NodeObj     (Leaf Null))  $ matchTypeParse     obj "null"
                    assertEqual (Right $ ObjO (1,1) [])                               $ matchTypeParse     obj "{}"

                    assertEqual (Left $ TypeMismatch (1,4)  NodeObj     (Leaf Table)) $ matchTypeParseWith (ObjOf obj) "{x:f()}" $ nullary "f" $ TableO (1,4) [] []
                    assertEqual (Left $ TypeMismatch (1,4)  NodeObj     (Leaf Plot))  $ matchTypeParseWith (ObjOf obj) "{x:f()}" $ nullary "f" $ PlotO  (1,4) [] []
                    assertEqual (Left $ TypeMismatch (1,4)  NodeObj      NodeArr)     $ matchTypeParse     (ObjOf obj) "{x:[]}"
                    assertEqual (Left $ TypeMismatch (1,4)  NodeObj     (Leaf Str))   $ matchTypeParse     (ObjOf obj) "{x:\"\"}"
                    assertEqual (Left $ TypeMismatch (1,4)  NodeObj     (Leaf Num))   $ matchTypeParse     (ObjOf obj) "{x:0}"
                    assertEqual (Left $ TypeMismatch (1,4)  NodeObj     (Leaf Bool))  $ matchTypeParse     (ObjOf obj) "{x:true}"
                    assertEqual (Left $ TypeMismatch (1,4)  NodeObj     (Leaf Null))  $ matchTypeParse     (ObjOf obj) "{x:null}"
                    assertEqual (Right $ ObjO (1,1) [("x",ObjO (1,4) [])])            $ matchTypeParse     (ObjOf obj) "{x:{}}"

                    assertEqual (Right $ ObjO (1,1) [("x",TableO (1,4) [] [])])       $ matchTypeParseWith obj "{x:f()}" $ nullary "f" $ TableO (1,4) [] []
                    assertEqual (Right $ ObjO (1,1) [("x",PlotO (1,4) [] [])])        $ matchTypeParseWith obj "{x:f()}" $ nullary "f" $ PlotO  (1,4) [] []
                    assertEqual (Right $ ObjO (1,1) [("x",ArrO (1,4) [])])            $ matchTypeParse     obj "{x:[]}"
                    assertEqual (Right $ ObjO (1,1) [("x",ObjO (1,4) [])])            $ matchTypeParse     obj "{x:{}}"
                    assertEqual (Right $ ObjO (1,1) [("x",StrO (1,4) "")])            $ matchTypeParse     obj "{x:\"\"}"
                    assertEqual (Right $ ObjO (1,1) [("x",NumO (1,4) 0)])             $ matchTypeParse     obj "{x:0}"
                    assertEqual (Right $ ObjO (1,1) [("x",BoolO (1,4) True)])         $ matchTypeParse     obj "{x:true}"
                    assertEqual (Right $ ObjO (1,1) [("x",NullO (1,4))])              $ matchTypeParse     obj "{x:null}"

test_StrType = do   assertEqual (Left $ TypeMismatch (1,1) (Leaf Str)   (Leaf Table)) $ matchTypeParseWith Str "f()" $ nullary "f" $ TableO (1,1) [] []
                    assertEqual (Left $ TypeMismatch (1,1) (Leaf Str)   (Leaf Plot))  $ matchTypeParseWith Str "f()" $ nullary "f" $ PlotO  (1,1) [] []
                    assertEqual (Left $ TypeMismatch (1,1) (Leaf Str)    NodeArr)     $ matchTypeParse     Str "[]"
                    assertEqual (Left $ TypeMismatch (1,1) (Leaf Str)    NodeObj)     $ matchTypeParse     Str "{}"
                    assertEqual (Left $ TypeMismatch (1,1) (Leaf Str)   (Leaf Num))   $ matchTypeParse     Str "0"
                    assertEqual (Left $ TypeMismatch (1,1) (Leaf Str)   (Leaf Bool))  $ matchTypeParse     Str "true"
                    assertEqual (Left $ TypeMismatch (1,1) (Leaf Str)   (Leaf Null))  $ matchTypeParse     Str "null"
                    assertEqual (Right $ StrO (1,1) "")                               $ matchTypeParse     Str "\"\""

test_NumType = do   assertEqual (Left $ TypeMismatch (1,1) (Leaf Num)   (Leaf Table)) $ matchTypeParseWith Num "f()" $ nullary "f" $ TableO (1,1) [] []
                    assertEqual (Left $ TypeMismatch (1,1) (Leaf Num)   (Leaf Plot))  $ matchTypeParseWith Num "f()" $ nullary "f" $ PlotO  (1,1) [] []
                    assertEqual (Left $ TypeMismatch (1,1) (Leaf Num)    NodeArr)     $ matchTypeParse     Num "[]"
                    assertEqual (Left $ TypeMismatch (1,1) (Leaf Num)    NodeObj)     $ matchTypeParse     Num "{}"
                    assertEqual (Left $ TypeMismatch (1,1) (Leaf Num)   (Leaf Str))   $ matchTypeParse     Num "\"\""
                    assertEqual (Left $ TypeMismatch (1,1) (Leaf Num)   (Leaf Bool))  $ matchTypeParse     Num "true"
                    assertEqual (Left $ TypeMismatch (1,1) (Leaf Num)   (Leaf Null))  $ matchTypeParse     Num "null"
                    assertEqual (Right $ NumO (1,1) 0)                                $ matchTypeParse     Num "0"

test_BoolType = do  assertEqual (Left $ TypeMismatch (1,1) (Leaf Bool)  (Leaf Table)) $ matchTypeParseWith Bool "f()" $ nullary "f" $ TableO (1,1) [] []
                    assertEqual (Left $ TypeMismatch (1,1) (Leaf Bool)  (Leaf Plot))  $ matchTypeParseWith Bool "f()" $ nullary "f" $ PlotO  (1,1) [] []
                    assertEqual (Left $ TypeMismatch (1,1) (Leaf Bool)   NodeArr)     $ matchTypeParse     Bool "[]"
                    assertEqual (Left $ TypeMismatch (1,1) (Leaf Bool)   NodeObj)     $ matchTypeParse     Bool "{}"
                    assertEqual (Left $ TypeMismatch (1,1) (Leaf Bool)  (Leaf Str))   $ matchTypeParse     Bool "\"\""
                    assertEqual (Left $ TypeMismatch (1,1) (Leaf Bool)  (Leaf Num))   $ matchTypeParse     Bool "0"
                    assertEqual (Left $ TypeMismatch (1,1) (Leaf Bool)  (Leaf Null))  $ matchTypeParse     Bool "null"
                    assertEqual (Right $ BoolO (1,1) True)                            $ matchTypeParse     Bool "true"

test_NullType = do  assertEqual (Left $ TypeMismatch (1,1) (Leaf Null)  (Leaf Table)) $ matchTypeParseWith Null "f()" $ nullary "f" $ TableO (1,1) [] []
                    assertEqual (Left $ TypeMismatch (1,1) (Leaf Null)  (Leaf Plot))  $ matchTypeParseWith Null "f()" $ nullary "f" $ PlotO  (1,1) [] []
                    assertEqual (Left $ TypeMismatch (1,1) (Leaf Null)   NodeArr)     $ matchTypeParse     Null "[]"
                    assertEqual (Left $ TypeMismatch (1,1) (Leaf Null)   NodeObj)     $ matchTypeParse     Null "{}"
                    assertEqual (Left $ TypeMismatch (1,1) (Leaf Null)  (Leaf Str))   $ matchTypeParse     Null "\"\""
                    assertEqual (Left $ TypeMismatch (1,1) (Leaf Null)  (Leaf Num))   $ matchTypeParse     Null "0"
                    assertEqual (Left $ TypeMismatch (1,1) (Leaf Null)  (Leaf Bool))  $ matchTypeParse     Null "true"
                    assertEqual (Right $ NullO (1,1))                                 $ matchTypeParse     Null "null"

test_AnyType = do   assertEqual (Right $ TableO (1,1) [] [])                    $ matchTypeParseWith any "f()" $ nullary "f" $ TableO (1,1) [] []
                    assertEqual (Right $ PlotO (1,1) [] [])                     $ matchTypeParseWith any "f()" $ nullary "f" $ PlotO  (1,1) [] []
                    assertEqual (Right $ ArrO (1,1) [])                         $ matchTypeParse     any "[]"
                    assertEqual (Right $ ObjO (1,1) [])                         $ matchTypeParse     any "{}"
                    assertEqual (Right $ StrO (1,1) "")                         $ matchTypeParse     any "\"\""
                    assertEqual (Right $ NumO (1,1) 0)                          $ matchTypeParse     any "0"
                    assertEqual (Right $ BoolO (1,1) True)                      $ matchTypeParse     any "true"
                    assertEqual (Right $ NullO (1,1))                           $ matchTypeParse     any "null"

                    assertEqual (Right $ ArrO (1,1) [TableO (1,2) [] []])       $ matchTypeParseWith any "[f()]" $ nullary "f" $ TableO (1,2) [] []
                    assertEqual (Right $ ArrO (1,1) [PlotO (1,2) [] []])        $ matchTypeParseWith any "[f()]" $ nullary "f" $ PlotO  (1,2) [] []
                    assertEqual (Right $ ArrO (1,1) [ArrO (1,2) []])            $ matchTypeParse     any "[[]]"
                    assertEqual (Right $ ArrO (1,1) [ObjO (1,2) []])            $ matchTypeParse     any "[{}]"
                    assertEqual (Right $ ArrO (1,1) [StrO (1,2) ""])            $ matchTypeParse     any "[\"\"]"
                    assertEqual (Right $ ArrO (1,1) [NumO (1,2) 0])             $ matchTypeParse     any "[0]"
                    assertEqual (Right $ ArrO (1,1) [BoolO (1,2) True])         $ matchTypeParse     any "[true]"
                    assertEqual (Right $ ArrO (1,1) [NullO (1,2)])              $ matchTypeParse     any "[null]"

                    assertEqual (Right $ ObjO (1,1) [("x",TableO (1,4) [] [])]) $ matchTypeParseWith any "{x:f()}" $ nullary "f" $ TableO (1,4) [] []
                    assertEqual (Right $ ObjO (1,1) [("x",PlotO (1,4) [] [])])  $ matchTypeParseWith any "{x:f()}" $ nullary "f" $ PlotO  (1,4) [] []
                    assertEqual (Right $ ObjO (1,1) [("x",ArrO (1,4) [])])      $ matchTypeParse     any "{x:[]}"
                    assertEqual (Right $ ObjO (1,1) [("x",ObjO (1,4) [])])      $ matchTypeParse     any "{x:{}}"
                    assertEqual (Right $ ObjO (1,1) [("x",StrO (1,4) "")])      $ matchTypeParse     any "{x:\"\"}"
                    assertEqual (Right $ ObjO (1,1) [("x",NumO (1,4) 0)])       $ matchTypeParse     any "{x:0}"
                    assertEqual (Right $ ObjO (1,1) [("x",BoolO (1,4) True)])   $ matchTypeParse     any "{x:true}"
                    assertEqual (Right $ ObjO (1,1) [("x",NullO (1,4))])        $ matchTypeParse     any "{x:null}"

test_NoneType = do  assertEqual (Left $ TypeMismatch (1,1) (getRoot none) (Leaf Table)) $ matchTypeParseWith none "f()" $ nullary "f" $ TableO (1,1) [] []
                    assertEqual (Left $ TypeMismatch (1,1) (getRoot none) (Leaf Plot))  $ matchTypeParseWith none "f()" $ nullary "f" $ PlotO  (1,1) [] []
                    assertEqual (Left $ TypeMismatch (1,1) (getRoot none)  NodeArr)     $ matchTypeParse     none "[]"
                    assertEqual (Left $ TypeMismatch (1,1) (getRoot none)  NodeObj)     $ matchTypeParse     none "{}"
                    assertEqual (Left $ TypeMismatch (1,1) (getRoot none) (Leaf Str))   $ matchTypeParse     none "\"\""
                    assertEqual (Left $ TypeMismatch (1,1) (getRoot none) (Leaf Num))   $ matchTypeParse     none "0"
                    assertEqual (Left $ TypeMismatch (1,1) (getRoot none) (Leaf Bool))  $ matchTypeParse     none "true"
                    assertEqual (Left $ TypeMismatch (1,1) (getRoot none) (Leaf Null))  $ matchTypeParse     none "null"
--
                    assertEqual (Left $ TypeMismatch (1,2) (getRoot none) (Leaf Table)) $ matchTypeParseWith (ArrOf none) "[f()]" $ nullary "f" $ TableO (1,2) [] []
                    assertEqual (Left $ TypeMismatch (1,2) (getRoot none) (Leaf Plot))  $ matchTypeParseWith (ArrOf none) "[f()]" $ nullary "f" $ PlotO  (1,2) [] []
                    assertEqual (Left $ TypeMismatch (1,2) (getRoot none)  NodeArr)     $ matchTypeParse     (ArrOf none) "[[]]"
                    assertEqual (Left $ TypeMismatch (1,2) (getRoot none)  NodeObj)     $ matchTypeParse     (ArrOf none) "[{}]"
                    assertEqual (Left $ TypeMismatch (1,2) (getRoot none) (Leaf Str))   $ matchTypeParse     (ArrOf none) "[\"\"]"
                    assertEqual (Left $ TypeMismatch (1,2) (getRoot none) (Leaf Num))   $ matchTypeParse     (ArrOf none) "[0]"
                    assertEqual (Left $ TypeMismatch (1,2) (getRoot none) (Leaf Bool))  $ matchTypeParse     (ArrOf none) "[true]"
                    assertEqual (Left $ TypeMismatch (1,2) (getRoot none) (Leaf Null))  $ matchTypeParse     (ArrOf none) "[null]"

                    assertEqual (Left $ TypeMismatch (1,1) (getRoot none)  NodeArr)     $ matchTypeParseWith none "[f()]" $ nullary "f" $ TableO (1,2) [] []
                    assertEqual (Left $ TypeMismatch (1,1) (getRoot none)  NodeArr)     $ matchTypeParseWith none "[f()]" $ nullary "f" $ PlotO  (1,2) [] []
                    assertEqual (Left $ TypeMismatch (1,1) (getRoot none)  NodeArr)     $ matchTypeParse     none "[[]]"
                    assertEqual (Left $ TypeMismatch (1,1) (getRoot none)  NodeArr)     $ matchTypeParse     none "[{}]"
                    assertEqual (Left $ TypeMismatch (1,1) (getRoot none)  NodeArr)     $ matchTypeParse     none "[\"\"]"
                    assertEqual (Left $ TypeMismatch (1,1) (getRoot none)  NodeArr)     $ matchTypeParse     none "[0]"
                    assertEqual (Left $ TypeMismatch (1,1) (getRoot none)  NodeArr)     $ matchTypeParse     none "[true]"
                    assertEqual (Left $ TypeMismatch (1,1) (getRoot none)  NodeArr)     $ matchTypeParse     none "[null]"

                    assertEqual (Left $ TypeMismatch (1,4) (getRoot none) (Leaf Table)) $ matchTypeParseWith (ObjOf none) "{x:f()}" $ nullary "f" $ TableO (1,4) [] []
                    assertEqual (Left $ TypeMismatch (1,4) (getRoot none) (Leaf Plot))  $ matchTypeParseWith (ObjOf none) "{x:f()}" $ nullary "f" $ PlotO  (1,4) [] []
                    assertEqual (Left $ TypeMismatch (1,4) (getRoot none)  NodeArr)     $ matchTypeParse     (ObjOf none) "{x:[]}"
                    assertEqual (Left $ TypeMismatch (1,4) (getRoot none)  NodeObj)     $ matchTypeParse     (ObjOf none) "{x:{}}"
                    assertEqual (Left $ TypeMismatch (1,4) (getRoot none) (Leaf Str))   $ matchTypeParse     (ObjOf none) "{x:\"\"}"
                    assertEqual (Left $ TypeMismatch (1,4) (getRoot none) (Leaf Num))   $ matchTypeParse     (ObjOf none) "{x:0}"
                    assertEqual (Left $ TypeMismatch (1,4) (getRoot none) (Leaf Bool))  $ matchTypeParse     (ObjOf none) "{x:true}"
                    assertEqual (Left $ TypeMismatch (1,4) (getRoot none) (Leaf Null))  $ matchTypeParse     (ObjOf none) "{x:null}"

                    assertEqual (Left $ TypeMismatch (1,1) (getRoot none)  NodeObj)     $ matchTypeParseWith none "{x:f()}" $ nullary "f" $ TableO (1,4) [] []
                    assertEqual (Left $ TypeMismatch (1,1) (getRoot none)  NodeObj)     $ matchTypeParseWith none "{x:f()}" $ nullary "f" $ PlotO  (1,4) [] []
                    assertEqual (Left $ TypeMismatch (1,1) (getRoot none)  NodeObj)     $ matchTypeParse     none "{x:[]}"
                    assertEqual (Left $ TypeMismatch (1,1) (getRoot none)  NodeObj)     $ matchTypeParse     none "{x:{}}"
                    assertEqual (Left $ TypeMismatch (1,1) (getRoot none)  NodeObj)     $ matchTypeParse     none "{x:\"\"}"
                    assertEqual (Left $ TypeMismatch (1,1) (getRoot none)  NodeObj)     $ matchTypeParse     none "{x:0}"
                    assertEqual (Left $ TypeMismatch (1,1) (getRoot none)  NodeObj)     $ matchTypeParse     none "{x:true}"
                    assertEqual (Left $ TypeMismatch (1,1) (getRoot none)  NodeObj)     $ matchTypeParse     none "{x:null}"



                    