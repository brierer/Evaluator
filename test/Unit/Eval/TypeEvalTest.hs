{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Unit.Eval.TypeEvalTest where

import Prelude hiding (any)

import Data.EvalError
import Data.ExpObj
import Eval.Function
import Test.Framework

import Unit.Eval.TypeEvalUtils

{-# ANN module "HLint: ignore Use camelCase" #-}
{-# ANN module "HLint: ignore Reduce duplication" #-}

test_TableType = do  assertEqual (Left $ TypeMismatch (1,1) (Leaf Table) (Leaf Plot))  $ matchTypeParseWith Table "f()" $ nullary "f" $ PlotO  (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1) (Leaf Table)  NodeArr)     $ matchTypeParse     Table "[]"
                     assertEqual (Left $ TypeMismatch (1,1) (Leaf Table)  NodeObj)     $ matchTypeParse     Table "{}"
                     assertEqual (Left $ TypeMismatch (1,1) (Leaf Table) (Leaf Str))   $ matchTypeParse     Table "\"\""
                     assertEqual (Left $ TypeMismatch (1,1) (Leaf Table) (Leaf Num))   $ matchTypeParse     Table "0"
                     assertEqual (Left $ TypeMismatch (1,1) (Leaf Table) (Leaf Bool))  $ matchTypeParse     Table "true"
                     assertEqual (Left $ TypeMismatch (1,1) (Leaf Table) (Leaf Null))  $ matchTypeParse     Table "null"
                     assertEqual (Right $ TableO (1,1) [] [])                          $ matchTypeParseWith Table "f()" $ nullary "f" $ TableO (1,1) [] []

test_PlotType = do   assertEqual (Left $ TypeMismatch (1,1) (Leaf Plot)  (Leaf Table)) $ matchTypeParseWith Plot "f()" $ nullary "f" $ TableO (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1) (Leaf Plot)   NodeArr)     $ matchTypeParse     Plot "[]"
                     assertEqual (Left $ TypeMismatch (1,1) (Leaf Plot)   NodeObj)     $ matchTypeParse     Plot "{}"
                     assertEqual (Left $ TypeMismatch (1,1) (Leaf Plot)  (Leaf Str))   $ matchTypeParse     Plot "\"\""
                     assertEqual (Left $ TypeMismatch (1,1) (Leaf Plot)  (Leaf Num))   $ matchTypeParse     Plot "0"
                     assertEqual (Left $ TypeMismatch (1,1) (Leaf Plot)  (Leaf Bool))  $ matchTypeParse     Plot "true"
                     assertEqual (Left $ TypeMismatch (1,1) (Leaf Plot)  (Leaf Null))  $ matchTypeParse     Plot "null"
                     assertEqual (Right $ PlotO (1,1) [] [])                           $ matchTypeParseWith Plot "f()" $ nullary "f" $ PlotO  (1,1) [] []

test_ArrType = do    assertEqual (Left $ TypeMismatch (1,1)  NodeArr     (Leaf Table)) $ matchTypeParseWith arr "f()" $ nullary "f" $ TableO (1,1) [] []
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

test_ObjType = do    assertEqual (Left $ TypeMismatch (1,1)  NodeObj     (Leaf Table)) $ matchTypeParseWith obj "f()" $ nullary "f" $ TableO (1,1) [] []
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

test_StrType = do    assertEqual (Left $ TypeMismatch (1,1) (Leaf Str)   (Leaf Table)) $ matchTypeParseWith Str "f()" $ nullary "f" $ TableO (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1) (Leaf Str)   (Leaf Plot))  $ matchTypeParseWith Str "f()" $ nullary "f" $ PlotO  (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1) (Leaf Str)    NodeArr)     $ matchTypeParse     Str "[]"
                     assertEqual (Left $ TypeMismatch (1,1) (Leaf Str)    NodeObj)     $ matchTypeParse     Str "{}"
                     assertEqual (Left $ TypeMismatch (1,1) (Leaf Str)   (Leaf Num))   $ matchTypeParse     Str "0"
                     assertEqual (Left $ TypeMismatch (1,1) (Leaf Str)   (Leaf Bool))  $ matchTypeParse     Str "true"
                     assertEqual (Left $ TypeMismatch (1,1) (Leaf Str)   (Leaf Null))  $ matchTypeParse     Str "null"
                     assertEqual (Right $ StrO (1,1) "")                               $ matchTypeParse     Str "\"\""

test_NumType = do    assertEqual (Left $ TypeMismatch (1,1) (Leaf Num)   (Leaf Table)) $ matchTypeParseWith Num "f()" $ nullary "f" $ TableO (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1) (Leaf Num)   (Leaf Plot))  $ matchTypeParseWith Num "f()" $ nullary "f" $ PlotO  (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1) (Leaf Num)    NodeArr)     $ matchTypeParse     Num "[]"
                     assertEqual (Left $ TypeMismatch (1,1) (Leaf Num)    NodeObj)     $ matchTypeParse     Num "{}"
                     assertEqual (Left $ TypeMismatch (1,1) (Leaf Num)   (Leaf Str))   $ matchTypeParse     Num "\"\""
                     assertEqual (Left $ TypeMismatch (1,1) (Leaf Num)   (Leaf Bool))  $ matchTypeParse     Num "true"
                     assertEqual (Left $ TypeMismatch (1,1) (Leaf Num)   (Leaf Null))  $ matchTypeParse     Num "null"
                     assertEqual (Right $ NumO (1,1) 0)                                $ matchTypeParse     Num "0"

test_BoolType = do   assertEqual (Left $ TypeMismatch (1,1) (Leaf Bool)  (Leaf Table)) $ matchTypeParseWith Bool "f()" $ nullary "f" $ TableO (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1) (Leaf Bool)  (Leaf Plot))  $ matchTypeParseWith Bool "f()" $ nullary "f" $ PlotO  (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1) (Leaf Bool)   NodeArr)     $ matchTypeParse     Bool "[]"
                     assertEqual (Left $ TypeMismatch (1,1) (Leaf Bool)   NodeObj)     $ matchTypeParse     Bool "{}"
                     assertEqual (Left $ TypeMismatch (1,1) (Leaf Bool)  (Leaf Str))   $ matchTypeParse     Bool "\"\""
                     assertEqual (Left $ TypeMismatch (1,1) (Leaf Bool)  (Leaf Num))   $ matchTypeParse     Bool "0"
                     assertEqual (Left $ TypeMismatch (1,1) (Leaf Bool)  (Leaf Null))  $ matchTypeParse     Bool "null"
                     assertEqual (Right $ BoolO (1,1) True)                            $ matchTypeParse     Bool "true"

test_NullType = do   assertEqual (Left $ TypeMismatch (1,1) (Leaf Null)  (Leaf Table)) $ matchTypeParseWith Null "f()" $ nullary "f" $ TableO (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1) (Leaf Null)  (Leaf Plot))  $ matchTypeParseWith Null "f()" $ nullary "f" $ PlotO  (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1) (Leaf Null)   NodeArr)     $ matchTypeParse     Null "[]"
                     assertEqual (Left $ TypeMismatch (1,1) (Leaf Null)   NodeObj)     $ matchTypeParse     Null "{}"
                     assertEqual (Left $ TypeMismatch (1,1) (Leaf Null)  (Leaf Str))   $ matchTypeParse     Null "\"\""
                     assertEqual (Left $ TypeMismatch (1,1) (Leaf Null)  (Leaf Num))   $ matchTypeParse     Null "0"
                     assertEqual (Left $ TypeMismatch (1,1) (Leaf Null)  (Leaf Bool))  $ matchTypeParse     Null "true"
                     assertEqual (Right $ NullO (1,1))                                 $ matchTypeParse     Null "null"

test_AnyType = do    assertEqual (Right $ TableO (1,1) [] [])                    $ matchTypeParseWith any "f()" $ nullary "f" $ TableO (1,1) [] []
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

test_NoneType = do   assertEqual (Left $ TypeMismatch (1,1) (getRoot none) (Leaf Table)) $ matchTypeParseWith none "f()" $ nullary "f" $ TableO (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1) (getRoot none) (Leaf Plot))  $ matchTypeParseWith none "f()" $ nullary "f" $ PlotO  (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1) (getRoot none)  NodeArr)     $ matchTypeParse     none "[]"
                     assertEqual (Left $ TypeMismatch (1,1) (getRoot none)  NodeObj)     $ matchTypeParse     none "{}"
                     assertEqual (Left $ TypeMismatch (1,1) (getRoot none) (Leaf Str))   $ matchTypeParse     none "\"\""
                     assertEqual (Left $ TypeMismatch (1,1) (getRoot none) (Leaf Num))   $ matchTypeParse     none "0"
                     assertEqual (Left $ TypeMismatch (1,1) (getRoot none) (Leaf Bool))  $ matchTypeParse     none "true"
                     assertEqual (Left $ TypeMismatch (1,1) (getRoot none) (Leaf Null))  $ matchTypeParse     none "null"

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

test_ArrOfType = do  assertEqual (Left $ TypeMismatch (1,2) (Leaf Table) (Leaf Plot))  $ matchTypeParseWith (ArrOf Table) "[f()]" $ nullary "f" $ PlotO  (1,2) [] []
                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Table)  NodeArr)     $ matchTypeParse     (ArrOf Table) "[[]]"
                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Table)  NodeObj)     $ matchTypeParse     (ArrOf Table) "[{}]"
                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Table) (Leaf Str))   $ matchTypeParse     (ArrOf Table) "[\"\"]"
                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Table) (Leaf Num))   $ matchTypeParse     (ArrOf Table) "[0]"
                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Table) (Leaf Bool))  $ matchTypeParse     (ArrOf Table) "[true]"
                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Table) (Leaf Null))  $ matchTypeParse     (ArrOf Table) "[null]"
                     assertEqual (Right $ ArrO (1,1) [TableO (1,2) [] []])             $ matchTypeParseWith (ArrOf Table) "[f()]" $ nullary "f" $ TableO (1,2) [] []

                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Plot)  (Leaf Table)) $ matchTypeParseWith (ArrOf Plot) "[f()]" $ nullary "f" $ TableO (1,2) [] []
                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Plot)   NodeArr)     $ matchTypeParse     (ArrOf Plot) "[[]]"
                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Plot)   NodeObj)     $ matchTypeParse     (ArrOf Plot) "[{}]"
                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Plot)  (Leaf Str))   $ matchTypeParse     (ArrOf Plot) "[\"\"]"
                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Plot)  (Leaf Num))   $ matchTypeParse     (ArrOf Plot) "[0]"
                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Plot)  (Leaf Bool))  $ matchTypeParse     (ArrOf Plot) "[true]"
                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Plot)  (Leaf Null))  $ matchTypeParse     (ArrOf Plot) "[null]"
                     assertEqual (Right $ ArrO (1,1) [PlotO (1,2) [] []])              $ matchTypeParseWith (ArrOf Plot) "[f()]" $ nullary "f" $ PlotO  (1,2) [] []

                     assertEqual (Left $ TypeMismatch (1,2)  NodeArr     (Leaf Table)) $ matchTypeParseWith (ArrOf arr) "[f()]" $ nullary "f" $ TableO (1,2) [] []
                     assertEqual (Left $ TypeMismatch (1,2)  NodeArr     (Leaf Plot))  $ matchTypeParseWith (ArrOf arr) "[f()]" $ nullary "f" $ PlotO  (1,2) [] []
                     assertEqual (Left $ TypeMismatch (1,2)  NodeArr      NodeObj)     $ matchTypeParse     (ArrOf arr) "[{}]"
                     assertEqual (Left $ TypeMismatch (1,2)  NodeArr     (Leaf Str))   $ matchTypeParse     (ArrOf arr) "[\"\"]"
                     assertEqual (Left $ TypeMismatch (1,2)  NodeArr     (Leaf Num))   $ matchTypeParse     (ArrOf arr) "[0]"
                     assertEqual (Left $ TypeMismatch (1,2)  NodeArr     (Leaf Bool))  $ matchTypeParse     (ArrOf arr) "[true]"
                     assertEqual (Left $ TypeMismatch (1,2)  NodeArr     (Leaf Null))  $ matchTypeParse     (ArrOf arr) "[null]"
                     assertEqual (Right $ ArrO (1,1) [ArrO (1,2) []])                  $ matchTypeParse     (ArrOf arr) "[[]]"

                     assertEqual (Left $ TypeMismatch (1,3)  NodeArr     (Leaf Table)) $ matchTypeParseWith (ArrOf $ ArrOf arr) "[[f()]]" $ nullary "f" $ TableO (1,3) [] []
                     assertEqual (Left $ TypeMismatch (1,3)  NodeArr     (Leaf Plot))  $ matchTypeParseWith (ArrOf $ ArrOf arr) "[[f()]]" $ nullary "f" $ PlotO  (1,3) [] []
                     assertEqual (Left $ TypeMismatch (1,3)  NodeArr      NodeObj)     $ matchTypeParse     (ArrOf $ ArrOf arr) "[[{}]]"
                     assertEqual (Left $ TypeMismatch (1,3)  NodeArr     (Leaf Str))   $ matchTypeParse     (ArrOf $ ArrOf arr) "[[\"\"]]"
                     assertEqual (Left $ TypeMismatch (1,3)  NodeArr     (Leaf Num))   $ matchTypeParse     (ArrOf $ ArrOf arr) "[[0]]"
                     assertEqual (Left $ TypeMismatch (1,3)  NodeArr     (Leaf Bool))  $ matchTypeParse     (ArrOf $ ArrOf arr) "[[true]]"
                     assertEqual (Left $ TypeMismatch (1,3)  NodeArr     (Leaf Null))  $ matchTypeParse     (ArrOf $ ArrOf arr) "[[null]]"
                     assertEqual (Right $ ArrO (1,1) [ArrO (1,2) [ArrO (1,3) []]])     $ matchTypeParse     (ArrOf $ ArrOf arr) "[[[]]]"

                     assertEqual (Right $ ArrO (1,1) [ArrO (1,2) [TableO (1,3) [] []]]) $ matchTypeParseWith (ArrOf arr) "[[f()]]" $ nullary "f" $ TableO (1,3) [] []
                     assertEqual (Right $ ArrO (1,1) [ArrO (1,2) [PlotO (1,3) [] []]])  $ matchTypeParseWith (ArrOf arr) "[[f()]]" $ nullary "f" $ PlotO  (1,3) [] []
                     assertEqual (Right $ ArrO (1,1) [ArrO (1,2) [ArrO (1,3) []]])      $ matchTypeParse     (ArrOf arr) "[[[]]]"
                     assertEqual (Right $ ArrO (1,1) [ArrO (1,2) [ObjO (1,3) []]])      $ matchTypeParse     (ArrOf arr) "[[{}]]"
                     assertEqual (Right $ ArrO (1,1) [ArrO (1,2) [StrO (1,3) ""]])      $ matchTypeParse     (ArrOf arr) "[[\"\"]]"
                     assertEqual (Right $ ArrO (1,1) [ArrO (1,2) [NumO (1,3) 0]])       $ matchTypeParse     (ArrOf arr) "[[0]]"
                     assertEqual (Right $ ArrO (1,1) [ArrO (1,2) [BoolO (1,3) True]])   $ matchTypeParse     (ArrOf arr) "[[true]]"
                     assertEqual (Right $ ArrO (1,1) [ArrO (1,2) [NullO (1,3)]])        $ matchTypeParse     (ArrOf arr) "[[null]]"

                     assertEqual (Left $ TypeMismatch (1,2)  NodeObj     (Leaf Table)) $ matchTypeParseWith (ArrOf obj) "[f()]" $ nullary "f" $ TableO (1,2) [] []
                     assertEqual (Left $ TypeMismatch (1,2)  NodeObj     (Leaf Plot))  $ matchTypeParseWith (ArrOf obj) "[f()]" $ nullary "f" $ PlotO  (1,2) [] []
                     assertEqual (Left $ TypeMismatch (1,2)  NodeObj      NodeArr)     $ matchTypeParse     (ArrOf obj) "[[]]"
                     assertEqual (Left $ TypeMismatch (1,2)  NodeObj     (Leaf Str))   $ matchTypeParse     (ArrOf obj) "[\"\"]"
                     assertEqual (Left $ TypeMismatch (1,2)  NodeObj     (Leaf Num))   $ matchTypeParse     (ArrOf obj) "[0]"
                     assertEqual (Left $ TypeMismatch (1,2)  NodeObj     (Leaf Bool))  $ matchTypeParse     (ArrOf obj) "[true]"
                     assertEqual (Left $ TypeMismatch (1,2)  NodeObj     (Leaf Null))  $ matchTypeParse     (ArrOf obj) "[null]"
                     assertEqual (Right $ ArrO (1,1) [ObjO (1,2) []])                  $ matchTypeParse     (ArrOf obj) "[{}]"

                     assertEqual (Left $ TypeMismatch (1,5)  NodeObj     (Leaf Table))   $ matchTypeParseWith (ArrOf $ ObjOf obj) "[{x:f()}]" $ nullary "f" $ TableO (1,5) [] []
                     assertEqual (Left $ TypeMismatch (1,5)  NodeObj     (Leaf Plot))    $ matchTypeParseWith (ArrOf $ ObjOf obj) "[{x:f()}]" $ nullary "f" $ PlotO  (1,5) [] []
                     assertEqual (Left $ TypeMismatch (1,5)  NodeObj      NodeArr)       $ matchTypeParse     (ArrOf $ ObjOf obj) "[{x:[]}]"
                     assertEqual (Left $ TypeMismatch (1,5)  NodeObj     (Leaf Str))     $ matchTypeParse     (ArrOf $ ObjOf obj) "[{x:\"\"}]"
                     assertEqual (Left $ TypeMismatch (1,5)  NodeObj     (Leaf Num))     $ matchTypeParse     (ArrOf $ ObjOf obj) "[{x:0}]"
                     assertEqual (Left $ TypeMismatch (1,5)  NodeObj     (Leaf Bool))    $ matchTypeParse     (ArrOf $ ObjOf obj) "[{x:true}]"
                     assertEqual (Left $ TypeMismatch (1,5)  NodeObj     (Leaf Null))    $ matchTypeParse     (ArrOf $ ObjOf obj) "[{x:null}]"
                     assertEqual (Right $ ArrO (1,1) [ObjO (1,2) [("x",ObjO (1,5) [])]]) $ matchTypeParse     (ArrOf $ ObjOf obj) "[{x:{}}]"

                     assertEqual (Right $ ArrO (1,1) [ObjO (1,2) [("x",TableO (1,5) [] [])]]) $ matchTypeParseWith (ArrOf obj) "[{x:f()}]" $ nullary "f" $ TableO (1,5) [] []
                     assertEqual (Right $ ArrO (1,1) [ObjO (1,2) [("x",PlotO (1,5) [] [])]])  $ matchTypeParseWith (ArrOf obj) "[{x:f()}]" $ nullary "f" $ PlotO  (1,5) [] []
                     assertEqual (Right $ ArrO (1,1) [ObjO (1,2) [("x",ArrO (1,5) [])]])      $ matchTypeParse     (ArrOf obj) "[{x:[]}]"
                     assertEqual (Right $ ArrO (1,1) [ObjO (1,2) [("x",ObjO (1,5) [])]])      $ matchTypeParse     (ArrOf obj) "[{x:{}}]"
                     assertEqual (Right $ ArrO (1,1) [ObjO (1,2) [("x",StrO (1,5) "")]])      $ matchTypeParse     (ArrOf obj) "[{x:\"\"}]"
                     assertEqual (Right $ ArrO (1,1) [ObjO (1,2) [("x",NumO (1,5) 0)]])       $ matchTypeParse     (ArrOf obj) "[{x:0}]"
                     assertEqual (Right $ ArrO (1,1) [ObjO (1,2) [("x",BoolO (1,5) True)]])   $ matchTypeParse     (ArrOf obj) "[{x:true}]"
                     assertEqual (Right $ ArrO (1,1) [ObjO (1,2) [("x",NullO (1,5))]])        $ matchTypeParse     (ArrOf obj) "[{x:null}]"

                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Str)   (Leaf Table)) $ matchTypeParseWith (ArrOf Str) "[f()]" $ nullary "f" $ TableO (1,2) [] []
                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Str)   (Leaf Plot))  $ matchTypeParseWith (ArrOf Str) "[f()]" $ nullary "f" $ PlotO  (1,2) [] []
                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Str)    NodeArr)     $ matchTypeParse     (ArrOf Str) "[[]]"
                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Str)    NodeObj)     $ matchTypeParse     (ArrOf Str) "[{}]"
                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Str)   (Leaf Num))   $ matchTypeParse     (ArrOf Str) "[0]"
                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Str)   (Leaf Bool))  $ matchTypeParse     (ArrOf Str) "[true]"
                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Str)   (Leaf Null))  $ matchTypeParse     (ArrOf Str) "[null]"
                     assertEqual (Right $ ArrO (1,1) [StrO (1,2) ""])                  $ matchTypeParse     (ArrOf Str) "[\"\"]"

                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Num)   (Leaf Table)) $ matchTypeParseWith (ArrOf Num) "[f()]" $ nullary "f" $ TableO (1,2) [] []
                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Num)   (Leaf Plot))  $ matchTypeParseWith (ArrOf Num) "[f()]" $ nullary "f" $ PlotO  (1,2) [] []
                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Num)    NodeArr)     $ matchTypeParse     (ArrOf Num) "[[]]"
                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Num)    NodeObj)     $ matchTypeParse     (ArrOf Num) "[{}]"
                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Num)   (Leaf Str))   $ matchTypeParse     (ArrOf Num) "[\"\"]"
                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Num)   (Leaf Bool))  $ matchTypeParse     (ArrOf Num) "[true]"
                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Num)   (Leaf Null))  $ matchTypeParse     (ArrOf Num) "[null]"
                     assertEqual (Right $ ArrO (1,1) [NumO (1,2) 0])                   $ matchTypeParse     (ArrOf Num) "[0]"

                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Bool)  (Leaf Table)) $ matchTypeParseWith (ArrOf Bool) "[f()]" $ nullary "f" $ TableO (1,2) [] []
                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Bool)  (Leaf Plot))  $ matchTypeParseWith (ArrOf Bool) "[f()]" $ nullary "f" $ PlotO  (1,2) [] []
                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Bool)   NodeArr)     $ matchTypeParse     (ArrOf Bool) "[[]]"
                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Bool)   NodeObj)     $ matchTypeParse     (ArrOf Bool) "[{}]"
                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Bool)  (Leaf Str))   $ matchTypeParse     (ArrOf Bool) "[\"\"]"
                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Bool)  (Leaf Num))   $ matchTypeParse     (ArrOf Bool) "[0]"
                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Bool)  (Leaf Null))  $ matchTypeParse     (ArrOf Bool) "[null]"
                     assertEqual (Right $ ArrO (1,1) [BoolO (1,2) True])               $ matchTypeParse     (ArrOf Bool) "[true]"

                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Null)  (Leaf Table)) $ matchTypeParseWith (ArrOf Null) "[f()]" $ nullary "f" $ TableO (1,2) [] []
                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Null)  (Leaf Plot))  $ matchTypeParseWith (ArrOf Null) "[f()]" $ nullary "f" $ PlotO  (1,2) [] []
                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Null)   NodeArr)     $ matchTypeParse     (ArrOf Null) "[[]]"
                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Null)   NodeObj)     $ matchTypeParse     (ArrOf Null) "[{}]"
                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Null)  (Leaf Str))   $ matchTypeParse     (ArrOf Null) "[\"\"]"
                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Null)  (Leaf Num))   $ matchTypeParse     (ArrOf Null) "[0]"
                     assertEqual (Left $ TypeMismatch (1,2) (Leaf Null)  (Leaf Bool))  $ matchTypeParse     (ArrOf Null) "[true]"
                     assertEqual (Right $ ArrO (1,1) [NullO (1,2)])                    $ matchTypeParse     (ArrOf Null) "[null]"

test_ObjOfType = do  assertEqual (Left $ TypeMismatch (1,4) (Leaf Table) (Leaf Plot))  $ matchTypeParseWith (ObjOf Table) "{x:f()}" $ nullary "f" $ PlotO  (1,4) [] []
                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Table)  NodeArr)     $ matchTypeParse     (ObjOf Table) "{x:[]}"
                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Table)  NodeObj)     $ matchTypeParse     (ObjOf Table) "{x:{}}"
                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Table) (Leaf Str))   $ matchTypeParse     (ObjOf Table) "{x:\"\"}"
                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Table) (Leaf Num))   $ matchTypeParse     (ObjOf Table) "{x:0}"
                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Table) (Leaf Bool))  $ matchTypeParse     (ObjOf Table) "{x:true}"
                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Table) (Leaf Null))  $ matchTypeParse     (ObjOf Table) "{x:null}"
                     assertEqual (Right $ ObjO (1,1) [("x",TableO (1,4) [] [])])       $ matchTypeParseWith (ObjOf Table) "{x:f()}" $ nullary "f" $ TableO (1,4) [] []

                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Plot)  (Leaf Table)) $ matchTypeParseWith (ObjOf Plot) "{x:f()}" $ nullary "f" $ TableO (1,4) [] []
                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Plot)   NodeArr)     $ matchTypeParse     (ObjOf Plot) "{x:[]}"
                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Plot)   NodeObj)     $ matchTypeParse     (ObjOf Plot) "{x:{}}"
                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Plot)  (Leaf Str))   $ matchTypeParse     (ObjOf Plot) "{x:\"\"}"
                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Plot)  (Leaf Num))   $ matchTypeParse     (ObjOf Plot) "{x:0}"
                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Plot)  (Leaf Bool))  $ matchTypeParse     (ObjOf Plot) "{x:true}"
                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Plot)  (Leaf Null))  $ matchTypeParse     (ObjOf Plot) "{x:null}"
                     assertEqual (Right $ ObjO (1,1) [("x",PlotO (1,4) [] [])])        $ matchTypeParseWith (ObjOf Plot) "{x:f()}" $ nullary "f" $ PlotO  (1,4) [] []

                     assertEqual (Left $ TypeMismatch (1,4)  NodeArr     (Leaf Table)) $ matchTypeParseWith (ObjOf arr) "{x:f()}" $ nullary "f" $ TableO (1,4) [] []
                     assertEqual (Left $ TypeMismatch (1,4)  NodeArr     (Leaf Plot))  $ matchTypeParseWith (ObjOf arr) "{x:f()}" $ nullary "f" $ PlotO  (1,4) [] []
                     assertEqual (Left $ TypeMismatch (1,4)  NodeArr      NodeObj)     $ matchTypeParse     (ObjOf arr) "{x:{}}"
                     assertEqual (Left $ TypeMismatch (1,4)  NodeArr     (Leaf Str))   $ matchTypeParse     (ObjOf arr) "{x:\"\"}"
                     assertEqual (Left $ TypeMismatch (1,4)  NodeArr     (Leaf Num))   $ matchTypeParse     (ObjOf arr) "{x:0}"
                     assertEqual (Left $ TypeMismatch (1,4)  NodeArr     (Leaf Bool))  $ matchTypeParse     (ObjOf arr) "{x:true}"
                     assertEqual (Left $ TypeMismatch (1,4)  NodeArr     (Leaf Null))  $ matchTypeParse     (ObjOf arr) "{x:null}"
                     assertEqual (Right $ ObjO (1,1) [("x",ArrO (1,4) [])])            $ matchTypeParse     (ObjOf arr) "{x:[]}"

                     assertEqual (Left $ TypeMismatch (1,5)  NodeArr     (Leaf Table))   $ matchTypeParseWith (ObjOf $ ArrOf arr) "{x:[f()]}" $ nullary "f" $ TableO (1,5) [] []
                     assertEqual (Left $ TypeMismatch (1,5)  NodeArr     (Leaf Plot))    $ matchTypeParseWith (ObjOf $ ArrOf arr) "{x:[f()]}" $ nullary "f" $ PlotO  (1,5) [] []
                     assertEqual (Left $ TypeMismatch (1,5)  NodeArr      NodeObj)       $ matchTypeParse     (ObjOf $ ArrOf arr) "{x:[{}]}"
                     assertEqual (Left $ TypeMismatch (1,5)  NodeArr     (Leaf Str))     $ matchTypeParse     (ObjOf $ ArrOf arr) "{x:[\"\"]}"
                     assertEqual (Left $ TypeMismatch (1,5)  NodeArr     (Leaf Num))     $ matchTypeParse     (ObjOf $ ArrOf arr) "{x:[0]}"
                     assertEqual (Left $ TypeMismatch (1,5)  NodeArr     (Leaf Bool))    $ matchTypeParse     (ObjOf $ ArrOf arr) "{x:[true]}"
                     assertEqual (Left $ TypeMismatch (1,5)  NodeArr     (Leaf Null))    $ matchTypeParse     (ObjOf $ ArrOf arr) "{x:[null]}"
                     assertEqual (Right $ ObjO (1,1) [("x",ArrO (1,4) [ArrO (1,5) []])]) $ matchTypeParse     (ObjOf $ ArrOf arr) "{x:[[]]}"

                     assertEqual (Right $ ObjO (1,1) [("x",ArrO (1,4) [TableO (1,5) [] []])]) $ matchTypeParseWith (ObjOf arr) "{x:[f()]}" $ nullary "f" $ TableO (1,5) [] []
                     assertEqual (Right $ ObjO (1,1) [("x",ArrO (1,4) [PlotO (1,5) [] []])])  $ matchTypeParseWith (ObjOf arr) "{x:[f()]}" $ nullary "f" $ PlotO  (1,5) [] []
                     assertEqual (Right $ ObjO (1,1) [("x",ArrO (1,4) [ArrO (1,5) []])])      $ matchTypeParse     (ObjOf arr) "{x:[[]]}"
                     assertEqual (Right $ ObjO (1,1) [("x",ArrO (1,4) [ObjO (1,5) []])])      $ matchTypeParse     (ObjOf arr) "{x:[{}]}"
                     assertEqual (Right $ ObjO (1,1) [("x",ArrO (1,4) [StrO (1,5) ""])])      $ matchTypeParse     (ObjOf arr) "{x:[\"\"]}"
                     assertEqual (Right $ ObjO (1,1) [("x",ArrO (1,4) [NumO (1,5) 0])])       $ matchTypeParse     (ObjOf arr) "{x:[0]}"
                     assertEqual (Right $ ObjO (1,1) [("x",ArrO (1,4) [BoolO (1,5) True])])   $ matchTypeParse     (ObjOf arr) "{x:[true]}"
                     assertEqual (Right $ ObjO (1,1) [("x",ArrO (1,4) [NullO (1,5)])])        $ matchTypeParse     (ObjOf arr) "{x:[null]}"

                     assertEqual (Left $ TypeMismatch (1,4)  NodeObj     (Leaf Table)) $ matchTypeParseWith (ObjOf obj) "{x:f()}" $ nullary "f" $ TableO (1,4) [] []
                     assertEqual (Left $ TypeMismatch (1,4)  NodeObj     (Leaf Plot))  $ matchTypeParseWith (ObjOf obj) "{x:f()}" $ nullary "f" $ PlotO  (1,4) [] []
                     assertEqual (Left $ TypeMismatch (1,4)  NodeObj      NodeArr)     $ matchTypeParse     (ObjOf obj) "{x:[]}"
                     assertEqual (Left $ TypeMismatch (1,4)  NodeObj     (Leaf Str))   $ matchTypeParse     (ObjOf obj) "{x:\"\"}"
                     assertEqual (Left $ TypeMismatch (1,4)  NodeObj     (Leaf Num))   $ matchTypeParse     (ObjOf obj) "{x:0}"
                     assertEqual (Left $ TypeMismatch (1,4)  NodeObj     (Leaf Bool))  $ matchTypeParse     (ObjOf obj) "{x:true}"
                     assertEqual (Left $ TypeMismatch (1,4)  NodeObj     (Leaf Null))  $ matchTypeParse     (ObjOf obj) "{x:null}"
                     assertEqual (Right $ ObjO (1,1) [("x",ObjO (1,4) [])])            $ matchTypeParse     (ObjOf obj) "{x:{}}"

                     assertEqual (Left $ TypeMismatch (1,7)  NodeObj     (Leaf Table))         $ matchTypeParseWith (ObjOf $ ObjOf obj) "{x:{x:f()}}" $ nullary "f" $ TableO (1,7) [] []
                     assertEqual (Left $ TypeMismatch (1,7)  NodeObj     (Leaf Plot))          $ matchTypeParseWith (ObjOf $ ObjOf obj) "{x:{x:f()}}" $ nullary "f" $ PlotO  (1,7) [] []
                     assertEqual (Left $ TypeMismatch (1,7)  NodeObj      NodeArr)             $ matchTypeParse     (ObjOf $ ObjOf obj) "{x:{x:[]}}"
                     assertEqual (Left $ TypeMismatch (1,7)  NodeObj     (Leaf Str))           $ matchTypeParse     (ObjOf $ ObjOf obj) "{x:{x:\"\"}}"
                     assertEqual (Left $ TypeMismatch (1,7)  NodeObj     (Leaf Num))           $ matchTypeParse     (ObjOf $ ObjOf obj) "{x:{x:0}}"
                     assertEqual (Left $ TypeMismatch (1,7)  NodeObj     (Leaf Bool))          $ matchTypeParse     (ObjOf $ ObjOf obj) "{x:{x:true}}"
                     assertEqual (Left $ TypeMismatch (1,7)  NodeObj     (Leaf Null))          $ matchTypeParse     (ObjOf $ ObjOf obj) "{x:{x:null}}"
                     assertEqual (Right $ ObjO (1,1) [("x",ObjO (1,4) [("x",ObjO (1,7) [])])]) $ matchTypeParse     (ObjOf $ ObjOf obj) "{x:{x:{}}}"

                     assertEqual (Right $ ObjO (1,1) [("x",ObjO (1,4) [("x",TableO (1,7) [] [])])]) $ matchTypeParseWith (ObjOf obj) "{x:{x:f()}}" $ nullary "f" $ TableO (1,7) [] []
                     assertEqual (Right $ ObjO (1,1) [("x",ObjO (1,4) [("x",PlotO (1,7) [] [])])])  $ matchTypeParseWith (ObjOf obj) "{x:{x:f()}}" $ nullary "f" $ PlotO  (1,7) [] []
                     assertEqual (Right $ ObjO (1,1) [("x",ObjO (1,4) [("x",ArrO (1,7) [])])])      $ matchTypeParse     (ObjOf obj) "{x:{x:[]}}"
                     assertEqual (Right $ ObjO (1,1) [("x",ObjO (1,4) [("x",ObjO (1,7) [])])])      $ matchTypeParse     (ObjOf obj) "{x:{x:{}}}"
                     assertEqual (Right $ ObjO (1,1) [("x",ObjO (1,4) [("x",StrO (1,7) "")])])      $ matchTypeParse     (ObjOf obj) "{x:{x:\"\"}}"
                     assertEqual (Right $ ObjO (1,1) [("x",ObjO (1,4) [("x",NumO (1,7) 0)])])       $ matchTypeParse     (ObjOf obj) "{x:{x:0}}"
                     assertEqual (Right $ ObjO (1,1) [("x",ObjO (1,4) [("x",BoolO (1,7) True)])])   $ matchTypeParse     (ObjOf obj) "{x:{x:true}}"
                     assertEqual (Right $ ObjO (1,1) [("x",ObjO (1,4) [("x",NullO (1,7))])])        $ matchTypeParse     (ObjOf obj) "{x:{x:null}}"

                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Str)   (Leaf Table)) $ matchTypeParseWith (ObjOf Str) "{x:f()}" $ nullary "f" $ TableO (1,4) [] []
                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Str)   (Leaf Plot))  $ matchTypeParseWith (ObjOf Str) "{x:f()}" $ nullary "f" $ PlotO  (1,4) [] []
                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Str)    NodeArr)     $ matchTypeParse     (ObjOf Str) "{x:[]}"
                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Str)    NodeObj)     $ matchTypeParse     (ObjOf Str) "{x:{}}"
                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Str)   (Leaf Num))   $ matchTypeParse     (ObjOf Str) "{x:0}"
                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Str)   (Leaf Bool))  $ matchTypeParse     (ObjOf Str) "{x:true}"
                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Str)   (Leaf Null))  $ matchTypeParse     (ObjOf Str) "{x:null}"
                     assertEqual (Right $ ObjO (1,1) [("x",StrO (1,4) "")])            $ matchTypeParse     (ObjOf Str) "{x:\"\"}"

                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Num)   (Leaf Table)) $ matchTypeParseWith (ObjOf Num) "{x:f()}" $ nullary "f" $ TableO (1,4) [] []
                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Num)   (Leaf Plot))  $ matchTypeParseWith (ObjOf Num) "{x:f()}" $ nullary "f" $ PlotO  (1,4) [] []
                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Num)    NodeArr)     $ matchTypeParse     (ObjOf Num) "{x:[]}"
                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Num)    NodeObj)     $ matchTypeParse     (ObjOf Num) "{x:{}}"
                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Num)   (Leaf Str))   $ matchTypeParse     (ObjOf Num) "{x:\"\"}"
                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Num)   (Leaf Bool))  $ matchTypeParse     (ObjOf Num) "{x:true}"
                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Num)   (Leaf Null))  $ matchTypeParse     (ObjOf Num) "{x:null}"
                     assertEqual (Right $ ObjO (1,1) [("x",NumO (1,4) 0)])             $ matchTypeParse     (ObjOf Num) "{x:0}"

                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Bool)  (Leaf Table)) $ matchTypeParseWith (ObjOf Bool) "{x:f()}" $ nullary "f" $ TableO (1,4) [] []
                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Bool)  (Leaf Plot))  $ matchTypeParseWith (ObjOf Bool) "{x:f()}" $ nullary "f" $ PlotO  (1,4) [] []
                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Bool)   NodeArr)     $ matchTypeParse     (ObjOf Bool) "{x:[]}"
                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Bool)   NodeObj)     $ matchTypeParse     (ObjOf Bool) "{x:{}}"
                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Bool)  (Leaf Str))   $ matchTypeParse     (ObjOf Bool) "{x:\"\"}"
                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Bool)  (Leaf Num))   $ matchTypeParse     (ObjOf Bool) "{x:0}"
                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Bool)  (Leaf Null))  $ matchTypeParse     (ObjOf Bool) "{x:null}"
                     assertEqual (Right $ ObjO (1,1) [("x",BoolO (1,4) True)])         $ matchTypeParse     (ObjOf Bool) "{x:true}"

                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Null)  (Leaf Table)) $ matchTypeParseWith (ObjOf Null) "{x:f()}" $ nullary "f" $ TableO (1,4) [] []
                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Null)  (Leaf Plot))  $ matchTypeParseWith (ObjOf Null) "{x:f()}" $ nullary "f" $ PlotO  (1,4) [] []
                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Null)   NodeArr)     $ matchTypeParse     (ObjOf Null) "{x:[]}"
                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Null)   NodeObj)     $ matchTypeParse     (ObjOf Null) "{x:{}}"
                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Null)  (Leaf Str))   $ matchTypeParse     (ObjOf Null) "{x:\"\"}"
                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Null)  (Leaf Num))   $ matchTypeParse     (ObjOf Null) "{x:0}"
                     assertEqual (Left $ TypeMismatch (1,4) (Leaf Null)  (Leaf Bool))  $ matchTypeParse     (ObjOf Null) "{x:true}"
                     assertEqual (Right $ ObjO (1,1) [("x",NullO (1,4))])              $ matchTypeParse     (ObjOf Null) "{x:null}"

test_OrType = do     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [Leaf Bool, Leaf Null]) (Leaf Table)) $ matchTypeParseWith (Or [Bool,Null]) "f()" $ nullary "f" $ TableO (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [Leaf Bool, Leaf Null]) (Leaf Plot))  $ matchTypeParseWith (Or [Null,Bool]) "f()" $ nullary "f" $ PlotO (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [Leaf Bool, Leaf Null])  NodeArr)     $ matchTypeParse     (Or [Bool,Null]) "[]"
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [Leaf Bool, Leaf Null])  NodeObj)     $ matchTypeParse     (Or [Null,Bool]) "{}"
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [Leaf Bool, Leaf Null]) (Leaf Str))   $ matchTypeParse     (Or [Bool,Null]) "\"\""
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [Leaf Bool, Leaf Null]) (Leaf Num))   $ matchTypeParse     (Or [Null,Bool]) "0"
                     assertEqual (Right $ BoolO (1,1) True)                                               $ matchTypeParse     (Or [Bool,Null]) "true"
                     assertEqual (Right $ NullO (1,1))                                                    $ matchTypeParse     (Or [Null,Bool]) "null"
                     
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [Leaf Str, Leaf Num]) (Leaf Table)) $ matchTypeParseWith (Or [Str,Num]) "f()" $ nullary "f" $ TableO (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [Leaf Str, Leaf Num]) (Leaf Plot))  $ matchTypeParseWith (Or [Num,Str]) "f()" $ nullary "f" $ PlotO (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [Leaf Str, Leaf Num])  NodeArr)     $ matchTypeParse     (Or [Str,Num]) "[]"
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [Leaf Str, Leaf Num])  NodeObj)     $ matchTypeParse     (Or [Num,Str]) "{}"
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [Leaf Str, Leaf Num]) (Leaf Bool))  $ matchTypeParse     (Or [Str,Num]) "true"
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [Leaf Str, Leaf Num]) (Leaf Null))  $ matchTypeParse     (Or [Num,Str]) "null"
                     assertEqual (Right $ StrO (1,1) "")                                                $ matchTypeParse     (Or [Str,Num]) "\"\""
                     assertEqual (Right $ NumO (1,1) 0)                                                 $ matchTypeParse     (Or [Num,Str]) "0"

                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [NodeArr,NodeObj]) (Leaf Table)) $ matchTypeParseWith (Or [arr,obj]) "f()" $ nullary "f" $ TableO (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [NodeArr,NodeObj]) (Leaf Plot))  $ matchTypeParseWith (Or [obj,arr]) "f()" $ nullary "f" $ PlotO (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [NodeArr,NodeObj]) (Leaf Str))   $ matchTypeParse     (Or [arr,obj]) "\"\""
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [NodeArr,NodeObj]) (Leaf Num))   $ matchTypeParse     (Or [obj,arr]) "0"
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [NodeArr,NodeObj]) (Leaf Bool))  $ matchTypeParse     (Or [arr,obj]) "true"
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [NodeArr,NodeObj]) (Leaf Null))  $ matchTypeParse     (Or [obj,arr]) "null"
                     assertEqual (Right $ ArrO (1,1) [])                                             $ matchTypeParse     (Or [arr,obj]) "[]"
                     assertEqual (Right $ ObjO (1,1) [])                                             $ matchTypeParse     (Or [obj,arr]) "{}"

                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [Leaf Table, Leaf Plot])  NodeArr)     $ matchTypeParse     (Or [Table,Plot]) "[]"
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [Leaf Table, Leaf Plot])  NodeObj)     $ matchTypeParse     (Or [Plot,Table]) "{}" 
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [Leaf Table, Leaf Plot]) (Leaf Str))   $ matchTypeParse     (Or [Table,Plot]) "\"\""
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [Leaf Table, Leaf Plot]) (Leaf Num))   $ matchTypeParse     (Or [Plot,Table]) "0"
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [Leaf Table, Leaf Plot]) (Leaf Bool))  $ matchTypeParse     (Or [Table,Plot]) "true"
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [Leaf Table, Leaf Plot]) (Leaf Null))  $ matchTypeParse     (Or [Plot,Table]) "null"
                     assertEqual (Right $ TableO (1,1) [] [])                                              $ matchTypeParseWith (Or [Table,Plot]) "f()" $ nullary "f" $ TableO (1,1) [] []
                     assertEqual (Right $ PlotO (1,1) [] [])                                               $ matchTypeParseWith (Or [Plot,Table]) "f()" $ nullary "f" $ PlotO (1,1) [] []




















                    