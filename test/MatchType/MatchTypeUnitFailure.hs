{-# OPTIONS_GHC -F -pgmF htfpp #-}
module MatchType.MatchTypeUnitFailure where

import Prelude hiding (any)

import Data.EvalError
import Data.ExpObj
import Data.Type
import Eval.MatchType
import Test.Framework

import MatchType.MatchTypeUnitUtils

{-# ANN module "HLint: ignore Use camelCase" #-}
{-# ANN module "HLint: ignore Reduce duplication" #-}

test_TableType = do  assertEqual (Left $ TypeMismatch (1,1) LeafTable LeafPlot)  $ matchTypeParseWith Table "f()" $ nullary "f" $ PlotO  (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1) LeafTable NodeArr)   $ matchTypeParse     Table "[]"
                     assertEqual (Left $ TypeMismatch (1,1) LeafTable NodeObj)   $ matchTypeParse     Table "{}"
                     assertEqual (Left $ TypeMismatch (1,1) LeafTable LeafStr)   $ matchTypeParse     Table "\"\""
                     assertEqual (Left $ TypeMismatch (1,1) LeafTable LeafNum)   $ matchTypeParse     Table "0"
                     assertEqual (Left $ TypeMismatch (1,1) LeafTable LeafBool)  $ matchTypeParse     Table "false"
                     assertEqual (Left $ TypeMismatch (1,1) LeafTable LeafNull)  $ matchTypeParse     Table "null"

test_PlotType = do   assertEqual (Left $ TypeMismatch (1,1) LeafPlot LeafTable) $ matchTypeParseWith Plot "f()" $ nullary "f" $ TableO (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1) LeafPlot NodeArr)   $ matchTypeParse     Plot "[]"
                     assertEqual (Left $ TypeMismatch (1,1) LeafPlot NodeObj)   $ matchTypeParse     Plot "{}"
                     assertEqual (Left $ TypeMismatch (1,1) LeafPlot LeafStr)   $ matchTypeParse     Plot "\"\""
                     assertEqual (Left $ TypeMismatch (1,1) LeafPlot LeafNum)   $ matchTypeParse     Plot "0"
                     assertEqual (Left $ TypeMismatch (1,1) LeafPlot LeafBool)  $ matchTypeParse     Plot "false"
                     assertEqual (Left $ TypeMismatch (1,1) LeafPlot LeafNull)  $ matchTypeParse     Plot "null"

test_ArrType = do    assertEqual (Left $ TypeMismatch (1,1)  NodeArr LeafTable) $ matchTypeParseWith arr "f()" $ nullary "f" $ TableO (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1)  NodeArr LeafPlot)  $ matchTypeParseWith arr "f()" $ nullary "f" $ PlotO  (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1)  NodeArr NodeObj)   $ matchTypeParse     arr "{}"
                     assertEqual (Left $ TypeMismatch (1,1)  NodeArr LeafStr)   $ matchTypeParse     arr "\"\""
                     assertEqual (Left $ TypeMismatch (1,1)  NodeArr LeafNum)   $ matchTypeParse     arr "0"
                     assertEqual (Left $ TypeMismatch (1,1)  NodeArr LeafBool)  $ matchTypeParse     arr "false"
                     assertEqual (Left $ TypeMismatch (1,1)  NodeArr LeafNull)  $ matchTypeParse     arr "null"

                     assertEqual (Left $ TypeMismatch (1,2)  NodeArr LeafTable) $ matchTypeParseWith (ArrOf arr) "[f()]" $ nullary "f" $ TableO (1,2) [] []
                     assertEqual (Left $ TypeMismatch (1,2)  NodeArr LeafPlot)  $ matchTypeParseWith (ArrOf arr) "[f()]" $ nullary "f" $ PlotO  (1,2) [] []
                     assertEqual (Left $ TypeMismatch (1,2)  NodeArr NodeObj)   $ matchTypeParse     (ArrOf arr) "[{}]"
                     assertEqual (Left $ TypeMismatch (1,2)  NodeArr LeafStr)   $ matchTypeParse     (ArrOf arr) "[\"\"]"
                     assertEqual (Left $ TypeMismatch (1,2)  NodeArr LeafNum)   $ matchTypeParse     (ArrOf arr) "[0]"
                     assertEqual (Left $ TypeMismatch (1,2)  NodeArr LeafBool)  $ matchTypeParse     (ArrOf arr) "[false]"
                     assertEqual (Left $ TypeMismatch (1,2)  NodeArr LeafNull)  $ matchTypeParse     (ArrOf arr) "[null]"

test_ObjType = do    assertEqual (Left $ TypeMismatch (1,1)  NodeObj LeafTable) $ matchTypeParseWith obj "f()" $ nullary "f" $ TableO (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1)  NodeObj LeafPlot)  $ matchTypeParseWith obj "f()" $ nullary "f" $ PlotO  (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1)  NodeObj NodeArr)   $ matchTypeParse     obj "[]"
                     assertEqual (Left $ TypeMismatch (1,1)  NodeObj LeafStr)   $ matchTypeParse     obj "\"\""
                     assertEqual (Left $ TypeMismatch (1,1)  NodeObj LeafNum)   $ matchTypeParse     obj "0"
                     assertEqual (Left $ TypeMismatch (1,1)  NodeObj LeafBool)  $ matchTypeParse     obj "false"
                     assertEqual (Left $ TypeMismatch (1,1)  NodeObj LeafNull)  $ matchTypeParse     obj "null"

                     assertEqual (Left $ TypeMismatch (1,4)  NodeObj LeafTable) $ matchTypeParseWith (ObjOf obj) "{x:f()}" $ nullary "f" $ TableO (1,4) [] []
                     assertEqual (Left $ TypeMismatch (1,4)  NodeObj LeafPlot)  $ matchTypeParseWith (ObjOf obj) "{x:f()}" $ nullary "f" $ PlotO  (1,4) [] []
                     assertEqual (Left $ TypeMismatch (1,4)  NodeObj NodeArr)   $ matchTypeParse     (ObjOf obj) "{x:[]}"
                     assertEqual (Left $ TypeMismatch (1,4)  NodeObj LeafStr)   $ matchTypeParse     (ObjOf obj) "{x:\"\"}"
                     assertEqual (Left $ TypeMismatch (1,4)  NodeObj LeafNum)   $ matchTypeParse     (ObjOf obj) "{x:0}"
                     assertEqual (Left $ TypeMismatch (1,4)  NodeObj LeafBool)  $ matchTypeParse     (ObjOf obj) "{x:false}"
                     assertEqual (Left $ TypeMismatch (1,4)  NodeObj LeafNull)  $ matchTypeParse     (ObjOf obj) "{x:null}"

test_StrType = do    assertEqual (Left $ TypeMismatch (1,1) LeafStr   LeafTable) $ matchTypeParseWith Str "f()" $ nullary "f" $ TableO (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1) LeafStr   LeafPlot)  $ matchTypeParseWith Str "f()" $ nullary "f" $ PlotO  (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1) LeafStr   NodeArr)   $ matchTypeParse     Str "[]"
                     assertEqual (Left $ TypeMismatch (1,1) LeafStr   NodeObj)   $ matchTypeParse     Str "{}"
                     assertEqual (Left $ TypeMismatch (1,1) LeafStr   LeafNum)   $ matchTypeParse     Str "0"
                     assertEqual (Left $ TypeMismatch (1,1) LeafStr   LeafBool)  $ matchTypeParse     Str "false"
                     assertEqual (Left $ TypeMismatch (1,1) LeafStr   LeafNull)  $ matchTypeParse     Str "null"

test_NumType = do    assertEqual (Left $ TypeMismatch (1,1) LeafNum LeafTable) $ matchTypeParseWith Num "f()" $ nullary "f" $ TableO (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1) LeafNum LeafPlot)  $ matchTypeParseWith Num "f()" $ nullary "f" $ PlotO  (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1) LeafNum NodeArr)   $ matchTypeParse     Num "[]"
                     assertEqual (Left $ TypeMismatch (1,1) LeafNum NodeObj)   $ matchTypeParse     Num "{}"
                     assertEqual (Left $ TypeMismatch (1,1) LeafNum LeafStr)   $ matchTypeParse     Num "\"\""
                     assertEqual (Left $ TypeMismatch (1,1) LeafNum LeafBool)  $ matchTypeParse     Num "false"
                     assertEqual (Left $ TypeMismatch (1,1) LeafNum LeafNull)  $ matchTypeParse     Num "null"

test_BoolType = do   assertEqual (Left $ TypeMismatch (1,1) LeafBool  LeafTable) $ matchTypeParseWith Bool "f()" $ nullary "f" $ TableO (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1) LeafBool  LeafPlot)  $ matchTypeParseWith Bool "f()" $ nullary "f" $ PlotO  (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1) LeafBool  NodeArr)   $ matchTypeParse     Bool "[]"
                     assertEqual (Left $ TypeMismatch (1,1) LeafBool  NodeObj)   $ matchTypeParse     Bool "{}"
                     assertEqual (Left $ TypeMismatch (1,1) LeafBool  LeafStr)   $ matchTypeParse     Bool "\"\""
                     assertEqual (Left $ TypeMismatch (1,1) LeafBool  LeafNum)   $ matchTypeParse     Bool "0"
                     assertEqual (Left $ TypeMismatch (1,1) LeafBool  LeafNull)  $ matchTypeParse     Bool "null"

test_NullType = do   assertEqual (Left $ TypeMismatch (1,1) LeafNull  LeafTable) $ matchTypeParseWith Null "f()" $ nullary "f" $ TableO (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1) LeafNull  LeafPlot)  $ matchTypeParseWith Null "f()" $ nullary "f" $ PlotO  (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1) LeafNull  NodeArr)   $ matchTypeParse     Null "[]"
                     assertEqual (Left $ TypeMismatch (1,1) LeafNull  NodeObj)   $ matchTypeParse     Null "{}"
                     assertEqual (Left $ TypeMismatch (1,1) LeafNull  LeafStr)   $ matchTypeParse     Null "\"\""
                     assertEqual (Left $ TypeMismatch (1,1) LeafNull  LeafNum)   $ matchTypeParse     Null "0"
                     assertEqual (Left $ TypeMismatch (1,1) LeafNull  LeafBool)  $ matchTypeParse     Null "false"
                     assertEqual (Right $ NullO (1,1))                           $ matchTypeParse     Null "null"

test_NoneType = do   assertEqual (Left $ TypeMismatch (1,1) (getRoot none) LeafTable) $ matchTypeParseWith none "f()" $ nullary "f" $ TableO (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1) (getRoot none) LeafPlot)  $ matchTypeParseWith none "f()" $ nullary "f" $ PlotO  (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1) (getRoot none) NodeArr)   $ matchTypeParse     none "[]"
                     assertEqual (Left $ TypeMismatch (1,1) (getRoot none) NodeObj)   $ matchTypeParse     none "{}"
                     assertEqual (Left $ TypeMismatch (1,1) (getRoot none) LeafStr)   $ matchTypeParse     none "\"\""
                     assertEqual (Left $ TypeMismatch (1,1) (getRoot none) LeafNum)   $ matchTypeParse     none "0"
                     assertEqual (Left $ TypeMismatch (1,1) (getRoot none) LeafBool)  $ matchTypeParse     none "false"
                     assertEqual (Left $ TypeMismatch (1,1) (getRoot none) LeafNull)  $ matchTypeParse     none "null"

                     assertEqual (Left $ TypeMismatch (1,2) (getRoot none) LeafTable) $ matchTypeParseWith (ArrOf none) "[f()]" $ nullary "f" $ TableO (1,2) [] []
                     assertEqual (Left $ TypeMismatch (1,2) (getRoot none) LeafPlot)  $ matchTypeParseWith (ArrOf none) "[f()]" $ nullary "f" $ PlotO  (1,2) [] []
                     assertEqual (Left $ TypeMismatch (1,2) (getRoot none) NodeArr)   $ matchTypeParse     (ArrOf none) "[[]]"
                     assertEqual (Left $ TypeMismatch (1,2) (getRoot none) NodeObj)   $ matchTypeParse     (ArrOf none) "[{}]"
                     assertEqual (Left $ TypeMismatch (1,2) (getRoot none) LeafStr)   $ matchTypeParse     (ArrOf none) "[\"\"]"
                     assertEqual (Left $ TypeMismatch (1,2) (getRoot none) LeafNum)   $ matchTypeParse     (ArrOf none) "[0]"
                     assertEqual (Left $ TypeMismatch (1,2) (getRoot none) LeafBool)  $ matchTypeParse     (ArrOf none) "[false]"
                     assertEqual (Left $ TypeMismatch (1,2) (getRoot none) LeafNull)  $ matchTypeParse     (ArrOf none) "[null]"

                     assertEqual (Left $ TypeMismatch (1,1) (getRoot none) NodeArr) $ matchTypeParseWith none "[f()]" $ nullary "f" $ TableO (1,2) [] []
                     assertEqual (Left $ TypeMismatch (1,1) (getRoot none) NodeArr) $ matchTypeParseWith none "[f()]" $ nullary "f" $ PlotO  (1,2) [] []
                     assertEqual (Left $ TypeMismatch (1,1) (getRoot none) NodeArr) $ matchTypeParse     none "[[]]"
                     assertEqual (Left $ TypeMismatch (1,1) (getRoot none) NodeArr) $ matchTypeParse     none "[{}]"
                     assertEqual (Left $ TypeMismatch (1,1) (getRoot none) NodeArr) $ matchTypeParse     none "[\"\"]"
                     assertEqual (Left $ TypeMismatch (1,1) (getRoot none) NodeArr) $ matchTypeParse     none "[0]"
                     assertEqual (Left $ TypeMismatch (1,1) (getRoot none) NodeArr) $ matchTypeParse     none "[false]"
                     assertEqual (Left $ TypeMismatch (1,1) (getRoot none) NodeArr) $ matchTypeParse     none "[null]"

                     assertEqual (Left $ TypeMismatch (1,4) (getRoot none) LeafTable) $ matchTypeParseWith (ObjOf none) "{x:f()}" $ nullary "f" $ TableO (1,4) [] []
                     assertEqual (Left $ TypeMismatch (1,4) (getRoot none) LeafPlot)  $ matchTypeParseWith (ObjOf none) "{x:f()}" $ nullary "f" $ PlotO  (1,4) [] []
                     assertEqual (Left $ TypeMismatch (1,4) (getRoot none) NodeArr)   $ matchTypeParse     (ObjOf none) "{x:[]}"
                     assertEqual (Left $ TypeMismatch (1,4) (getRoot none) NodeObj)   $ matchTypeParse     (ObjOf none) "{x:{}}"
                     assertEqual (Left $ TypeMismatch (1,4) (getRoot none) LeafStr)   $ matchTypeParse     (ObjOf none) "{x:\"\"}"
                     assertEqual (Left $ TypeMismatch (1,4) (getRoot none) LeafNum)   $ matchTypeParse     (ObjOf none) "{x:0}"
                     assertEqual (Left $ TypeMismatch (1,4) (getRoot none) LeafBool)  $ matchTypeParse     (ObjOf none) "{x:false}"
                     assertEqual (Left $ TypeMismatch (1,4) (getRoot none) LeafNull)  $ matchTypeParse     (ObjOf none) "{x:null}"

                     assertEqual (Left $ TypeMismatch (1,1) (getRoot none) NodeObj) $ matchTypeParseWith none "{x:f()}" $ nullary "f" $ TableO (1,4) [] []
                     assertEqual (Left $ TypeMismatch (1,1) (getRoot none) NodeObj) $ matchTypeParseWith none "{x:f()}" $ nullary "f" $ PlotO  (1,4) [] []
                     assertEqual (Left $ TypeMismatch (1,1) (getRoot none) NodeObj) $ matchTypeParse     none "{x:[]}"
                     assertEqual (Left $ TypeMismatch (1,1) (getRoot none) NodeObj) $ matchTypeParse     none "{x:{}}"
                     assertEqual (Left $ TypeMismatch (1,1) (getRoot none) NodeObj) $ matchTypeParse     none "{x:\"\"}"
                     assertEqual (Left $ TypeMismatch (1,1) (getRoot none) NodeObj) $ matchTypeParse     none "{x:0}"
                     assertEqual (Left $ TypeMismatch (1,1) (getRoot none) NodeObj) $ matchTypeParse     none "{x:false}"
                     assertEqual (Left $ TypeMismatch (1,1) (getRoot none) NodeObj) $ matchTypeParse     none "{x:null}"

test_ArrOfType = do  assertEqual (Left $ TypeMismatch (1,2) LeafTable LeafPlot) $ matchTypeParseWith (ArrOf Table) "[f()]" $ nullary "f" $ PlotO  (1,2) [] []
                     assertEqual (Left $ TypeMismatch (1,2) LeafTable NodeArr)  $ matchTypeParse     (ArrOf Table) "[[]]"
                     assertEqual (Left $ TypeMismatch (1,2) LeafTable NodeObj)  $ matchTypeParse     (ArrOf Table) "[{}]"
                     assertEqual (Left $ TypeMismatch (1,2) LeafTable LeafStr)  $ matchTypeParse     (ArrOf Table) "[\"\"]"
                     assertEqual (Left $ TypeMismatch (1,2) LeafTable LeafNum)  $ matchTypeParse     (ArrOf Table) "[0]"
                     assertEqual (Left $ TypeMismatch (1,2) LeafTable LeafBool) $ matchTypeParse     (ArrOf Table) "[false]"
                     assertEqual (Left $ TypeMismatch (1,2) LeafTable LeafNull) $ matchTypeParse     (ArrOf Table) "[null]"

                     assertEqual (Left $ TypeMismatch (1,2) LeafPlot  LeafTable) $ matchTypeParseWith (ArrOf Plot) "[f()]" $ nullary "f" $ TableO (1,2) [] []
                     assertEqual (Left $ TypeMismatch (1,2) LeafPlot  NodeArr)   $ matchTypeParse     (ArrOf Plot) "[[]]"
                     assertEqual (Left $ TypeMismatch (1,2) LeafPlot  NodeObj)   $ matchTypeParse     (ArrOf Plot) "[{}]"
                     assertEqual (Left $ TypeMismatch (1,2) LeafPlot  LeafStr)   $ matchTypeParse     (ArrOf Plot) "[\"\"]"
                     assertEqual (Left $ TypeMismatch (1,2) LeafPlot  LeafNum)   $ matchTypeParse     (ArrOf Plot) "[0]"
                     assertEqual (Left $ TypeMismatch (1,2) LeafPlot  LeafBool)  $ matchTypeParse     (ArrOf Plot) "[false]"
                     assertEqual (Left $ TypeMismatch (1,2) LeafPlot  LeafNull)  $ matchTypeParse     (ArrOf Plot) "[null]"

                     assertEqual (Left $ TypeMismatch (1,2)  NodeArr LeafTable) $ matchTypeParseWith (ArrOf arr) "[f()]" $ nullary "f" $ TableO (1,2) [] []
                     assertEqual (Left $ TypeMismatch (1,2)  NodeArr LeafPlot)  $ matchTypeParseWith (ArrOf arr) "[f()]" $ nullary "f" $ PlotO  (1,2) [] []
                     assertEqual (Left $ TypeMismatch (1,2)  NodeArr NodeObj)   $ matchTypeParse     (ArrOf arr) "[{}]"
                     assertEqual (Left $ TypeMismatch (1,2)  NodeArr LeafStr)   $ matchTypeParse     (ArrOf arr) "[\"\"]"
                     assertEqual (Left $ TypeMismatch (1,2)  NodeArr LeafNum)   $ matchTypeParse     (ArrOf arr) "[0]"
                     assertEqual (Left $ TypeMismatch (1,2)  NodeArr LeafBool)  $ matchTypeParse     (ArrOf arr) "[false]"
                     assertEqual (Left $ TypeMismatch (1,2)  NodeArr LeafNull)  $ matchTypeParse     (ArrOf arr) "[null]"

                     assertEqual (Left $ TypeMismatch (1,3)  NodeArr LeafTable)    $ matchTypeParseWith (ArrOf $ ArrOf arr) "[[f()]]" $ nullary "f" $ TableO (1,3) [] []
                     assertEqual (Left $ TypeMismatch (1,3)  NodeArr LeafPlot)     $ matchTypeParseWith (ArrOf $ ArrOf arr) "[[f()]]" $ nullary "f" $ PlotO  (1,3) [] []
                     assertEqual (Left $ TypeMismatch (1,3)  NodeArr NodeObj)      $ matchTypeParse     (ArrOf $ ArrOf arr) "[[{}]]"
                     assertEqual (Left $ TypeMismatch (1,3)  NodeArr LeafStr)      $ matchTypeParse     (ArrOf $ ArrOf arr) "[[\"\"]]"
                     assertEqual (Left $ TypeMismatch (1,3)  NodeArr LeafNum)      $ matchTypeParse     (ArrOf $ ArrOf arr) "[[0]]"
                     assertEqual (Left $ TypeMismatch (1,3)  NodeArr LeafBool)     $ matchTypeParse     (ArrOf $ ArrOf arr) "[[false]]"
                     assertEqual (Left $ TypeMismatch (1,3)  NodeArr LeafNull)     $ matchTypeParse     (ArrOf $ ArrOf arr) "[[null]]"

                     assertEqual (Left $ TypeMismatch (1,2)  NodeObj LeafTable) $ matchTypeParseWith (ArrOf obj) "[f()]" $ nullary "f" $ TableO (1,2) [] []
                     assertEqual (Left $ TypeMismatch (1,2)  NodeObj LeafPlot)  $ matchTypeParseWith (ArrOf obj) "[f()]" $ nullary "f" $ PlotO  (1,2) [] []
                     assertEqual (Left $ TypeMismatch (1,2)  NodeObj NodeArr)   $ matchTypeParse     (ArrOf obj) "[[]]"
                     assertEqual (Left $ TypeMismatch (1,2)  NodeObj LeafStr)   $ matchTypeParse     (ArrOf obj) "[\"\"]"
                     assertEqual (Left $ TypeMismatch (1,2)  NodeObj LeafNum)   $ matchTypeParse     (ArrOf obj) "[0]"
                     assertEqual (Left $ TypeMismatch (1,2)  NodeObj LeafBool)  $ matchTypeParse     (ArrOf obj) "[false]"
                     assertEqual (Left $ TypeMismatch (1,2)  NodeObj LeafNull)  $ matchTypeParse     (ArrOf obj) "[null]"

                     assertEqual (Left $ TypeMismatch (1,5)  NodeObj LeafTable)          $ matchTypeParseWith (ArrOf $ ObjOf obj) "[{x:f()}]" $ nullary "f" $ TableO (1,5) [] []
                     assertEqual (Left $ TypeMismatch (1,5)  NodeObj LeafPlot)           $ matchTypeParseWith (ArrOf $ ObjOf obj) "[{x:f()}]" $ nullary "f" $ PlotO  (1,5) [] []
                     assertEqual (Left $ TypeMismatch (1,5)  NodeObj NodeArr)            $ matchTypeParse     (ArrOf $ ObjOf obj) "[{x:[]}]"
                     assertEqual (Left $ TypeMismatch (1,5)  NodeObj LeafStr)            $ matchTypeParse     (ArrOf $ ObjOf obj) "[{x:\"\"}]"
                     assertEqual (Left $ TypeMismatch (1,5)  NodeObj LeafNum)            $ matchTypeParse     (ArrOf $ ObjOf obj) "[{x:0}]"
                     assertEqual (Left $ TypeMismatch (1,5)  NodeObj LeafBool)           $ matchTypeParse     (ArrOf $ ObjOf obj) "[{x:false}]"
                     assertEqual (Left $ TypeMismatch (1,5)  NodeObj LeafNull)           $ matchTypeParse     (ArrOf $ ObjOf obj) "[{x:null}]"

                     assertEqual (Left $ TypeMismatch (1,2) LeafStr LeafTable) $ matchTypeParseWith (ArrOf Str) "[f()]" $ nullary "f" $ TableO (1,2) [] []
                     assertEqual (Left $ TypeMismatch (1,2) LeafStr LeafPlot)  $ matchTypeParseWith (ArrOf Str) "[f()]" $ nullary "f" $ PlotO  (1,2) [] []
                     assertEqual (Left $ TypeMismatch (1,2) LeafStr NodeArr)   $ matchTypeParse     (ArrOf Str) "[[]]"
                     assertEqual (Left $ TypeMismatch (1,2) LeafStr NodeObj)   $ matchTypeParse     (ArrOf Str) "[{}]"
                     assertEqual (Left $ TypeMismatch (1,2) LeafStr LeafNum)   $ matchTypeParse     (ArrOf Str) "[0]"
                     assertEqual (Left $ TypeMismatch (1,2) LeafStr LeafBool)  $ matchTypeParse     (ArrOf Str) "[false]"
                     assertEqual (Left $ TypeMismatch (1,2) LeafStr LeafNull)  $ matchTypeParse     (ArrOf Str) "[null]"

                     assertEqual (Left $ TypeMismatch (1,2) LeafNum LeafTable) $ matchTypeParseWith (ArrOf Num) "[f()]" $ nullary "f" $ TableO (1,2) [] []
                     assertEqual (Left $ TypeMismatch (1,2) LeafNum LeafPlot)  $ matchTypeParseWith (ArrOf Num) "[f()]" $ nullary "f" $ PlotO  (1,2) [] []
                     assertEqual (Left $ TypeMismatch (1,2) LeafNum NodeArr)   $ matchTypeParse     (ArrOf Num) "[[]]"
                     assertEqual (Left $ TypeMismatch (1,2) LeafNum NodeObj)   $ matchTypeParse     (ArrOf Num) "[{}]"
                     assertEqual (Left $ TypeMismatch (1,2) LeafNum LeafStr)   $ matchTypeParse     (ArrOf Num) "[\"\"]"
                     assertEqual (Left $ TypeMismatch (1,2) LeafNum LeafBool)  $ matchTypeParse     (ArrOf Num) "[false]"
                     assertEqual (Left $ TypeMismatch (1,2) LeafNum LeafNull)  $ matchTypeParse     (ArrOf Num) "[null]"

                     assertEqual (Left $ TypeMismatch (1,2) LeafBool LeafTable) $ matchTypeParseWith (ArrOf Bool) "[f()]" $ nullary "f" $ TableO (1,2) [] []
                     assertEqual (Left $ TypeMismatch (1,2) LeafBool LeafPlot)  $ matchTypeParseWith (ArrOf Bool) "[f()]" $ nullary "f" $ PlotO  (1,2) [] []
                     assertEqual (Left $ TypeMismatch (1,2) LeafBool NodeArr)   $ matchTypeParse     (ArrOf Bool) "[[]]"
                     assertEqual (Left $ TypeMismatch (1,2) LeafBool NodeObj)   $ matchTypeParse     (ArrOf Bool) "[{}]"
                     assertEqual (Left $ TypeMismatch (1,2) LeafBool LeafStr)   $ matchTypeParse     (ArrOf Bool) "[\"\"]"
                     assertEqual (Left $ TypeMismatch (1,2) LeafBool LeafNum)   $ matchTypeParse     (ArrOf Bool) "[0]"
                     assertEqual (Left $ TypeMismatch (1,2) LeafBool LeafNull)  $ matchTypeParse     (ArrOf Bool) "[null]"

                     assertEqual (Left $ TypeMismatch (1,2) LeafNull LeafTable) $ matchTypeParseWith (ArrOf Null) "[f()]" $ nullary "f" $ TableO (1,2) [] []
                     assertEqual (Left $ TypeMismatch (1,2) LeafNull LeafPlot)  $ matchTypeParseWith (ArrOf Null) "[f()]" $ nullary "f" $ PlotO  (1,2) [] []
                     assertEqual (Left $ TypeMismatch (1,2) LeafNull NodeArr)   $ matchTypeParse     (ArrOf Null) "[[]]"
                     assertEqual (Left $ TypeMismatch (1,2) LeafNull NodeObj)   $ matchTypeParse     (ArrOf Null) "[{}]"
                     assertEqual (Left $ TypeMismatch (1,2) LeafNull LeafStr)   $ matchTypeParse     (ArrOf Null) "[\"\"]"
                     assertEqual (Left $ TypeMismatch (1,2) LeafNull LeafNum)   $ matchTypeParse     (ArrOf Null) "[0]"
                     assertEqual (Left $ TypeMismatch (1,2) LeafNull LeafBool)  $ matchTypeParse     (ArrOf Null) "[false]"

test_ObjOfType = do  assertEqual (Left $ TypeMismatch (1,4) LeafTable LeafPlot)  $ matchTypeParseWith (ObjOf Table) "{x:f()}" $ nullary "f" $ PlotO  (1,4) [] []
                     assertEqual (Left $ TypeMismatch (1,4) LeafTable NodeArr)   $ matchTypeParse     (ObjOf Table) "{x:[]}"
                     assertEqual (Left $ TypeMismatch (1,4) LeafTable NodeObj)   $ matchTypeParse     (ObjOf Table) "{x:{}}"
                     assertEqual (Left $ TypeMismatch (1,4) LeafTable LeafStr)   $ matchTypeParse     (ObjOf Table) "{x:\"\"}"
                     assertEqual (Left $ TypeMismatch (1,4) LeafTable LeafNum)   $ matchTypeParse     (ObjOf Table) "{x:0}"
                     assertEqual (Left $ TypeMismatch (1,4) LeafTable LeafBool)  $ matchTypeParse     (ObjOf Table) "{x:false}"
                     assertEqual (Left $ TypeMismatch (1,4) LeafTable LeafNull)  $ matchTypeParse     (ObjOf Table) "{x:null}"

                     assertEqual (Left $ TypeMismatch (1,4) LeafPlot LeafTable) $ matchTypeParseWith (ObjOf Plot) "{x:f()}" $ nullary "f" $ TableO (1,4) [] []
                     assertEqual (Left $ TypeMismatch (1,4) LeafPlot NodeArr)   $ matchTypeParse     (ObjOf Plot) "{x:[]}"
                     assertEqual (Left $ TypeMismatch (1,4) LeafPlot NodeObj)   $ matchTypeParse     (ObjOf Plot) "{x:{}}"
                     assertEqual (Left $ TypeMismatch (1,4) LeafPlot LeafStr)   $ matchTypeParse     (ObjOf Plot) "{x:\"\"}"
                     assertEqual (Left $ TypeMismatch (1,4) LeafPlot LeafNum)   $ matchTypeParse     (ObjOf Plot) "{x:0}"
                     assertEqual (Left $ TypeMismatch (1,4) LeafPlot LeafBool)  $ matchTypeParse     (ObjOf Plot) "{x:false}"
                     assertEqual (Left $ TypeMismatch (1,4) LeafPlot LeafNull)  $ matchTypeParse     (ObjOf Plot) "{x:null}"

                     assertEqual (Left $ TypeMismatch (1,4)  NodeArr LeafTable) $ matchTypeParseWith (ObjOf arr) "{x:f()}" $ nullary "f" $ TableO (1,4) [] []
                     assertEqual (Left $ TypeMismatch (1,4)  NodeArr LeafPlot)  $ matchTypeParseWith (ObjOf arr) "{x:f()}" $ nullary "f" $ PlotO  (1,4) [] []
                     assertEqual (Left $ TypeMismatch (1,4)  NodeArr NodeObj)   $ matchTypeParse     (ObjOf arr) "{x:{}}"
                     assertEqual (Left $ TypeMismatch (1,4)  NodeArr LeafStr)   $ matchTypeParse     (ObjOf arr) "{x:\"\"}"
                     assertEqual (Left $ TypeMismatch (1,4)  NodeArr LeafNum)   $ matchTypeParse     (ObjOf arr) "{x:0}"
                     assertEqual (Left $ TypeMismatch (1,4)  NodeArr LeafBool)  $ matchTypeParse     (ObjOf arr) "{x:false}"
                     assertEqual (Left $ TypeMismatch (1,4)  NodeArr LeafNull)  $ matchTypeParse     (ObjOf arr) "{x:null}"

                     assertEqual (Left $ TypeMismatch (1,5)  NodeArr LeafTable)          $ matchTypeParseWith (ObjOf $ ArrOf arr) "{x:[f()]}" $ nullary "f" $ TableO (1,5) [] []
                     assertEqual (Left $ TypeMismatch (1,5)  NodeArr LeafPlot)           $ matchTypeParseWith (ObjOf $ ArrOf arr) "{x:[f()]}" $ nullary "f" $ PlotO  (1,5) [] []
                     assertEqual (Left $ TypeMismatch (1,5)  NodeArr NodeObj)            $ matchTypeParse     (ObjOf $ ArrOf arr) "{x:[{}]}"
                     assertEqual (Left $ TypeMismatch (1,5)  NodeArr LeafStr)            $ matchTypeParse     (ObjOf $ ArrOf arr) "{x:[\"\"]}"
                     assertEqual (Left $ TypeMismatch (1,5)  NodeArr LeafNum)            $ matchTypeParse     (ObjOf $ ArrOf arr) "{x:[0]}"
                     assertEqual (Left $ TypeMismatch (1,5)  NodeArr LeafBool)           $ matchTypeParse     (ObjOf $ ArrOf arr) "{x:[false]}"
                     assertEqual (Left $ TypeMismatch (1,5)  NodeArr LeafNull)           $ matchTypeParse     (ObjOf $ ArrOf arr) "{x:[null]}"

                     assertEqual (Left $ TypeMismatch (1,4)  NodeObj LeafTable) $ matchTypeParseWith (ObjOf obj) "{x:f()}" $ nullary "f" $ TableO (1,4) [] []
                     assertEqual (Left $ TypeMismatch (1,4)  NodeObj LeafPlot)  $ matchTypeParseWith (ObjOf obj) "{x:f()}" $ nullary "f" $ PlotO  (1,4) [] []
                     assertEqual (Left $ TypeMismatch (1,4)  NodeObj NodeArr)   $ matchTypeParse     (ObjOf obj) "{x:[]}"
                     assertEqual (Left $ TypeMismatch (1,4)  NodeObj LeafStr)   $ matchTypeParse     (ObjOf obj) "{x:\"\"}"
                     assertEqual (Left $ TypeMismatch (1,4)  NodeObj LeafNum)   $ matchTypeParse     (ObjOf obj) "{x:0}"
                     assertEqual (Left $ TypeMismatch (1,4)  NodeObj LeafBool)  $ matchTypeParse     (ObjOf obj) "{x:false}"
                     assertEqual (Left $ TypeMismatch (1,4)  NodeObj LeafNull)  $ matchTypeParse     (ObjOf obj) "{x:null}"

                     assertEqual (Left $ TypeMismatch (1,7)  NodeObj LeafTable)                $ matchTypeParseWith (ObjOf $ ObjOf obj) "{x:{x:f()}}" $ nullary "f" $ TableO (1,7) [] []
                     assertEqual (Left $ TypeMismatch (1,7)  NodeObj LeafPlot)                 $ matchTypeParseWith (ObjOf $ ObjOf obj) "{x:{x:f()}}" $ nullary "f" $ PlotO  (1,7) [] []
                     assertEqual (Left $ TypeMismatch (1,7)  NodeObj NodeArr)                  $ matchTypeParse     (ObjOf $ ObjOf obj) "{x:{x:[]}}"
                     assertEqual (Left $ TypeMismatch (1,7)  NodeObj LeafStr)                  $ matchTypeParse     (ObjOf $ ObjOf obj) "{x:{x:\"\"}}"
                     assertEqual (Left $ TypeMismatch (1,7)  NodeObj LeafNum)                  $ matchTypeParse     (ObjOf $ ObjOf obj) "{x:{x:0}}"
                     assertEqual (Left $ TypeMismatch (1,7)  NodeObj LeafBool)                 $ matchTypeParse     (ObjOf $ ObjOf obj) "{x:{x:false}}"
                     assertEqual (Left $ TypeMismatch (1,7)  NodeObj LeafNull)                 $ matchTypeParse     (ObjOf $ ObjOf obj) "{x:{x:null}}"

                     assertEqual (Left $ TypeMismatch (1,4) LeafStr LeafTable) $ matchTypeParseWith (ObjOf Str) "{x:f()}" $ nullary "f" $ TableO (1,4) [] []
                     assertEqual (Left $ TypeMismatch (1,4) LeafStr LeafPlot)  $ matchTypeParseWith (ObjOf Str) "{x:f()}" $ nullary "f" $ PlotO  (1,4) [] []
                     assertEqual (Left $ TypeMismatch (1,4) LeafStr NodeArr)   $ matchTypeParse     (ObjOf Str) "{x:[]}"
                     assertEqual (Left $ TypeMismatch (1,4) LeafStr NodeObj)   $ matchTypeParse     (ObjOf Str) "{x:{}}"
                     assertEqual (Left $ TypeMismatch (1,4) LeafStr LeafNum)   $ matchTypeParse     (ObjOf Str) "{x:0}"
                     assertEqual (Left $ TypeMismatch (1,4) LeafStr LeafBool)  $ matchTypeParse     (ObjOf Str) "{x:false}"
                     assertEqual (Left $ TypeMismatch (1,4) LeafStr LeafNull)  $ matchTypeParse     (ObjOf Str) "{x:null}"

                     assertEqual (Left $ TypeMismatch (1,4) LeafNum LeafTable) $ matchTypeParseWith (ObjOf Num) "{x:f()}" $ nullary "f" $ TableO (1,4) [] []
                     assertEqual (Left $ TypeMismatch (1,4) LeafNum LeafPlot)  $ matchTypeParseWith (ObjOf Num) "{x:f()}" $ nullary "f" $ PlotO  (1,4) [] []
                     assertEqual (Left $ TypeMismatch (1,4) LeafNum NodeArr)   $ matchTypeParse     (ObjOf Num) "{x:[]}"
                     assertEqual (Left $ TypeMismatch (1,4) LeafNum NodeObj)   $ matchTypeParse     (ObjOf Num) "{x:{}}"
                     assertEqual (Left $ TypeMismatch (1,4) LeafNum LeafStr)   $ matchTypeParse     (ObjOf Num) "{x:\"\"}"
                     assertEqual (Left $ TypeMismatch (1,4) LeafNum LeafBool)  $ matchTypeParse     (ObjOf Num) "{x:false}"
                     assertEqual (Left $ TypeMismatch (1,4) LeafNum LeafNull)  $ matchTypeParse     (ObjOf Num) "{x:null}"

                     assertEqual (Left $ TypeMismatch (1,4) LeafBool LeafTable) $ matchTypeParseWith (ObjOf Bool) "{x:f()}" $ nullary "f" $ TableO (1,4) [] []
                     assertEqual (Left $ TypeMismatch (1,4) LeafBool LeafPlot)  $ matchTypeParseWith (ObjOf Bool) "{x:f()}" $ nullary "f" $ PlotO  (1,4) [] []
                     assertEqual (Left $ TypeMismatch (1,4) LeafBool NodeArr)   $ matchTypeParse     (ObjOf Bool) "{x:[]}"
                     assertEqual (Left $ TypeMismatch (1,4) LeafBool NodeObj)   $ matchTypeParse     (ObjOf Bool) "{x:{}}"
                     assertEqual (Left $ TypeMismatch (1,4) LeafBool LeafStr)   $ matchTypeParse     (ObjOf Bool) "{x:\"\"}"
                     assertEqual (Left $ TypeMismatch (1,4) LeafBool LeafNum)   $ matchTypeParse     (ObjOf Bool) "{x:0}"
                     assertEqual (Left $ TypeMismatch (1,4) LeafBool LeafNull)  $ matchTypeParse     (ObjOf Bool) "{x:null}"

                     assertEqual (Left $ TypeMismatch (1,4) LeafNull  LeafTable) $ matchTypeParseWith (ObjOf Null) "{x:f()}" $ nullary "f" $ TableO (1,4) [] []
                     assertEqual (Left $ TypeMismatch (1,4) LeafNull  LeafPlot)  $ matchTypeParseWith (ObjOf Null) "{x:f()}" $ nullary "f" $ PlotO  (1,4) [] []
                     assertEqual (Left $ TypeMismatch (1,4) LeafNull  NodeArr)   $ matchTypeParse     (ObjOf Null) "{x:[]}"
                     assertEqual (Left $ TypeMismatch (1,4) LeafNull  NodeObj)   $ matchTypeParse     (ObjOf Null) "{x:{}}"
                     assertEqual (Left $ TypeMismatch (1,4) LeafNull  LeafStr)   $ matchTypeParse     (ObjOf Null) "{x:\"\"}"
                     assertEqual (Left $ TypeMismatch (1,4) LeafNull  LeafNum)   $ matchTypeParse     (ObjOf Null) "{x:0}"
                     assertEqual (Left $ TypeMismatch (1,4) LeafNull  LeafBool)  $ matchTypeParse     (ObjOf Null) "{x:false}"

test_OrType = do     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [LeafBool,LeafNull]) LeafTable) $ matchTypeParseWith (Or [Bool,Null]) "f()" $ nullary "f" $ TableO (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [LeafBool,LeafNull]) LeafPlot)  $ matchTypeParseWith (Or [Null,Bool]) "f()" $ nullary "f" $ PlotO (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [LeafBool,LeafNull]) NodeArr)   $ matchTypeParse     (Or [Bool,Null]) "[]"
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [LeafBool,LeafNull]) NodeObj)   $ matchTypeParse     (Or [Null,Bool]) "{}"
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [LeafBool,LeafNull]) LeafStr)   $ matchTypeParse     (Or [Bool,Null]) "\"\""
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [LeafBool,LeafNull]) LeafNum)   $ matchTypeParse     (Or [Null,Bool]) "0"

                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [LeafStr,LeafNum]) LeafTable) $ matchTypeParseWith (Or [Str,Num]) "f()" $ nullary "f" $ TableO (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [LeafStr,LeafNum]) LeafPlot)  $ matchTypeParseWith (Or [Num,Str]) "f()" $ nullary "f" $ PlotO (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [LeafStr,LeafNum]) NodeArr)   $ matchTypeParse     (Or [Str,Num]) "[]"
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [LeafStr,LeafNum]) NodeObj)   $ matchTypeParse     (Or [Num,Str]) "{}"
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [LeafStr,LeafNum]) LeafBool)  $ matchTypeParse     (Or [Str,Num]) "false"
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [LeafStr,LeafNum]) LeafNull)  $ matchTypeParse     (Or [Num,Str]) "null"

                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [NodeArr,NodeObj]) LeafTable) $ matchTypeParseWith (Or [arr,obj]) "f()" $ nullary "f" $ TableO (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [NodeArr,NodeObj]) LeafPlot)  $ matchTypeParseWith (Or [obj,arr]) "f()" $ nullary "f" $ PlotO (1,1) [] []
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [NodeArr,NodeObj]) LeafStr)   $ matchTypeParse     (Or [arr,obj]) "\"\""
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [NodeArr,NodeObj]) LeafNum)   $ matchTypeParse     (Or [obj,arr]) "0"
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [NodeArr,NodeObj]) LeafBool)  $ matchTypeParse     (Or [arr,obj]) "false"
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [NodeArr,NodeObj]) LeafNull)  $ matchTypeParse     (Or [obj,arr]) "null"

                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [LeafTable,LeafPlot])  NodeArr)  $ matchTypeParse     (Or [Table,Plot]) "[]"
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [LeafTable,LeafPlot])  NodeObj)  $ matchTypeParse     (Or [Plot,Table]) "{}"
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [LeafTable,LeafPlot]) LeafStr)   $ matchTypeParse     (Or [Table,Plot]) "\"\""
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [LeafTable,LeafPlot]) LeafNum)   $ matchTypeParse     (Or [Plot,Table]) "0"
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [LeafTable,LeafPlot]) LeafBool)  $ matchTypeParse     (Or [Table,Plot]) "false"
                     assertEqual (Left $ TypeMismatch (1,1) (NodeOr [LeafTable,LeafPlot]) LeafNull)  $ matchTypeParse     (Or [Plot,Table]) "null"

                    