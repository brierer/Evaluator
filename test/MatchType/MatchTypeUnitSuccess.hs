{-# OPTIONS_GHC -F -pgmF htfpp #-}
module MatchType.MatchTypeUnitSuccess where

import Prelude hiding (any)

import Data.ExpObj
import Data.Type
import Eval.MatchType
import Test.Framework

import MatchType.MatchTypeUnitUtils

{-# ANN module "HLint: ignore Use camelCase" #-}

test_TableType =     assertEqual (Right $ TableO (1,1) [] [])                $ matchTypeParseWith Table "f()" $ nullary "f" $ TableO (1,1) [] []
test_PlotType =      assertEqual (Right $ PlotO  (1,1) [] [])                $ matchTypeParseWith Plot  "f()" $ nullary "f" $ PlotO  (1,1) [] []
test_ArrType =   do  assertEqual (Right $ ArrO   (1,1) [])                   $ matchTypeParse     arr "[]"
                     assertEqual (Right $ ArrO   (1,1) [ArrO (1,2) []])      $ matchTypeParse     (ArrOf arr) "[[]]"
                     assertEqual (Right $ ArrO   (1,1) [TableO (1,2) [] []]) $ matchTypeParseWith arr "[f()]" $ nullary "f" $ TableO (1,2) [] []
                     assertEqual (Right $ ArrO   (1,1) [PlotO (1,2) [] []])  $ matchTypeParseWith arr "[f()]" $ nullary "f" $ PlotO  (1,2) [] []
                     assertEqual (Right $ ArrO   (1,1) [ArrO (1,2) []])      $ matchTypeParse     arr "[[]]"
                     assertEqual (Right $ ArrO   (1,1) [ObjO (1,2) []])      $ matchTypeParse     arr "[{}]"
                     assertEqual (Right $ ArrO   (1,1) [StrO (1,2) ""])      $ matchTypeParse     arr "[\"\"]"
                     assertEqual (Right $ ArrO   (1,1) [NumO (1,2) 0])       $ matchTypeParse     arr "[0]"
                     assertEqual (Right $ ArrO   (1,1) [BoolO (1,2) False])  $ matchTypeParse     arr "[false]"
                     assertEqual (Right $ ArrO   (1,1) [NullO (1,2)])        $ matchTypeParse     arr "[null]"

test_ObjType = do    assertEqual (Right $ ObjO (1,1) [])                         $ matchTypeParse     obj "{}"
                     assertEqual (Right $ ObjO (1,1) [("x",ObjO (1,4) [])])      $ matchTypeParse     (ObjOf obj) "{x:{}}"
                     assertEqual (Right $ ObjO (1,1) [("x",TableO (1,4) [] [])]) $ matchTypeParseWith obj "{x:f()}" $ nullary "f" $ TableO (1,4) [] []
                     assertEqual (Right $ ObjO (1,1) [("x",PlotO (1,4) [] [])])  $ matchTypeParseWith obj "{x:f()}" $ nullary "f" $ PlotO  (1,4) [] []
                     assertEqual (Right $ ObjO (1,1) [("x",ArrO (1,4) [])])      $ matchTypeParse     obj "{x:[]}"
                     assertEqual (Right $ ObjO (1,1) [("x",ObjO (1,4) [])])      $ matchTypeParse     obj "{x:{}}"
                     assertEqual (Right $ ObjO (1,1) [("x",StrO (1,4) "")])      $ matchTypeParse     obj "{x:\"\"}"
                     assertEqual (Right $ ObjO (1,1) [("x",NumO (1,4) 0)])       $ matchTypeParse     obj "{x:0}"
                     assertEqual (Right $ ObjO (1,1) [("x",BoolO (1,4) False)])  $ matchTypeParse     obj "{x:false}"
                     assertEqual (Right $ ObjO (1,1) [("x",NullO (1,4))])        $ matchTypeParse     obj "{x:null}"

test_StrType =       assertEqual (Right $ StrO   (1,1) "")    $ matchTypeParse     Str "\"\""
test_NumType =       assertEqual (Right $ NumO   (1,1) 0)     $ matchTypeParse     Num "0"
test_BoolType =      assertEqual (Right $ BoolO  (1,1) False) $ matchTypeParse     Bool "false"
test_NullType =      assertEqual (Right $ NullO  (1,1))       $ matchTypeParse     Null "null"
test_AnyType =  do   assertEqual (Right $ TableO (1,1) [] []) $ matchTypeParseWith any "f()" $ nullary "f" $ TableO (1,1) [] []
                     assertEqual (Right $ PlotO (1,1) [] [])  $ matchTypeParseWith any "f()" $ nullary "f" $ PlotO  (1,1) [] []
                     assertEqual (Right $ ArrO   (1,1) [])    $ matchTypeParse     any "[]"
                     assertEqual (Right $ ObjO   (1,1) [])    $ matchTypeParse     any "{}"
                     assertEqual (Right $ StrO   (1,1) "")    $ matchTypeParse     any "\"\""
                     assertEqual (Right $ NumO   (1,1) 0)     $ matchTypeParse     any "0"
                     assertEqual (Right $ BoolO  (1,1) False) $ matchTypeParse     any "false"
                     assertEqual (Right $ NullO  (1,1))       $ matchTypeParse     any "null"

                     assertEqual (Right $ ArrO (1,1) [TableO (1,2) [] []]) $ matchTypeParseWith any "[f()]" $ nullary "f" $ TableO (1,2) [] []
                     assertEqual (Right $ ArrO (1,1) [PlotO (1,2) [] []])  $ matchTypeParseWith any "[f()]" $ nullary "f" $ PlotO  (1,2) [] []
                     assertEqual (Right $ ArrO (1,1) [ArrO (1,2) []])      $ matchTypeParse     any "[[]]"
                     assertEqual (Right $ ArrO (1,1) [ObjO (1,2) []])      $ matchTypeParse     any "[{}]"
                     assertEqual (Right $ ArrO (1,1) [StrO (1,2) ""])      $ matchTypeParse     any "[\"\"]"
                     assertEqual (Right $ ArrO (1,1) [NumO (1,2) 0])       $ matchTypeParse     any "[0]"
                     assertEqual (Right $ ArrO (1,1) [BoolO (1,2) False])  $ matchTypeParse     any "[false]"
                     assertEqual (Right $ ArrO (1,1) [NullO (1,2)])        $ matchTypeParse     any "[null]"

                     assertEqual (Right $ ObjO (1,1) [("x",TableO (1,4) [] [])]) $ matchTypeParseWith any "{x:f()}" $ nullary "f" $ TableO (1,4) [] []
                     assertEqual (Right $ ObjO (1,1) [("x",PlotO (1,4) [] [])])  $ matchTypeParseWith any "{x:f()}" $ nullary "f" $ PlotO  (1,4) [] []
                     assertEqual (Right $ ObjO (1,1) [("x",ArrO (1,4) [])])      $ matchTypeParse     any "{x:[]}"
                     assertEqual (Right $ ObjO (1,1) [("x",ObjO (1,4) [])])      $ matchTypeParse     any "{x:{}}"
                     assertEqual (Right $ ObjO (1,1) [("x",StrO (1,4) "")])      $ matchTypeParse     any "{x:\"\"}"
                     assertEqual (Right $ ObjO (1,1) [("x",NumO (1,4) 0)])       $ matchTypeParse     any "{x:0}"
                     assertEqual (Right $ ObjO (1,1) [("x",BoolO (1,4) False)])  $ matchTypeParse     any "{x:false}"
                     assertEqual (Right $ ObjO (1,1) [("x",NullO (1,4))])        $ matchTypeParse     any "{x:null}"

test_ArrOfType = do  assertEqual (Right $ ArrO (1,1) [TableO (1,2) [] []])           $ matchTypeParseWith (ArrOf Table) "[f()]" $ nullary "f" $ TableO (1,2) [] []
                     assertEqual (Right $ ArrO (1,1) [PlotO  (1,2) [] []])           $ matchTypeParseWith (ArrOf Plot)  "[f()]" $ nullary "f" $ PlotO  (1,2) [] []
                     assertEqual (Right $ ArrO (1,1) [ArrO   (1,2) []])              $ matchTypeParse     (ArrOf arr) "[[]]"
                     assertEqual (Right $ ArrO (1,1) [ArrO   (1,2) [ArrO (1,3) []]]) $ matchTypeParse     (ArrOf $ ArrOf arr) "[[[]]]"

                     assertEqual (Right $ ArrO (1,1) [ArrO (1,2) [TableO (1,3) [] []]]) $ matchTypeParseWith (ArrOf arr) "[[f()]]" $ nullary "f" $ TableO (1,3) [] []
                     assertEqual (Right $ ArrO (1,1) [ArrO (1,2) [PlotO (1,3) [] []]])  $ matchTypeParseWith (ArrOf arr) "[[f()]]" $ nullary "f" $ PlotO  (1,3) [] []
                     assertEqual (Right $ ArrO (1,1) [ArrO (1,2) [ArrO (1,3) []]])      $ matchTypeParse     (ArrOf arr) "[[[]]]"
                     assertEqual (Right $ ArrO (1,1) [ArrO (1,2) [ObjO (1,3) []]])      $ matchTypeParse     (ArrOf arr) "[[{}]]"
                     assertEqual (Right $ ArrO (1,1) [ArrO (1,2) [StrO (1,3) ""]])      $ matchTypeParse     (ArrOf arr) "[[\"\"]]"
                     assertEqual (Right $ ArrO (1,1) [ArrO (1,2) [NumO (1,3) 0]])       $ matchTypeParse     (ArrOf arr) "[[0]]"
                     assertEqual (Right $ ArrO (1,1) [ArrO (1,2) [BoolO (1,3) False]])  $ matchTypeParse     (ArrOf arr) "[[false]]"
                     assertEqual (Right $ ArrO (1,1) [ArrO (1,2) [NullO (1,3)]])        $ matchTypeParse     (ArrOf arr) "[[null]]"

                     assertEqual (Right $ ArrO (1,1) [ObjO (1,2) []])                         $ matchTypeParse     (ArrOf obj) "[{}]"
                     assertEqual (Right $ ArrO (1,1) [ObjO (1,2) [("x",ObjO (1,5) [])]])      $ matchTypeParse     (ArrOf $ ObjOf obj) "[{x:{}}]"
                     assertEqual (Right $ ArrO (1,1) [ObjO (1,2) [("x",TableO (1,5) [] [])]]) $ matchTypeParseWith (ArrOf obj) "[{x:f()}]" $ nullary "f" $ TableO (1,5) [] []
                     assertEqual (Right $ ArrO (1,1) [ObjO (1,2) [("x",PlotO (1,5) [] [])]])  $ matchTypeParseWith (ArrOf obj) "[{x:f()}]" $ nullary "f" $ PlotO  (1,5) [] []
                     assertEqual (Right $ ArrO (1,1) [ObjO (1,2) [("x",ArrO (1,5) [])]])      $ matchTypeParse     (ArrOf obj) "[{x:[]}]"
                     assertEqual (Right $ ArrO (1,1) [ObjO (1,2) [("x",ObjO (1,5) [])]])      $ matchTypeParse     (ArrOf obj) "[{x:{}}]"
                     assertEqual (Right $ ArrO (1,1) [ObjO (1,2) [("x",StrO (1,5) "")]])      $ matchTypeParse     (ArrOf obj) "[{x:\"\"}]"
                     assertEqual (Right $ ArrO (1,1) [ObjO (1,2) [("x",NumO (1,5) 0)]])       $ matchTypeParse     (ArrOf obj) "[{x:0}]"
                     assertEqual (Right $ ArrO (1,1) [ObjO (1,2) [("x",BoolO (1,5) False)]])  $ matchTypeParse     (ArrOf obj) "[{x:false}]"
                     assertEqual (Right $ ArrO (1,1) [ObjO (1,2) [("x",NullO (1,5))]])        $ matchTypeParse     (ArrOf obj) "[{x:null}]"

                     assertEqual (Right $ ArrO (1,1) [StrO (1,2) ""])     $ matchTypeParse (ArrOf Str) "[\"\"]"
                     assertEqual (Right $ ArrO (1,1) [NumO (1,2) 0])      $ matchTypeParse (ArrOf Num) "[0]"
                     assertEqual (Right $ ArrO (1,1) [BoolO (1,2) False]) $ matchTypeParse (ArrOf Bool) "[false]"
                     assertEqual (Right $ ArrO (1,1) [NullO (1,2)])       $ matchTypeParse (ArrOf Null) "[null]"

test_ObjOfType = do  assertEqual (Right $ ObjO (1,1) [("x",TableO (1,4) [] [])])           $ matchTypeParseWith (ObjOf Table) "{x:f()}" $ nullary "f" $ TableO (1,4) [] []
                     assertEqual (Right $ ObjO (1,1) [("x",PlotO  (1,4) [] [])])           $ matchTypeParseWith (ObjOf Plot)  "{x:f()}" $ nullary "f" $ PlotO  (1,4) [] []
                     assertEqual (Right $ ObjO (1,1) [("x",ArrO   (1,4) [])])              $ matchTypeParse     (ObjOf arr) "{x:[]}"
                     assertEqual (Right $ ObjO (1,1) [("x",ArrO   (1,4) [ArrO (1,5) []])]) $ matchTypeParse     (ObjOf $ ArrOf arr) "{x:[[]]}"

                     assertEqual (Right $ ObjO (1,1) [("x",ArrO (1,4) [TableO (1,5) [] []])]) $ matchTypeParseWith (ObjOf arr) "{x:[f()]}" $ nullary "f" $ TableO (1,5) [] []
                     assertEqual (Right $ ObjO (1,1) [("x",ArrO (1,4) [PlotO (1,5) [] []])])  $ matchTypeParseWith (ObjOf arr) "{x:[f()]}" $ nullary "f" $ PlotO  (1,5) [] []
                     assertEqual (Right $ ObjO (1,1) [("x",ArrO (1,4) [ArrO (1,5) []])])      $ matchTypeParse     (ObjOf arr) "{x:[[]]}"
                     assertEqual (Right $ ObjO (1,1) [("x",ArrO (1,4) [ObjO (1,5) []])])      $ matchTypeParse     (ObjOf arr) "{x:[{}]}"
                     assertEqual (Right $ ObjO (1,1) [("x",ArrO (1,4) [StrO (1,5) ""])])      $ matchTypeParse     (ObjOf arr) "{x:[\"\"]}"
                     assertEqual (Right $ ObjO (1,1) [("x",ArrO (1,4) [NumO (1,5) 0])])       $ matchTypeParse     (ObjOf arr) "{x:[0]}"
                     assertEqual (Right $ ObjO (1,1) [("x",ArrO (1,4) [BoolO (1,5) False])])  $ matchTypeParse     (ObjOf arr) "{x:[false]}"
                     assertEqual (Right $ ObjO (1,1) [("x",ArrO (1,4) [NullO (1,5)])])        $ matchTypeParse     (ObjOf arr) "{x:[null]}"

                     assertEqual (Right $ ObjO (1,1) [("x",ObjO (1,4) [])])                         $ matchTypeParse     (ObjOf obj) "{x:{}}"
                     assertEqual (Right $ ObjO (1,1) [("x",ObjO (1,4) [("x",ObjO (1,7) [])])])      $ matchTypeParse     (ObjOf $ ObjOf obj) "{x:{x:{}}}"
                     assertEqual (Right $ ObjO (1,1) [("x",ObjO (1,4) [("x",TableO (1,7) [] [])])]) $ matchTypeParseWith (ObjOf obj) "{x:{x:f()}}" $ nullary "f" $ TableO (1,7) [] []
                     assertEqual (Right $ ObjO (1,1) [("x",ObjO (1,4) [("x",PlotO (1,7) [] [])])])  $ matchTypeParseWith (ObjOf obj) "{x:{x:f()}}" $ nullary "f" $ PlotO  (1,7) [] []
                     assertEqual (Right $ ObjO (1,1) [("x",ObjO (1,4) [("x",ArrO (1,7) [])])])      $ matchTypeParse     (ObjOf obj) "{x:{x:[]}}"
                     assertEqual (Right $ ObjO (1,1) [("x",ObjO (1,4) [("x",ObjO (1,7) [])])])      $ matchTypeParse     (ObjOf obj) "{x:{x:{}}}"
                     assertEqual (Right $ ObjO (1,1) [("x",ObjO (1,4) [("x",StrO (1,7) "")])])      $ matchTypeParse     (ObjOf obj) "{x:{x:\"\"}}"
                     assertEqual (Right $ ObjO (1,1) [("x",ObjO (1,4) [("x",NumO (1,7) 0)])])       $ matchTypeParse     (ObjOf obj) "{x:{x:0}}"
                     assertEqual (Right $ ObjO (1,1) [("x",ObjO (1,4) [("x",BoolO (1,7) False)])])  $ matchTypeParse     (ObjOf obj) "{x:{x:false}}"
                     assertEqual (Right $ ObjO (1,1) [("x",ObjO (1,4) [("x",NullO (1,7))])])        $ matchTypeParse     (ObjOf obj) "{x:{x:null}}"

                     assertEqual (Right $ ObjO (1,1) [("x",StrO (1,4) "")])     $ matchTypeParse (ObjOf Str) "{x:\"\"}"
                     assertEqual (Right $ ObjO (1,1) [("x",NumO (1,4) 0)])      $ matchTypeParse (ObjOf Num) "{x:0}"
                     assertEqual (Right $ ObjO (1,1) [("x",BoolO (1,4) False)]) $ matchTypeParse (ObjOf Bool) "{x:false}"
                     assertEqual (Right $ ObjO (1,1) [("x",NullO (1,4))])       $ matchTypeParse (ObjOf Null) "{x:null}"

test_OrType = do     assertEqual (Right $ TableO (1,1) [] []) $ matchTypeParseWith (Or [Table,Plot]) "f()" $ nullary "f" $ TableO (1,1) [] []
                     assertEqual (Right $ PlotO  (1,1) [] []) $ matchTypeParseWith (Or [Plot,Table]) "f()" $ nullary "f" $ PlotO  (1,1) [] []
                     assertEqual (Right $ ArrO   (1,1) [])    $ matchTypeParse     (Or [arr,obj]) "[]"
                     assertEqual (Right $ ObjO   (1,1) [])    $ matchTypeParse     (Or [obj,arr]) "{}"
                     assertEqual (Right $ StrO   (1,1) "")    $ matchTypeParse     (Or [Str,Num]) "\"\""
                     assertEqual (Right $ NumO   (1,1) 0)     $ matchTypeParse     (Or [Num,Str]) "0"
                     assertEqual (Right $ BoolO  (1,1) False) $ matchTypeParse     (Or [Bool,Null]) "false"
                     assertEqual (Right $ NullO  (1,1))       $ matchTypeParse     (Or [Null,Bool]) "null"

                    