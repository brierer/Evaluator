{-# OPTIONS_GHC -F -pgmF htfpp #-}
module MatchType.MatchTypeUnitSuccess where

import Prelude hiding (any)

import Data.Type
import Eval.MatchType
import Test.Framework

import Marshall.MarshallUtils
import MatchType.MatchTypeUnitUtils

{-# ANN module "HLint: ignore Use camelCase" #-}

test_TableType =     assertEqual (Right $ mkTableO (1,1) [] [])                  $ matchTypeParseWith Table "f()" $ nullary "f" $ mkTableO (1,1) [] []
test_PlotType =      assertEqual (Right $ mkPlotO  (1,1) [] [])                  $ matchTypeParseWith Plot  "f()" $ nullary "f" $ mkPlotO  (1,1) [] []
test_ArrType =   do  assertEqual (Right $ mkArrO   (1,1) [])                     $ matchTypeParse     arr   "[]"
                     assertEqual (Right $ mkArrO   (1,1) [mkArrO (1,2) []])      $ matchTypeParse     (ArrOf arr) "[[]]"
                     assertEqual (Right $ mkArrO   (1,1) [mkTableO (1,2) [] []]) $ matchTypeParseWith arr "[f()]" $ nullary "f" $ mkTableO (1,2) [] []
                     assertEqual (Right $ mkArrO   (1,1) [mkPlotO (1,2) [] []])  $ matchTypeParseWith arr "[f()]" $ nullary "f" $ mkPlotO  (1,2) [] []
                     assertEqual (Right $ mkArrO   (1,1) [mkArrO (1,2) []])      $ matchTypeParse     arr "[[]]"
                     assertEqual (Right $ mkArrO   (1,1) [mkObjO (1,2) []])      $ matchTypeParse     arr "[{}]"
                     assertEqual (Right $ mkArrO   (1,1) [mkStrO (1,2) ""])      $ matchTypeParse     arr "[\"\"]"
                     assertEqual (Right $ mkArrO   (1,1) [mkNumO (1,2) 0])       $ matchTypeParse     arr "[0]"
                     assertEqual (Right $ mkArrO   (1,1) [mkBoolO (1,2) False])  $ matchTypeParse     arr "[false]"
                     assertEqual (Right $ mkArrO   (1,1) [mkNullO (1,2)])        $ matchTypeParse     arr "[null]"

test_ObjType = do    assertEqual (Right $ mkObjO (1,1) [])                           $ matchTypeParse     obj "{}"
                     assertEqual (Right $ mkObjO (1,1) [("x",mkObjO (1,4) [])])      $ matchTypeParse     (ObjOf obj) "{x:{}}"
                     assertEqual (Right $ mkObjO (1,1) [("x",mkTableO (1,4) [] [])]) $ matchTypeParseWith obj "{x:f()}" $ nullary "f" $ mkTableO (1,4) [] []
                     assertEqual (Right $ mkObjO (1,1) [("x",mkPlotO (1,4) [] [])])  $ matchTypeParseWith obj "{x:f()}" $ nullary "f" $ mkPlotO  (1,4) [] []
                     assertEqual (Right $ mkObjO (1,1) [("x",mkArrO (1,4) [])])      $ matchTypeParse     obj "{x:[]}"
                     assertEqual (Right $ mkObjO (1,1) [("x",mkObjO (1,4) [])])      $ matchTypeParse     obj "{x:{}}"
                     assertEqual (Right $ mkObjO (1,1) [("x",mkStrO (1,4) "")])      $ matchTypeParse     obj "{x:\"\"}"
                     assertEqual (Right $ mkObjO (1,1) [("x",mkNumO (1,4) 0)])       $ matchTypeParse     obj "{x:0}"
                     assertEqual (Right $ mkObjO (1,1) [("x",mkBoolO (1,4) False)])  $ matchTypeParse     obj "{x:false}"
                     assertEqual (Right $ mkObjO (1,1) [("x",mkNullO (1,4))])        $ matchTypeParse     obj "{x:null}"

test_StrType =       assertEqual (Right $ mkStrO   (1,1) "")    $ matchTypeParse     Str "\"\""
test_NumType =       assertEqual (Right $ mkNumO   (1,1) 0)     $ matchTypeParse     Num "0"
test_BoolType =      assertEqual (Right $ mkBoolO  (1,1) False) $ matchTypeParse     Bool "false"
test_NullType =      assertEqual (Right $ mkNullO  (1,1))       $ matchTypeParse     Null "null"
test_AnyType =  do   assertEqual (Right $ mkTableO (1,1) [] []) $ matchTypeParseWith any "f()" $ nullary "f" $ mkTableO (1,1) [] []
                     assertEqual (Right $ mkPlotO (1,1) [] [])  $ matchTypeParseWith any "f()" $ nullary "f" $ mkPlotO  (1,1) [] []
                     assertEqual (Right $ mkArrO   (1,1) [])    $ matchTypeParse     any "[]"
                     assertEqual (Right $ mkObjO   (1,1) [])    $ matchTypeParse     any "{}"
                     assertEqual (Right $ mkStrO   (1,1) "")    $ matchTypeParse     any "\"\""
                     assertEqual (Right $ mkNumO   (1,1) 0)     $ matchTypeParse     any "0"
                     assertEqual (Right $ mkBoolO  (1,1) False) $ matchTypeParse     any "false"
                     assertEqual (Right $ mkNullO  (1,1))       $ matchTypeParse     any "null"

                     assertEqual (Right $ mkArrO (1,1) [mkTableO (1,2) [] []]) $ matchTypeParseWith any "[f()]" $ nullary "f" $ mkTableO (1,2) [] []
                     assertEqual (Right $ mkArrO (1,1) [mkPlotO (1,2) [] []])  $ matchTypeParseWith any "[f()]" $ nullary "f" $ mkPlotO  (1,2) [] []
                     assertEqual (Right $ mkArrO (1,1) [mkArrO (1,2) []])      $ matchTypeParse     any "[[]]"
                     assertEqual (Right $ mkArrO (1,1) [mkObjO (1,2) []])      $ matchTypeParse     any "[{}]"
                     assertEqual (Right $ mkArrO (1,1) [mkStrO (1,2) ""])      $ matchTypeParse     any "[\"\"]"
                     assertEqual (Right $ mkArrO (1,1) [mkNumO (1,2) 0])       $ matchTypeParse     any "[0]"
                     assertEqual (Right $ mkArrO (1,1) [mkBoolO (1,2) False])  $ matchTypeParse     any "[false]"
                     assertEqual (Right $ mkArrO (1,1) [mkNullO (1,2)])        $ matchTypeParse     any "[null]"

                     assertEqual (Right $ mkObjO (1,1) [("x",mkTableO (1,4) [] [])]) $ matchTypeParseWith any "{x:f()}" $ nullary "f" $ mkTableO (1,4) [] []
                     assertEqual (Right $ mkObjO (1,1) [("x",mkPlotO (1,4) [] [])])  $ matchTypeParseWith any "{x:f()}" $ nullary "f" $ mkPlotO  (1,4) [] []
                     assertEqual (Right $ mkObjO (1,1) [("x",mkArrO (1,4) [])])      $ matchTypeParse     any "{x:[]}"
                     assertEqual (Right $ mkObjO (1,1) [("x",mkObjO (1,4) [])])      $ matchTypeParse     any "{x:{}}"
                     assertEqual (Right $ mkObjO (1,1) [("x",mkStrO (1,4) "")])      $ matchTypeParse     any "{x:\"\"}"
                     assertEqual (Right $ mkObjO (1,1) [("x",mkNumO (1,4) 0)])       $ matchTypeParse     any "{x:0}"
                     assertEqual (Right $ mkObjO (1,1) [("x",mkBoolO (1,4) False)])  $ matchTypeParse     any "{x:false}"
                     assertEqual (Right $ mkObjO (1,1) [("x",mkNullO (1,4))])        $ matchTypeParse     any "{x:null}"

test_ArrOfType = do  assertEqual (Right $ mkArrO (1,1) [mkTableO (1,2) [] []])             $ matchTypeParseWith (ArrOf Table) "[f()]" $ nullary "f" $ mkTableO (1,2) [] []
                     assertEqual (Right $ mkArrO (1,1) [mkPlotO  (1,2) [] []])             $ matchTypeParseWith (ArrOf Plot)  "[f()]" $ nullary "f" $ mkPlotO  (1,2) [] []
                     assertEqual (Right $ mkArrO (1,1) [mkArrO   (1,2) []])                $ matchTypeParse     (ArrOf arr) "[[]]"
                     assertEqual (Right $ mkArrO (1,1) [mkArrO   (1,2) [mkArrO (1,3) []]]) $ matchTypeParse     (ArrOf $ ArrOf arr) "[[[]]]"

                     assertEqual (Right $ mkArrO (1,1) [mkArrO (1,2) [mkTableO (1,3) [] []]]) $ matchTypeParseWith (ArrOf arr) "[[f()]]" $ nullary "f" $ mkTableO (1,3) [] []
                     assertEqual (Right $ mkArrO (1,1) [mkArrO (1,2) [mkPlotO (1,3) [] []]])  $ matchTypeParseWith (ArrOf arr) "[[f()]]" $ nullary "f" $ mkPlotO  (1,3) [] []
                     assertEqual (Right $ mkArrO (1,1) [mkArrO (1,2) [mkArrO (1,3) []]])      $ matchTypeParse     (ArrOf arr) "[[[]]]"
                     assertEqual (Right $ mkArrO (1,1) [mkArrO (1,2) [mkObjO (1,3) []]])      $ matchTypeParse     (ArrOf arr) "[[{}]]"
                     assertEqual (Right $ mkArrO (1,1) [mkArrO (1,2) [mkStrO (1,3) ""]])      $ matchTypeParse     (ArrOf arr) "[[\"\"]]"
                     assertEqual (Right $ mkArrO (1,1) [mkArrO (1,2) [mkNumO (1,3) 0]])       $ matchTypeParse     (ArrOf arr) "[[0]]"
                     assertEqual (Right $ mkArrO (1,1) [mkArrO (1,2) [mkBoolO (1,3) False]])  $ matchTypeParse     (ArrOf arr) "[[false]]"
                     assertEqual (Right $ mkArrO (1,1) [mkArrO (1,2) [mkNullO (1,3)]])        $ matchTypeParse     (ArrOf arr) "[[null]]"

                     assertEqual (Right $ mkArrO (1,1) [mkObjO (1,2) []])                           $ matchTypeParse     (ArrOf obj) "[{}]"
                     assertEqual (Right $ mkArrO (1,1) [mkObjO (1,2) [("x",mkObjO (1,5) [])]])      $ matchTypeParse     (ArrOf $ ObjOf obj) "[{x:{}}]"
                     assertEqual (Right $ mkArrO (1,1) [mkObjO (1,2) [("x",mkTableO (1,5) [] [])]]) $ matchTypeParseWith (ArrOf obj) "[{x:f()}]" $ nullary "f" $ mkTableO (1,5) [] []
                     assertEqual (Right $ mkArrO (1,1) [mkObjO (1,2) [("x",mkPlotO (1,5) [] [])]])  $ matchTypeParseWith (ArrOf obj) "[{x:f()}]" $ nullary "f" $ mkPlotO  (1,5) [] []
                     assertEqual (Right $ mkArrO (1,1) [mkObjO (1,2) [("x",mkArrO (1,5) [])]])      $ matchTypeParse     (ArrOf obj) "[{x:[]}]"
                     assertEqual (Right $ mkArrO (1,1) [mkObjO (1,2) [("x",mkObjO (1,5) [])]])      $ matchTypeParse     (ArrOf obj) "[{x:{}}]"
                     assertEqual (Right $ mkArrO (1,1) [mkObjO (1,2) [("x",mkStrO (1,5) "")]])      $ matchTypeParse     (ArrOf obj) "[{x:\"\"}]"
                     assertEqual (Right $ mkArrO (1,1) [mkObjO (1,2) [("x",mkNumO (1,5) 0)]])       $ matchTypeParse     (ArrOf obj) "[{x:0}]"
                     assertEqual (Right $ mkArrO (1,1) [mkObjO (1,2) [("x",mkBoolO (1,5) False)]])  $ matchTypeParse     (ArrOf obj) "[{x:false}]"
                     assertEqual (Right $ mkArrO (1,1) [mkObjO (1,2) [("x",mkNullO (1,5))]])        $ matchTypeParse     (ArrOf obj) "[{x:null}]"

                     assertEqual (Right $ mkArrO (1,1) [mkStrO (1,2) ""])     $ matchTypeParse (ArrOf Str) "[\"\"]"
                     assertEqual (Right $ mkArrO (1,1) [mkNumO (1,2) 0])      $ matchTypeParse (ArrOf Num) "[0]"
                     assertEqual (Right $ mkArrO (1,1) [mkBoolO (1,2) False]) $ matchTypeParse (ArrOf Bool) "[false]"
                     assertEqual (Right $ mkArrO (1,1) [mkNullO (1,2)])       $ matchTypeParse (ArrOf Null) "[null]"

test_ObjOfType = do  assertEqual (Right $ mkObjO (1,1) [("x",mkTableO (1,4) [] [])])             $ matchTypeParseWith (ObjOf Table) "{x:f()}" $ nullary "f" $ mkTableO (1,4) [] []
                     assertEqual (Right $ mkObjO (1,1) [("x",mkPlotO  (1,4) [] [])])             $ matchTypeParseWith (ObjOf Plot)  "{x:f()}" $ nullary "f" $ mkPlotO  (1,4) [] []
                     assertEqual (Right $ mkObjO (1,1) [("x",mkArrO   (1,4) [])])                $ matchTypeParse     (ObjOf arr) "{x:[]}"
                     assertEqual (Right $ mkObjO (1,1) [("x",mkArrO   (1,4) [mkArrO (1,5) []])]) $ matchTypeParse     (ObjOf $ ArrOf arr) "{x:[[]]}"

                     assertEqual (Right $ mkObjO (1,1) [("x",mkArrO (1,4) [mkTableO (1,5) [] []])]) $ matchTypeParseWith (ObjOf arr) "{x:[f()]}" $ nullary "f" $ mkTableO (1,5) [] []
                     assertEqual (Right $ mkObjO (1,1) [("x",mkArrO (1,4) [mkPlotO (1,5) [] []])])  $ matchTypeParseWith (ObjOf arr) "{x:[f()]}" $ nullary "f" $ mkPlotO  (1,5) [] []
                     assertEqual (Right $ mkObjO (1,1) [("x",mkArrO (1,4) [mkArrO (1,5) []])])      $ matchTypeParse     (ObjOf arr) "{x:[[]]}"
                     assertEqual (Right $ mkObjO (1,1) [("x",mkArrO (1,4) [mkObjO (1,5) []])])      $ matchTypeParse     (ObjOf arr) "{x:[{}]}"
                     assertEqual (Right $ mkObjO (1,1) [("x",mkArrO (1,4) [mkStrO (1,5) ""])])      $ matchTypeParse     (ObjOf arr) "{x:[\"\"]}"
                     assertEqual (Right $ mkObjO (1,1) [("x",mkArrO (1,4) [mkNumO (1,5) 0])])       $ matchTypeParse     (ObjOf arr) "{x:[0]}"
                     assertEqual (Right $ mkObjO (1,1) [("x",mkArrO (1,4) [mkBoolO (1,5) False])])  $ matchTypeParse     (ObjOf arr) "{x:[false]}"
                     assertEqual (Right $ mkObjO (1,1) [("x",mkArrO (1,4) [mkNullO (1,5)])])        $ matchTypeParse     (ObjOf arr) "{x:[null]}"

                     assertEqual (Right $ mkObjO (1,1) [("x",mkObjO (1,4) [])])                           $ matchTypeParse     (ObjOf obj) "{x:{}}"
                     assertEqual (Right $ mkObjO (1,1) [("x",mkObjO (1,4) [("x",mkObjO (1,7) [])])])      $ matchTypeParse     (ObjOf $ ObjOf obj) "{x:{x:{}}}"
                     assertEqual (Right $ mkObjO (1,1) [("x",mkObjO (1,4) [("x",mkTableO (1,7) [] [])])]) $ matchTypeParseWith (ObjOf obj) "{x:{x:f()}}" $ nullary "f" $ mkTableO (1,7) [] []
                     assertEqual (Right $ mkObjO (1,1) [("x",mkObjO (1,4) [("x",mkPlotO (1,7) [] [])])])  $ matchTypeParseWith (ObjOf obj) "{x:{x:f()}}" $ nullary "f" $ mkPlotO  (1,7) [] []
                     assertEqual (Right $ mkObjO (1,1) [("x",mkObjO (1,4) [("x",mkArrO (1,7) [])])])      $ matchTypeParse     (ObjOf obj) "{x:{x:[]}}"
                     assertEqual (Right $ mkObjO (1,1) [("x",mkObjO (1,4) [("x",mkObjO (1,7) [])])])      $ matchTypeParse     (ObjOf obj) "{x:{x:{}}}"
                     assertEqual (Right $ mkObjO (1,1) [("x",mkObjO (1,4) [("x",mkStrO (1,7) "")])])      $ matchTypeParse     (ObjOf obj) "{x:{x:\"\"}}"
                     assertEqual (Right $ mkObjO (1,1) [("x",mkObjO (1,4) [("x",mkNumO (1,7) 0)])])       $ matchTypeParse     (ObjOf obj) "{x:{x:0}}"
                     assertEqual (Right $ mkObjO (1,1) [("x",mkObjO (1,4) [("x",mkBoolO (1,7) False)])])  $ matchTypeParse     (ObjOf obj) "{x:{x:false}}"
                     assertEqual (Right $ mkObjO (1,1) [("x",mkObjO (1,4) [("x",mkNullO (1,7))])])        $ matchTypeParse     (ObjOf obj) "{x:{x:null}}"

                     assertEqual (Right $ mkObjO (1,1) [("x",mkStrO (1,4) "")])     $ matchTypeParse (ObjOf Str) "{x:\"\"}"
                     assertEqual (Right $ mkObjO (1,1) [("x",mkNumO (1,4) 0)])      $ matchTypeParse (ObjOf Num) "{x:0}"
                     assertEqual (Right $ mkObjO (1,1) [("x",mkBoolO (1,4) False)]) $ matchTypeParse (ObjOf Bool) "{x:false}"
                     assertEqual (Right $ mkObjO (1,1) [("x",mkNullO (1,4))])       $ matchTypeParse (ObjOf Null) "{x:null}"

test_OrType = do     assertEqual (Right $ mkTableO (1,1) [] []) $ matchTypeParseWith (Or [Table,Plot]) "f()" $ nullary "f" $ mkTableO (1,1) [] []
                     assertEqual (Right $ mkPlotO  (1,1) [] []) $ matchTypeParseWith (Or [Plot,Table]) "f()" $ nullary "f" $ mkPlotO  (1,1) [] []
                     assertEqual (Right $ mkArrO   (1,1) [])    $ matchTypeParse     (Or [arr,obj]) "[]"
                     assertEqual (Right $ mkObjO   (1,1) [])    $ matchTypeParse     (Or [obj,arr]) "{}"
                     assertEqual (Right $ mkStrO   (1,1) "")    $ matchTypeParse     (Or [Str,Num]) "\"\""
                     assertEqual (Right $ mkNumO   (1,1) 0)     $ matchTypeParse     (Or [Num,Str]) "0"
                     assertEqual (Right $ mkBoolO  (1,1) False) $ matchTypeParse     (Or [Bool,Null]) "false"
                     assertEqual (Right $ mkNullO  (1,1))       $ matchTypeParse     (Or [Null,Bool]) "null"

                    