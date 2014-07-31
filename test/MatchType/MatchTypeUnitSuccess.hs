{-# OPTIONS_GHC -F -pgmF htfpp #-}
module MatchType.MatchTypeUnitSuccess where

import Prelude hiding (any)

import Data.Type
import Eval.MatchType
import Test.Framework

import Marshall.MarshallUtils
import MatchType.MatchTypeUnitUtils

{-# ANN module "HLint: ignore Use camelCase" #-}

test_TableType =     assertEqual (Right $ mkTableC (1,1) [] [])                  $ matchTypeParseWith Table "f()" $ nullary "f" $ mkTableC (1,1) [] []
test_PlotType =      assertEqual (Right $ mkPlotC  (1,1) [] [])                  $ matchTypeParseWith Plot  "f()" $ nullary "f" $ mkPlotC  (1,1) [] []
test_ArrType =   do  assertEqual (Right $ mkArrC   (1,1) [])                     $ matchTypeParse     arr   "[]"
                     assertEqual (Right $ mkArrC   (1,1) [mkArrC (1,2) []])      $ matchTypeParse     (ArrOf arr) "[[]]"
                     assertEqual (Right $ mkArrC   (1,1) [mkTableC (1,2) [] []]) $ matchTypeParseWith arr "[f()]" $ nullary "f" $ mkTableC (1,2) [] []
                     assertEqual (Right $ mkArrC   (1,1) [mkPlotC (1,2) [] []])  $ matchTypeParseWith arr "[f()]" $ nullary "f" $ mkPlotC  (1,2) [] []
                     assertEqual (Right $ mkArrC   (1,1) [mkArrC (1,2) []])      $ matchTypeParse     arr "[[]]"
                     assertEqual (Right $ mkArrC   (1,1) [mkObjC (1,2) []])      $ matchTypeParse     arr "[{}]"
                     assertEqual (Right $ mkArrC   (1,1) [mkStrU (1,2) ""])      $ matchTypeParse     arr "[\"\"]"
                     assertEqual (Right $ mkArrC   (1,1) [mkNumU (1,2) 0])       $ matchTypeParse     arr "[0]"
                     assertEqual (Right $ mkArrC   (1,1) [mkBoolU (1,2) False])  $ matchTypeParse     arr "[false]"
                     assertEqual (Right $ mkArrC   (1,1) [mkNullU (1,2)])        $ matchTypeParse     arr "[null]"

test_ObjType = do    assertEqual (Right $ mkObjC (1,1) [])                           $ matchTypeParse     obj "{}"
                     assertEqual (Right $ mkObjC (1,1) [("x",mkObjC (1,4) [])])      $ matchTypeParse     (ObjOf obj) "{x:{}}"
                     assertEqual (Right $ mkObjC (1,1) [("x",mkTableC (1,4) [] [])]) $ matchTypeParseWith obj "{x:f()}" $ nullary "f" $ mkTableC (1,4) [] []
                     assertEqual (Right $ mkObjC (1,1) [("x",mkPlotC (1,4) [] [])])  $ matchTypeParseWith obj "{x:f()}" $ nullary "f" $ mkPlotC  (1,4) [] []
                     assertEqual (Right $ mkObjC (1,1) [("x",mkArrC (1,4) [])])      $ matchTypeParse     obj "{x:[]}"
                     assertEqual (Right $ mkObjC (1,1) [("x",mkObjC (1,4) [])])      $ matchTypeParse     obj "{x:{}}"
                     assertEqual (Right $ mkObjC (1,1) [("x",mkStrU (1,4) "")])      $ matchTypeParse     obj "{x:\"\"}"
                     assertEqual (Right $ mkObjC (1,1) [("x",mkNumU (1,4) 0)])       $ matchTypeParse     obj "{x:0}"
                     assertEqual (Right $ mkObjC (1,1) [("x",mkBoolU (1,4) False)])  $ matchTypeParse     obj "{x:false}"
                     assertEqual (Right $ mkObjC (1,1) [("x",mkNullU (1,4))])        $ matchTypeParse     obj "{x:null}"

test_StrType =       assertEqual (Right $ mkStrU   (1,1) "")    $ matchTypeParse     Str "\"\""
test_NumType =       assertEqual (Right $ mkNumU   (1,1) 0)     $ matchTypeParse     Num "0"
test_BoolType =      assertEqual (Right $ mkBoolU  (1,1) False) $ matchTypeParse     Bool "false"
test_NullType =      assertEqual (Right $ mkNullU  (1,1))       $ matchTypeParse     Null "null"
test_AnyType =  do   assertEqual (Right $ mkTableC (1,1) [] []) $ matchTypeParseWith any "f()" $ nullary "f" $ mkTableC (1,1) [] []
                     assertEqual (Right $ mkPlotC (1,1) [] [])  $ matchTypeParseWith any "f()" $ nullary "f" $ mkPlotC  (1,1) [] []
                     assertEqual (Right $ mkArrC   (1,1) [])    $ matchTypeParse     any "[]"
                     assertEqual (Right $ mkObjC   (1,1) [])    $ matchTypeParse     any "{}"
                     assertEqual (Right $ mkStrU   (1,1) "")    $ matchTypeParse     any "\"\""
                     assertEqual (Right $ mkNumU   (1,1) 0)     $ matchTypeParse     any "0"
                     assertEqual (Right $ mkBoolU  (1,1) False) $ matchTypeParse     any "false"
                     assertEqual (Right $ mkNullU  (1,1))       $ matchTypeParse     any "null"

                     assertEqual (Right $ mkArrC (1,1) [mkTableC (1,2) [] []]) $ matchTypeParseWith any "[f()]" $ nullary "f" $ mkTableC (1,2) [] []
                     assertEqual (Right $ mkArrC (1,1) [mkPlotC (1,2) [] []])  $ matchTypeParseWith any "[f()]" $ nullary "f" $ mkPlotC  (1,2) [] []
                     assertEqual (Right $ mkArrC (1,1) [mkArrC (1,2) []])      $ matchTypeParse     any "[[]]"
                     assertEqual (Right $ mkArrC (1,1) [mkObjC (1,2) []])      $ matchTypeParse     any "[{}]"
                     assertEqual (Right $ mkArrC (1,1) [mkStrU (1,2) ""])      $ matchTypeParse     any "[\"\"]"
                     assertEqual (Right $ mkArrC (1,1) [mkNumU (1,2) 0])       $ matchTypeParse     any "[0]"
                     assertEqual (Right $ mkArrC (1,1) [mkBoolU (1,2) False])  $ matchTypeParse     any "[false]"
                     assertEqual (Right $ mkArrC (1,1) [mkNullU (1,2)])        $ matchTypeParse     any "[null]"

                     assertEqual (Right $ mkObjC (1,1) [("x",mkTableC (1,4) [] [])]) $ matchTypeParseWith any "{x:f()}" $ nullary "f" $ mkTableC (1,4) [] []
                     assertEqual (Right $ mkObjC (1,1) [("x",mkPlotC (1,4) [] [])])  $ matchTypeParseWith any "{x:f()}" $ nullary "f" $ mkPlotC  (1,4) [] []
                     assertEqual (Right $ mkObjC (1,1) [("x",mkArrC (1,4) [])])      $ matchTypeParse     any "{x:[]}"
                     assertEqual (Right $ mkObjC (1,1) [("x",mkObjC (1,4) [])])      $ matchTypeParse     any "{x:{}}"
                     assertEqual (Right $ mkObjC (1,1) [("x",mkStrU (1,4) "")])      $ matchTypeParse     any "{x:\"\"}"
                     assertEqual (Right $ mkObjC (1,1) [("x",mkNumU (1,4) 0)])       $ matchTypeParse     any "{x:0}"
                     assertEqual (Right $ mkObjC (1,1) [("x",mkBoolU (1,4) False)])  $ matchTypeParse     any "{x:false}"
                     assertEqual (Right $ mkObjC (1,1) [("x",mkNullU (1,4))])        $ matchTypeParse     any "{x:null}"

test_ArrOfType = do  assertEqual (Right $ mkArrC (1,1) [mkTableC (1,2) [] []])             $ matchTypeParseWith (ArrOf Table) "[f()]" $ nullary "f" $ mkTableC (1,2) [] []
                     assertEqual (Right $ mkArrC (1,1) [mkPlotC  (1,2) [] []])             $ matchTypeParseWith (ArrOf Plot)  "[f()]" $ nullary "f" $ mkPlotC  (1,2) [] []
                     assertEqual (Right $ mkArrC (1,1) [mkArrC   (1,2) []])                $ matchTypeParse     (ArrOf arr) "[[]]"
                     assertEqual (Right $ mkArrC (1,1) [mkArrC   (1,2) [mkArrC (1,3) []]]) $ matchTypeParse     (ArrOf $ ArrOf arr) "[[[]]]"

                     assertEqual (Right $ mkArrC (1,1) [mkArrC (1,2) [mkTableC (1,3) [] []]]) $ matchTypeParseWith (ArrOf arr) "[[f()]]" $ nullary "f" $ mkTableC (1,3) [] []
                     assertEqual (Right $ mkArrC (1,1) [mkArrC (1,2) [mkPlotC (1,3) [] []]])  $ matchTypeParseWith (ArrOf arr) "[[f()]]" $ nullary "f" $ mkPlotC  (1,3) [] []
                     assertEqual (Right $ mkArrC (1,1) [mkArrC (1,2) [mkArrC (1,3) []]])      $ matchTypeParse     (ArrOf arr) "[[[]]]"
                     assertEqual (Right $ mkArrC (1,1) [mkArrC (1,2) [mkObjC (1,3) []]])      $ matchTypeParse     (ArrOf arr) "[[{}]]"
                     assertEqual (Right $ mkArrC (1,1) [mkArrC (1,2) [mkStrU (1,3) ""]])      $ matchTypeParse     (ArrOf arr) "[[\"\"]]"
                     assertEqual (Right $ mkArrC (1,1) [mkArrC (1,2) [mkNumU (1,3) 0]])       $ matchTypeParse     (ArrOf arr) "[[0]]"
                     assertEqual (Right $ mkArrC (1,1) [mkArrC (1,2) [mkBoolU (1,3) False]])  $ matchTypeParse     (ArrOf arr) "[[false]]"
                     assertEqual (Right $ mkArrC (1,1) [mkArrC (1,2) [mkNullU (1,3)]])        $ matchTypeParse     (ArrOf arr) "[[null]]"

                     assertEqual (Right $ mkArrC (1,1) [mkObjC (1,2) []])                           $ matchTypeParse     (ArrOf obj) "[{}]"
                     assertEqual (Right $ mkArrC (1,1) [mkObjC (1,2) [("x",mkObjC (1,5) [])]])      $ matchTypeParse     (ArrOf $ ObjOf obj) "[{x:{}}]"
                     assertEqual (Right $ mkArrC (1,1) [mkObjC (1,2) [("x",mkTableC (1,5) [] [])]]) $ matchTypeParseWith (ArrOf obj) "[{x:f()}]" $ nullary "f" $ mkTableC (1,5) [] []
                     assertEqual (Right $ mkArrC (1,1) [mkObjC (1,2) [("x",mkPlotC (1,5) [] [])]])  $ matchTypeParseWith (ArrOf obj) "[{x:f()}]" $ nullary "f" $ mkPlotC  (1,5) [] []
                     assertEqual (Right $ mkArrC (1,1) [mkObjC (1,2) [("x",mkArrC (1,5) [])]])      $ matchTypeParse     (ArrOf obj) "[{x:[]}]"
                     assertEqual (Right $ mkArrC (1,1) [mkObjC (1,2) [("x",mkObjC (1,5) [])]])      $ matchTypeParse     (ArrOf obj) "[{x:{}}]"
                     assertEqual (Right $ mkArrC (1,1) [mkObjC (1,2) [("x",mkStrU (1,5) "")]])      $ matchTypeParse     (ArrOf obj) "[{x:\"\"}]"
                     assertEqual (Right $ mkArrC (1,1) [mkObjC (1,2) [("x",mkNumU (1,5) 0)]])       $ matchTypeParse     (ArrOf obj) "[{x:0}]"
                     assertEqual (Right $ mkArrC (1,1) [mkObjC (1,2) [("x",mkBoolU (1,5) False)]])  $ matchTypeParse     (ArrOf obj) "[{x:false}]"
                     assertEqual (Right $ mkArrC (1,1) [mkObjC (1,2) [("x",mkNullU (1,5))]])        $ matchTypeParse     (ArrOf obj) "[{x:null}]"

                     assertEqual (Right $ mkArrC (1,1) [mkStrU (1,2) ""])     $ matchTypeParse (ArrOf Str) "[\"\"]"
                     assertEqual (Right $ mkArrC (1,1) [mkNumU (1,2) 0])      $ matchTypeParse (ArrOf Num) "[0]"
                     assertEqual (Right $ mkArrC (1,1) [mkBoolU (1,2) False]) $ matchTypeParse (ArrOf Bool) "[false]"
                     assertEqual (Right $ mkArrC (1,1) [mkNullU (1,2)])       $ matchTypeParse (ArrOf Null) "[null]"

test_ObjOfType = do  assertEqual (Right $ mkObjC (1,1) [("x",mkTableC (1,4) [] [])])             $ matchTypeParseWith (ObjOf Table) "{x:f()}" $ nullary "f" $ mkTableC (1,4) [] []
                     assertEqual (Right $ mkObjC (1,1) [("x",mkPlotC  (1,4) [] [])])             $ matchTypeParseWith (ObjOf Plot)  "{x:f()}" $ nullary "f" $ mkPlotC  (1,4) [] []
                     assertEqual (Right $ mkObjC (1,1) [("x",mkArrC   (1,4) [])])                $ matchTypeParse     (ObjOf arr) "{x:[]}"
                     assertEqual (Right $ mkObjC (1,1) [("x",mkArrC   (1,4) [mkArrC (1,5) []])]) $ matchTypeParse     (ObjOf $ ArrOf arr) "{x:[[]]}"

                     assertEqual (Right $ mkObjC (1,1) [("x",mkArrC (1,4) [mkTableC (1,5) [] []])]) $ matchTypeParseWith (ObjOf arr) "{x:[f()]}" $ nullary "f" $ mkTableC (1,5) [] []
                     assertEqual (Right $ mkObjC (1,1) [("x",mkArrC (1,4) [mkPlotC (1,5) [] []])])  $ matchTypeParseWith (ObjOf arr) "{x:[f()]}" $ nullary "f" $ mkPlotC  (1,5) [] []
                     assertEqual (Right $ mkObjC (1,1) [("x",mkArrC (1,4) [mkArrC (1,5) []])])      $ matchTypeParse     (ObjOf arr) "{x:[[]]}"
                     assertEqual (Right $ mkObjC (1,1) [("x",mkArrC (1,4) [mkObjC (1,5) []])])      $ matchTypeParse     (ObjOf arr) "{x:[{}]}"
                     assertEqual (Right $ mkObjC (1,1) [("x",mkArrC (1,4) [mkStrU (1,5) ""])])      $ matchTypeParse     (ObjOf arr) "{x:[\"\"]}"
                     assertEqual (Right $ mkObjC (1,1) [("x",mkArrC (1,4) [mkNumU (1,5) 0])])       $ matchTypeParse     (ObjOf arr) "{x:[0]}"
                     assertEqual (Right $ mkObjC (1,1) [("x",mkArrC (1,4) [mkBoolU (1,5) False])])  $ matchTypeParse     (ObjOf arr) "{x:[false]}"
                     assertEqual (Right $ mkObjC (1,1) [("x",mkArrC (1,4) [mkNullU (1,5)])])        $ matchTypeParse     (ObjOf arr) "{x:[null]}"

                     assertEqual (Right $ mkObjC (1,1) [("x",mkObjC (1,4) [])])                           $ matchTypeParse     (ObjOf obj) "{x:{}}"
                     assertEqual (Right $ mkObjC (1,1) [("x",mkObjC (1,4) [("x",mkObjC (1,7) [])])])      $ matchTypeParse     (ObjOf $ ObjOf obj) "{x:{x:{}}}"
                     assertEqual (Right $ mkObjC (1,1) [("x",mkObjC (1,4) [("x",mkTableC (1,7) [] [])])]) $ matchTypeParseWith (ObjOf obj) "{x:{x:f()}}" $ nullary "f" $ mkTableC (1,7) [] []
                     assertEqual (Right $ mkObjC (1,1) [("x",mkObjC (1,4) [("x",mkPlotC (1,7) [] [])])])  $ matchTypeParseWith (ObjOf obj) "{x:{x:f()}}" $ nullary "f" $ mkPlotC  (1,7) [] []
                     assertEqual (Right $ mkObjC (1,1) [("x",mkObjC (1,4) [("x",mkArrC (1,7) [])])])      $ matchTypeParse     (ObjOf obj) "{x:{x:[]}}"
                     assertEqual (Right $ mkObjC (1,1) [("x",mkObjC (1,4) [("x",mkObjC (1,7) [])])])      $ matchTypeParse     (ObjOf obj) "{x:{x:{}}}"
                     assertEqual (Right $ mkObjC (1,1) [("x",mkObjC (1,4) [("x",mkStrU (1,7) "")])])      $ matchTypeParse     (ObjOf obj) "{x:{x:\"\"}}"
                     assertEqual (Right $ mkObjC (1,1) [("x",mkObjC (1,4) [("x",mkNumU (1,7) 0)])])       $ matchTypeParse     (ObjOf obj) "{x:{x:0}}"
                     assertEqual (Right $ mkObjC (1,1) [("x",mkObjC (1,4) [("x",mkBoolU (1,7) False)])])  $ matchTypeParse     (ObjOf obj) "{x:{x:false}}"
                     assertEqual (Right $ mkObjC (1,1) [("x",mkObjC (1,4) [("x",mkNullU (1,7))])])        $ matchTypeParse     (ObjOf obj) "{x:{x:null}}"

                     assertEqual (Right $ mkObjC (1,1) [("x",mkStrU (1,4) "")])     $ matchTypeParse (ObjOf Str) "{x:\"\"}"
                     assertEqual (Right $ mkObjC (1,1) [("x",mkNumU (1,4) 0)])      $ matchTypeParse (ObjOf Num) "{x:0}"
                     assertEqual (Right $ mkObjC (1,1) [("x",mkBoolU (1,4) False)]) $ matchTypeParse (ObjOf Bool) "{x:false}"
                     assertEqual (Right $ mkObjC (1,1) [("x",mkNullU (1,4))])       $ matchTypeParse (ObjOf Null) "{x:null}"

test_OrType = do     assertEqual (Right $ mkTableC (1,1) [] []) $ matchTypeParseWith (Or [Table,Plot]) "f()" $ nullary "f" $ mkTableC (1,1) [] []
                     assertEqual (Right $ mkPlotC  (1,1) [] []) $ matchTypeParseWith (Or [Plot,Table]) "f()" $ nullary "f" $ mkPlotC  (1,1) [] []
                     assertEqual (Right $ mkArrC   (1,1) [])    $ matchTypeParse     (Or [arr,obj]) "[]"
                     assertEqual (Right $ mkObjC   (1,1) [])    $ matchTypeParse     (Or [obj,arr]) "{}"
                     assertEqual (Right $ mkStrU   (1,1) "")    $ matchTypeParse     (Or [Str,Num]) "\"\""
                     assertEqual (Right $ mkNumU   (1,1) 0)     $ matchTypeParse     (Or [Num,Str]) "0"
                     assertEqual (Right $ mkBoolU  (1,1) False) $ matchTypeParse     (Or [Bool,Null]) "false"
                     assertEqual (Right $ mkNullU  (1,1))       $ matchTypeParse     (Or [Null,Bool]) "null"

                    