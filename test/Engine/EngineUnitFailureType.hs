{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Engine.EngineUnitFailureType where

import Data.EvalError
import Data.ExpObj
import Data.Type
import Test.Framework

import Engine.EngineUnitFailureUtils
import Engine.EngineUnitUtils
import Marshall.MarshallUtils
import MatchType.MatchTypeUnitUtils

{-# ANN module "HLint: ignore Use camelCase" #-}

test_Show = do   assertEqual(leafTypeMismatch (1,6) NodeArr LeafTable) $ runWith "show(f())" [("f",TableO (1,6) [] [])]
                 assertEqual(leafTypeMismatch (1,6) NodeArr LeafPlot)  $ runWith "show(f())" [("f",PlotO  (1,6) [] [])]
                 assertEqual(leafTypeMismatch (1,6) NodeArr NodeObj)   $ run     "show({})"
                 assertEqual(leafTypeMismatch (1,6) NodeArr LeafStr)   $ run     "show(\"\")"
                 assertEqual(leafTypeMismatch (1,6) NodeArr LeafNum)   $ run     "show(0)"
                 assertEqual(leafTypeMismatch (1,6) NodeArr LeafBool)  $ run     "show(false)"
                 assertEqual(leafTypeMismatch (1,6) NodeArr LeafNull)  $ run     "show(null)"

                 assertEqual(leafTypeMismatch (1,7) (NodeOr showable) NodeArr)  $ run "show([[]])"
                 assertEqual(leafTypeMismatch (1,7) (NodeOr showable) NodeObj)  $ run "show([{}])"
                 assertEqual(leafTypeMismatch (1,7) (NodeOr showable) LeafStr)  $ run "show([\"\"])"
                 assertEqual(leafTypeMismatch (1,7) (NodeOr showable) LeafNum)  $ run "show([0])"
                 assertEqual(leafTypeMismatch (1,7) (NodeOr showable) LeafBool) $ run "show([false])"
                 assertEqual(leafTypeMismatch (1,7) (NodeOr showable) LeafNull) $ run "show([null])"

test_Multi = do  assertEqual(leafTypeMismatch (1,7) NodeArr LeafTable) $ runWith "multi(f())" [("f",TableO (1,7) [] [])]
                 assertEqual(leafTypeMismatch (1,7) NodeArr LeafPlot)  $ runWith "multi(f())" [("f",PlotO  (1,7) [] [])]
                 assertEqual(leafTypeMismatch (1,7) NodeArr NodeObj)   $ run     "multi({})"
                 assertEqual(leafTypeMismatch (1,7) NodeArr LeafStr)   $ run     "multi(\"\")"
                 assertEqual(leafTypeMismatch (1,7) NodeArr LeafNum)   $ run     "multi(0)"
                 assertEqual(leafTypeMismatch (1,7) NodeArr LeafBool)  $ run     "multi(false)"
                 assertEqual(leafTypeMismatch (1,7) NodeArr LeafNull)  $ run     "multi(null)"

                 assertEqual(leafTypeMismatch (1,8) (NodeOr atom) LeafTable) $ runWith "multi([f()])" [("f",TableO (1,8) [] [])]
                 assertEqual(leafTypeMismatch (1,8) (NodeOr atom) LeafPlot)  $ runWith "multi([f()])" [("f",PlotO  (1,8) [] [])]
                 assertEqual(leafTypeMismatch (1,8) (NodeOr atom) NodeArr)   $ run     "multi([[]])"
                 assertEqual(leafTypeMismatch (1,8) (NodeOr atom) NodeObj)   $ run     "multi([{}])"

test_Mean = do   assertEqual(leafTypeMismatch (1,6) NodeArr LeafTable) $ runWith "mean(f())" [("f",TableO (1,6) [] [])]
                 assertEqual(leafTypeMismatch (1,6) NodeArr LeafPlot)  $ runWith "mean(f())" [("f",PlotO  (1,6) [] [])]
                 assertEqual(leafTypeMismatch (1,6) NodeArr NodeObj)   $ run     "mean({})"
                 assertEqual(leafTypeMismatch (1,6) NodeArr LeafStr)   $ run     "mean(\"\")"
                 assertEqual(leafTypeMismatch (1,6) NodeArr LeafNum)   $ run     "mean(0)"
                 assertEqual(leafTypeMismatch (1,6) NodeArr LeafBool)  $ run     "mean(false)"
                 assertEqual(leafTypeMismatch (1,6) NodeArr LeafNull)  $ run     "mean(null)"

                 assertEqual(leafTypeMismatch (1,7) (NodeOr atom) LeafTable) $ runWith "mean([f()])" [("f",TableO (1,7) [] [])]
                 assertEqual(leafTypeMismatch (1,7) (NodeOr atom) LeafPlot)  $ runWith "mean([f()])" [("f",PlotO  (1,7) [] [])]
                 assertEqual(leafTypeMismatch (1,7) (NodeOr atom) NodeArr)   $ run     "mean([[]])"
                 assertEqual(leafTypeMismatch (1,7) (NodeOr atom) NodeObj)   $ run     "mean([{}])"

test_Desc = do   assertEqual(leafTypeMismatch (1,13) NodeArr LeafTable) $ runWith "descriptive(f())" [("f",TableO (1,13) [] [])]
                 assertEqual(leafTypeMismatch (1,13) NodeArr LeafPlot)  $ runWith "descriptive(f())" [("f",PlotO  (1,13) [] [])]
                 assertEqual(leafTypeMismatch (1,13) NodeArr NodeObj)   $ run     "descriptive({})"
                 assertEqual(leafTypeMismatch (1,13) NodeArr LeafStr)   $ run     "descriptive(\"\")"
                 assertEqual(leafTypeMismatch (1,13) NodeArr LeafNum)   $ run     "descriptive(0)"
                 assertEqual(leafTypeMismatch (1,13) NodeArr LeafBool)  $ run     "descriptive(false)"
                 assertEqual(leafTypeMismatch (1,13) NodeArr LeafNull)  $ run     "descriptive(null)"

                 assertEqual(leafTypeMismatch (1,14) (NodeOr atom) LeafTable) $ runWith "descriptive([f()])" [("f",TableO (1,14) [] [])]
                 assertEqual(leafTypeMismatch (1,14) (NodeOr atom) LeafPlot)  $ runWith "descriptive([f()])" [("f",PlotO  (1,14) [] [])]
                 assertEqual(leafTypeMismatch (1,14) (NodeOr atom) NodeArr)   $ run     "descriptive([[]])"
                 assertEqual(leafTypeMismatch (1,14) (NodeOr atom) NodeObj)   $ run     "descriptive([{}])"

test_Table = do  assertEqual(leafTypeMismatch (1,7) NodeArr LeafTable) $ runWith "table(f(),{})" [("f",TableO (1,7) [] [])]
                 assertEqual(leafTypeMismatch (1,7) NodeArr LeafPlot)  $ runWith "table(f(),{})" [("f",PlotO  (1,7) [] [])]
                 assertEqual(leafTypeMismatch (1,7) NodeArr NodeObj)   $ run     "table({},{})"
                 assertEqual(leafTypeMismatch (1,7) NodeArr LeafStr)   $ run     "table(\"\",{})"
                 assertEqual(leafTypeMismatch (1,7) NodeArr LeafNum)   $ run     "table(0,{})"
                 assertEqual(leafTypeMismatch (1,7) NodeArr LeafBool)  $ run     "table(false,{})"
                 assertEqual(leafTypeMismatch (1,7) NodeArr LeafNull)  $ run     "table(null,{})"

                 assertEqual(leafTypeMismatch (1,7) NodeArr LeafTable) $ runWith "table(f(),0)" [("f",TableO (1,7) [] [])]
                 assertEqual(leafTypeMismatch (1,7) NodeArr LeafPlot)  $ runWith "table(f(),0)" [("f",PlotO  (1,7) [] [])]
                 assertEqual(leafTypeMismatch (1,7) NodeArr NodeObj)   $ run     "table({},0)"
                 assertEqual(leafTypeMismatch (1,7) NodeArr LeafStr)   $ run     "table(\"\",0)"
                 assertEqual(leafTypeMismatch (1,7) NodeArr LeafNum)   $ run     "table(0,0)"
                 assertEqual(leafTypeMismatch (1,7) NodeArr LeafBool)  $ run     "table(false,0)"
                 assertEqual(leafTypeMismatch (1,7) NodeArr LeafNull)  $ run     "table(null,0)"

                 assertEqual(leafTypeMismatch (1,8) NodeArr LeafTable) $ runWith "table([f()],{})" [("f",TableO (1,8) [] [])]
                 assertEqual(leafTypeMismatch (1,8) NodeArr LeafPlot)  $ runWith "table([f()],{})" [("f",PlotO  (1,8) [] [])]
                 assertEqual(leafTypeMismatch (1,8) NodeArr NodeObj)   $ run     "table([{}],{})"
                 assertEqual(leafTypeMismatch (1,8) NodeArr LeafStr)   $ run     "table([\"\"],{})"
                 assertEqual(leafTypeMismatch (1,8) NodeArr LeafNum)   $ run     "table([0],{})"
                 assertEqual(leafTypeMismatch (1,8) NodeArr LeafBool)  $ run     "table([false],{})"
                 assertEqual(leafTypeMismatch (1,8) NodeArr LeafNull)  $ run     "table([null],{})"

                 assertEqual(leafTypeMismatch (1,8) NodeArr LeafTable) $ runWith "table([f()],0)" [("f",TableO (1,8) [] [])]
                 assertEqual(leafTypeMismatch (1,8) NodeArr LeafPlot)  $ runWith "table([f()],0)" [("f",PlotO  (1,8) [] [])]
                 assertEqual(leafTypeMismatch (1,8) NodeArr NodeObj)   $ run     "table([{}],0)"
                 assertEqual(leafTypeMismatch (1,8) NodeArr LeafStr)   $ run     "table([\"\"],0)"
                 assertEqual(leafTypeMismatch (1,8) NodeArr LeafNum)   $ run     "table([0],0)"
                 assertEqual(leafTypeMismatch (1,8) NodeArr LeafBool)  $ run     "table([false],0)"
                 assertEqual(leafTypeMismatch (1,8) NodeArr LeafNull)  $ run     "table([null],0)"

                 assertEqual(leafTypeMismatch (1,9) (NodeOr atom) LeafTable) $ runWith "table([[f()]],{})" [("f",TableO (1,9) [] [])]
                 assertEqual(leafTypeMismatch (1,9) (NodeOr atom) LeafPlot)  $ runWith "table([[f()]],{})" [("f",PlotO  (1,9) [] [])]
                 assertEqual(leafTypeMismatch (1,9) (NodeOr atom) NodeArr)   $ run     "table([[[]]],{})"
                 assertEqual(leafTypeMismatch (1,9) (NodeOr atom) NodeObj)   $ run     "table([[{}]],{})"

                 assertEqual(leafTypeMismatch (1,9) (NodeOr atom) LeafTable) $ runWith "table([[f()]],0)" [("f",TableO (1,9) [] [])]
                 assertEqual(leafTypeMismatch (1,9) (NodeOr atom) LeafPlot)  $ runWith "table([[f()]],0)" [("f",PlotO  (1,9) [] [])]
                 assertEqual(leafTypeMismatch (1,9) (NodeOr atom) NodeArr)   $ run     "table([[[]]],0)"
                 assertEqual(leafTypeMismatch (1,9) (NodeOr atom) NodeObj)   $ run     "table([[{}]],0)"

                 assertEqual(leafTypeMismatch (1,13) NodeObj LeafTable) $ runWith "table([[0]],f())" [("f",TableO (1,13) [] [])]
                 assertEqual(leafTypeMismatch (1,13) NodeObj LeafPlot)  $ runWith "table([[0]],f())" [("f",PlotO  (1,13) [] [])]
                 assertEqual(leafTypeMismatch (1,13) NodeObj NodeArr)   $ run     "table([[0]],[])"
                 assertEqual(leafTypeMismatch (1,13) NodeObj LeafStr)   $ run     "table([[0]],\"\")"
                 assertEqual(leafTypeMismatch (1,13) NodeObj LeafNum)   $ run     "table([[0]],0)"
                 assertEqual(leafTypeMismatch (1,13) NodeObj LeafBool)  $ run     "table([[0]],false)"
                 assertEqual(leafTypeMismatch (1,13) NodeObj LeafNull)  $ run     "table([[0]],null)"

                 assertEqual(leafTypeMismatch (1,16) NodeArr LeafTable) $ runWith "table([[0]],{x:f()})" [("f",TableO (1,16) [] [])]
                 assertEqual(leafTypeMismatch (1,16) NodeArr LeafPlot)  $ runWith "table([[0]],{x:f()})" [("f",PlotO  (1,16) [] [])]
                 assertEqual(leafTypeMismatch (1,16) NodeArr NodeObj)   $ run     "table([[0]],{x:{}})"
                 assertEqual(leafTypeMismatch (1,16) NodeArr LeafStr)   $ run     "table([[0]],{x:\"\"})"
                 assertEqual(leafTypeMismatch (1,16) NodeArr LeafNum)   $ run     "table([[0]],{x:0})"
                 assertEqual(leafTypeMismatch (1,16) NodeArr LeafBool)  $ run     "table([[0]],{x:false})"
                 assertEqual(leafTypeMismatch (1,16) NodeArr LeafNull)  $ run     "table([[0]],{x:null})"

                 assertEqual(leafTypeMismatch (1,17) LeafStr LeafTable) $ runWith "table([[0]],{x:[f()]})" [("f",TableO (1,17) [] [])]
                 assertEqual(leafTypeMismatch (1,17) LeafStr LeafPlot)  $ runWith "table([[0]],{x:[f()]})" [("f",PlotO  (1,17) [] [])]
                 assertEqual(leafTypeMismatch (1,17) LeafStr NodeArr)   $ run     "table([[0]],{x:[[]]})"
                 assertEqual(leafTypeMismatch (1,17) LeafStr NodeObj)   $ run     "table([[0]],{x:[{}]})"
                 assertEqual(leafTypeMismatch (1,17) LeafStr LeafNum)   $ run     "table([[0]],{x:[0]})"
                 assertEqual(leafTypeMismatch (1,17) LeafStr LeafBool)  $ run     "table([[0]],{x:[false]})"
                 assertEqual(leafTypeMismatch (1,17) LeafStr LeafNull)  $ run     "table([[0]],{x:[null]})"

test_NTimes = do assertEqual(leafTypeMismatch (1,8) LeafNum LeafTable)  $ runWith "nTimes(f(),0)" [("f",TableO (1,8) [] [])]
                 assertEqual(leafTypeMismatch (1,8) LeafNum LeafPlot)   $ runWith "nTimes(f(),0)" [("f",PlotO  (1,8) [] [])]
                 assertEqual(leafTypeMismatch (1,8) LeafNum NodeArr)    $ run     "nTimes([],0)"
                 assertEqual(leafTypeMismatch (1,8) LeafNum NodeObj)    $ run     "nTimes({},0)"
                 assertEqual(leafTypeMismatch (1,8) LeafNum LeafStr)    $ run     "nTimes(\"\",0)"
                 assertEqual(leafTypeMismatch (1,8) LeafNum LeafBool)   $ run     "nTimes(false,0)"
                 assertEqual(leafTypeMismatch (1,8) LeafNum LeafNull)   $ run     "nTimes(null,0)"

                 assertEqual(leafTypeMismatch (1,8) LeafNum LeafTable)  $ runWith "nTimes(f(),false)" [("f",TableO (1,8) [] [])]
                 assertEqual(leafTypeMismatch (1,8) LeafNum LeafPlot)   $ runWith "nTimes(f(),false)" [("f",PlotO  (1,8) [] [])]
                 assertEqual(leafTypeMismatch (1,8) LeafNum NodeArr)    $ run     "nTimes([],false)"
                 assertEqual(leafTypeMismatch (1,8) LeafNum NodeObj)    $ run     "nTimes({},false)"
                 assertEqual(leafTypeMismatch (1,8) LeafNum LeafStr)    $ run     "nTimes(\"\",false)"
                 assertEqual(leafTypeMismatch (1,8) LeafNum LeafBool)   $ run     "nTimes(false,false)"
                 assertEqual(leafTypeMismatch (1,8) LeafNum LeafNull)   $ run     "nTimes(null,false)"

                 assertEqual(leafTypeMismatch (1,10) LeafNum LeafTable)  $ runWith "nTimes(0,f())" [("f",TableO (1,10) [] [])]
                 assertEqual(leafTypeMismatch (1,10) LeafNum LeafPlot)   $ runWith "nTimes(0,f())" [("f",PlotO  (1,10) [] [])]
                 assertEqual(leafTypeMismatch (1,10) LeafNum NodeArr)    $ run     "nTimes(0,[])"
                 assertEqual(leafTypeMismatch (1,10) LeafNum NodeObj)    $ run     "nTimes(0,{})"
                 assertEqual(leafTypeMismatch (1,10) LeafNum LeafStr)    $ run     "nTimes(0,\"\")"
                 assertEqual(leafTypeMismatch (1,10) LeafNum LeafBool)   $ run     "nTimes(0,false)"
                 assertEqual(leafTypeMismatch (1,10) LeafNum LeafNull)   $ run     "nTimes(0,null)"


test_Take = do   assertEqual(leafTypeMismatch (1,6) LeafNum LeafTable)  $ runWith "take(f(),g())"   [("g",tableO [] []),("f",TableO (1,6) [] [])]
                 assertEqual(leafTypeMismatch (1,6) LeafNum LeafPlot)   $ runWith "take(f(),g())"   [("g",tableO [] []),("f",PlotO  (1,6) [] [])]
                 assertEqual(leafTypeMismatch (1,6) LeafNum NodeArr)    $ runWith "take([],g())"    [("g",tableO [] [])]
                 assertEqual(leafTypeMismatch (1,6) LeafNum NodeObj)    $ runWith "take({},g())"    [("g",tableO [] [])]
                 assertEqual(leafTypeMismatch (1,6) LeafNum LeafStr)    $ runWith "take(\"\",g())"  [("g",tableO [] [])]
                 assertEqual(leafTypeMismatch (1,6) LeafNum LeafBool)   $ runWith "take(false,g())" [("g",tableO [] [])]
                 assertEqual(leafTypeMismatch (1,6) LeafNum LeafNull)   $ runWith "take(null,g())"  [("g",tableO [] [])]

                 assertEqual(leafTypeMismatch (1,6) LeafNum LeafTable)  $ runWith "take(f(),[])" [("f",TableO (1,6) [] [])]
                 assertEqual(leafTypeMismatch (1,6) LeafNum LeafPlot)   $ runWith "take(f(),[])" [("f",PlotO  (1,6) [] [])]
                 assertEqual(leafTypeMismatch (1,6) LeafNum NodeArr)    $ run     "take([],[])"
                 assertEqual(leafTypeMismatch (1,6) LeafNum NodeObj)    $ run     "take({},[])"
                 assertEqual(leafTypeMismatch (1,6) LeafNum LeafStr)    $ run     "take(\"\",[])"
                 assertEqual(leafTypeMismatch (1,6) LeafNum LeafBool)   $ run     "take(false,[])"
                 assertEqual(leafTypeMismatch (1,6) LeafNum LeafNull)   $ run     "take(null,[])"

                 assertEqual(leafTypeMismatch (1,6) LeafNum LeafTable)  $ runWith "take(f(),0)" [("f",TableO (1,6) [] [])]
                 assertEqual(leafTypeMismatch (1,6) LeafNum LeafPlot)   $ runWith "take(f(),0)" [("f",PlotO  (1,6) [] [])]
                 assertEqual(leafTypeMismatch (1,6) LeafNum NodeArr)    $ run     "take([],0)"
                 assertEqual(leafTypeMismatch (1,6) LeafNum NodeObj)    $ run     "take({},0)"
                 assertEqual(leafTypeMismatch (1,6) LeafNum LeafStr)    $ run     "take(\"\",0)"
                 assertEqual(leafTypeMismatch (1,6) LeafNum LeafBool)   $ run     "take(false,0)"
                 assertEqual(leafTypeMismatch (1,6) LeafNum LeafNull)   $ run     "take(null,0)"

                 assertEqual(leafTypeMismatch (1,8) (NodeOr tableOrArr) LeafPlot)   $ runWith "take(0,f())" [("f",PlotO  (1,8) [] [])]
                 assertEqual(leafTypeMismatch (1,8) (NodeOr tableOrArr) NodeObj)    $ run     "take(0,{})"
                 assertEqual(leafTypeMismatch (1,8) (NodeOr tableOrArr) LeafStr)    $ run     "take(0,\"\")"
                 assertEqual(leafTypeMismatch (1,8) (NodeOr tableOrArr) LeafBool)   $ run     "take(0,false)"
                 assertEqual(leafTypeMismatch (1,8) (NodeOr tableOrArr) LeafNull)   $ run     "take(0,null)"

test_Sort = do   assertEqual(leafTypeMismatch (1,6) LeafNum LeafTable)  $ runWith "sort(f(),g())"   [("g",tableO [] []),("f",TableO (1,6) [] [])]
                 assertEqual(leafTypeMismatch (1,6) LeafNum LeafPlot)   $ runWith "sort(f(),g())"   [("g",tableO [] []),("f",PlotO  (1,6) [] [])]
                 assertEqual(leafTypeMismatch (1,6) LeafNum NodeArr)    $ runWith "sort([],g())"    [("g",tableO [] [])]
                 assertEqual(leafTypeMismatch (1,6) LeafNum NodeObj)    $ runWith "sort({},g())"    [("g",tableO [] [])]
                 assertEqual(leafTypeMismatch (1,6) LeafNum LeafStr)    $ runWith "sort(\"\",g())"  [("g",tableO [] [])]
                 assertEqual(leafTypeMismatch (1,6) LeafNum LeafBool)   $ runWith "sort(false,g())" [("g",tableO [] [])]
                 assertEqual(leafTypeMismatch (1,6) LeafNum LeafNull)   $ runWith "sort(null,g())"  [("g",tableO [] [])]

                 assertEqual(leafTypeMismatch (1,6) LeafNum LeafTable)  $ runWith "sort(f(),[])" [("f",TableO (1,6) [] [])]
                 assertEqual(leafTypeMismatch (1,6) LeafNum LeafPlot)   $ runWith "sort(f(),[])" [("f",PlotO  (1,6) [] [])]
                 assertEqual(leafTypeMismatch (1,6) LeafNum NodeArr)    $ run     "sort([],[])"
                 assertEqual(leafTypeMismatch (1,6) LeafNum NodeObj)    $ run     "sort({},[])"
                 assertEqual(leafTypeMismatch (1,6) LeafNum LeafStr)    $ run     "sort(\"\",[])"
                 assertEqual(leafTypeMismatch (1,6) LeafNum LeafBool)   $ run     "sort(false,[])"
                 assertEqual(leafTypeMismatch (1,6) LeafNum LeafNull)   $ run     "sort(null,[])"

                 assertEqual(leafTypeMismatch (1,6) LeafNum LeafTable)  $ runWith "sort(f(),[[]])" [("f",TableO (1,6) [] [])]
                 assertEqual(leafTypeMismatch (1,6) LeafNum LeafPlot)   $ runWith "sort(f(),[[]])" [("f",PlotO  (1,6) [] [])]
                 assertEqual(leafTypeMismatch (1,6) LeafNum NodeArr)    $ run     "sort([],[[]])"
                 assertEqual(leafTypeMismatch (1,6) LeafNum NodeObj)    $ run     "sort({},[[]])"
                 assertEqual(leafTypeMismatch (1,6) LeafNum LeafStr)    $ run     "sort(\"\",[[]])"
                 assertEqual(leafTypeMismatch (1,6) LeafNum LeafBool)   $ run     "sort(false,[[]])"
                 assertEqual(leafTypeMismatch (1,6) LeafNum LeafNull)   $ run     "sort(null,[[]])"

                 assertEqual(leafTypeMismatch (1,6) LeafNum LeafTable)  $ runWith "sort(f(),0)" [("f",TableO (1,6) [] [])]
                 assertEqual(leafTypeMismatch (1,6) LeafNum LeafPlot)   $ runWith "sort(f(),0)" [("f",PlotO  (1,6) [] [])]
                 assertEqual(leafTypeMismatch (1,6) LeafNum NodeArr)    $ run     "sort([],0)"
                 assertEqual(leafTypeMismatch (1,6) LeafNum NodeObj)    $ run     "sort({},0)"
                 assertEqual(leafTypeMismatch (1,6) LeafNum LeafStr)    $ run     "sort(\"\",0)"
                 assertEqual(leafTypeMismatch (1,6) LeafNum LeafBool)   $ run     "sort(false,0)"
                 assertEqual(leafTypeMismatch (1,6) LeafNum LeafNull)   $ run     "sort(null,0)"

                 assertEqual(leafTypeMismatch (1,8) (NodeOr tableOrArr) LeafPlot) $ runWith "sort(0,f())" [("f",PlotO  (1,8) [] [])]
                 assertEqual(leafTypeMismatch (1,8) (NodeOr tableOrArr) NodeObj)  $ run     "sort(0,{})"
                 assertEqual(leafTypeMismatch (1,8) (NodeOr tableOrArr) LeafStr)  $ run     "sort(0,\"\")"
                 assertEqual(leafTypeMismatch (1,8) (NodeOr tableOrArr) LeafBool) $ run     "sort(0,false)"
                 assertEqual(leafTypeMismatch (1,8) (NodeOr tableOrArr) LeafNull) $ run     "sort(0,null)"

                 assertEqual(nodeTypeMismatch [TMLeaf (1,8) LeafTable NodeArr,TMLeaf (1,9) NodeArr LeafTable]) $ runWith "sort(0,[f()])" [("f",TableO (1,9) [] [])]
                 assertEqual(nodeTypeMismatch [TMLeaf (1,8) LeafTable NodeArr,TMLeaf (1,9) NodeArr LeafPlot])  $ runWith "sort(0,[f()])" [("f",PlotO  (1,9) [] [])]
                 assertEqual(nodeTypeMismatch [TMLeaf (1,8) LeafTable NodeArr,TMLeaf (1,9) NodeArr NodeObj])   $ run     "sort(0,[{}])"
                 assertEqual(nodeTypeMismatch [TMLeaf (1,8) LeafTable NodeArr,TMLeaf (1,9) NodeArr LeafStr])   $ run     "sort(0,[\"\"])"
                 assertEqual(nodeTypeMismatch [TMLeaf (1,8) LeafTable NodeArr,TMLeaf (1,9) NodeArr LeafNum])   $ run     "sort(0,[0])"
                 assertEqual(nodeTypeMismatch [TMLeaf (1,8) LeafTable NodeArr,TMLeaf (1,9) NodeArr LeafBool])  $ run     "sort(0,[false])"
                 assertEqual(nodeTypeMismatch [TMLeaf (1,8) LeafTable NodeArr,TMLeaf (1,9) NodeArr LeafNull])  $ run     "sort(0,[null])"

                 assertEqual(nodeTypeMismatch [TMLeaf (1,8) LeafTable NodeArr,TMLeaf (1,10) (NodeOr atom) LeafTable])  $ runWith "sort(0,[[f()]])" [("f",TableO (1,10) [] [])]
                 assertEqual(nodeTypeMismatch [TMLeaf (1,8) LeafTable NodeArr,TMLeaf (1,10) (NodeOr atom) LeafPlot])   $ runWith "sort(0,[[f()]])" [("f",PlotO  (1,10) [] [])]
                 assertEqual(nodeTypeMismatch [TMLeaf (1,8) LeafTable NodeArr,TMLeaf (1,10) (NodeOr atom) NodeArr])    $ run     "sort(0,[[[]]])"
                 assertEqual(nodeTypeMismatch [TMLeaf (1,8) LeafTable NodeArr,TMLeaf (1,10) (NodeOr atom) NodeObj])    $ run     "sort(0,[[{}]])"

test_Col = do    assertEqual(leafTypeMismatch (1,5) LeafNum LeafTable)  $ runWith "col(f(),g())"   [("g",tableO [] []),("f",TableO (1,5) [] [])]
                 assertEqual(leafTypeMismatch (1,5) LeafNum LeafPlot)   $ runWith "col(f(),g())"   [("g",tableO [] []),("f",PlotO  (1,5) [] [])]
                 assertEqual(leafTypeMismatch (1,5) LeafNum NodeArr)    $ runWith "col([],g())"    [("g",tableO [] [])]
                 assertEqual(leafTypeMismatch (1,5) LeafNum NodeObj)    $ runWith "col({},g())"    [("g",tableO [] [])]
                 assertEqual(leafTypeMismatch (1,5) LeafNum LeafStr)    $ runWith "col(\"\",g())"  [("g",tableO [] [])]
                 assertEqual(leafTypeMismatch (1,5) LeafNum LeafBool)   $ runWith "col(false,g())" [("g",tableO [] [])]
                 assertEqual(leafTypeMismatch (1,5) LeafNum LeafNull)   $ runWith "col(null,g())"  [("g",tableO [] [])]

                 assertEqual(leafTypeMismatch (1,5) LeafNum LeafTable)  $ runWith "col(f(),[])" [("f",TableO (1,5) [] [])]
                 assertEqual(leafTypeMismatch (1,5) LeafNum LeafPlot)   $ runWith "col(f(),[])" [("f",PlotO  (1,5) [] [])]
                 assertEqual(leafTypeMismatch (1,5) LeafNum NodeArr)    $ run     "col([],[])"
                 assertEqual(leafTypeMismatch (1,5) LeafNum NodeObj)    $ run     "col({},[])"
                 assertEqual(leafTypeMismatch (1,5) LeafNum LeafStr)    $ run     "col(\"\",[])"
                 assertEqual(leafTypeMismatch (1,5) LeafNum LeafBool)   $ run     "col(false,[])"
                 assertEqual(leafTypeMismatch (1,5) LeafNum LeafNull)   $ run     "col(null,[])"

                 assertEqual(leafTypeMismatch (1,5) LeafNum LeafTable)  $ runWith "col(f(),[[]])" [("f",TableO (1,5) [] [])]
                 assertEqual(leafTypeMismatch (1,5) LeafNum LeafPlot)   $ runWith "col(f(),[[]])" [("f",PlotO  (1,5) [] [])]
                 assertEqual(leafTypeMismatch (1,5) LeafNum NodeArr)    $ run     "col([],[[]])"
                 assertEqual(leafTypeMismatch (1,5) LeafNum NodeObj)    $ run     "col({},[[]])"
                 assertEqual(leafTypeMismatch (1,5) LeafNum LeafStr)    $ run     "col(\"\",[[]])"
                 assertEqual(leafTypeMismatch (1,5) LeafNum LeafBool)   $ run     "col(false,[[]])"
                 assertEqual(leafTypeMismatch (1,5) LeafNum LeafNull)   $ run     "col(null,[[]])"

                 assertEqual(leafTypeMismatch (1,5) LeafNum LeafTable)  $ runWith "col(f(),0)" [("f",TableO (1,5) [] [])]
                 assertEqual(leafTypeMismatch (1,5) LeafNum LeafPlot)   $ runWith "col(f(),0)" [("f",PlotO  (1,5) [] [])]
                 assertEqual(leafTypeMismatch (1,5) LeafNum NodeArr)    $ run     "col([],0)"
                 assertEqual(leafTypeMismatch (1,5) LeafNum NodeObj)    $ run     "col({},0)"
                 assertEqual(leafTypeMismatch (1,5) LeafNum LeafStr)    $ run     "col(\"\",0)"
                 assertEqual(leafTypeMismatch (1,5) LeafNum LeafBool)   $ run     "col(false,0)"
                 assertEqual(leafTypeMismatch (1,5) LeafNum LeafNull)   $ run     "col(null,0)"

                 assertEqual(leafTypeMismatch (1,7) (NodeOr tableOrArr) LeafPlot) $ runWith "col(0,f())" [("f",PlotO  (1,7) [] [])]
                 assertEqual(leafTypeMismatch (1,7) (NodeOr tableOrArr) NodeObj)  $ run     "col(0,{})"
                 assertEqual(leafTypeMismatch (1,7) (NodeOr tableOrArr) LeafStr)  $ run     "col(0,\"\")"
                 assertEqual(leafTypeMismatch (1,7) (NodeOr tableOrArr) LeafBool) $ run     "col(0,false)"
                 assertEqual(leafTypeMismatch (1,7) (NodeOr tableOrArr) LeafNull) $ run     "col(0,null)"

                 assertEqual(nodeTypeMismatch [TMLeaf (1,7) LeafTable NodeArr,TMLeaf (1,8) NodeArr LeafTable]) $ runWith "col(0,[f()])" [("f",TableO (1,8) [] [])]
                 assertEqual(nodeTypeMismatch [TMLeaf (1,7) LeafTable NodeArr,TMLeaf (1,8) NodeArr LeafPlot])  $ runWith "col(0,[f()])" [("f",PlotO  (1,8) [] [])]
                 assertEqual(nodeTypeMismatch [TMLeaf (1,7) LeafTable NodeArr,TMLeaf (1,8) NodeArr NodeObj])   $ run     "col(0,[{}])"
                 assertEqual(nodeTypeMismatch [TMLeaf (1,7) LeafTable NodeArr,TMLeaf (1,8) NodeArr LeafStr])   $ run     "col(0,[\"\"])"
                 assertEqual(nodeTypeMismatch [TMLeaf (1,7) LeafTable NodeArr,TMLeaf (1,8) NodeArr LeafNum])   $ run     "col(0,[0])"
                 assertEqual(nodeTypeMismatch [TMLeaf (1,7) LeafTable NodeArr,TMLeaf (1,8) NodeArr LeafBool])  $ run     "col(0,[false])"
                 assertEqual(nodeTypeMismatch [TMLeaf (1,7) LeafTable NodeArr,TMLeaf (1,8) NodeArr LeafNull])  $ run     "col(0,[null])"

                 assertEqual(nodeTypeMismatch [TMLeaf (1,7) LeafTable NodeArr,TMLeaf (1,9) (NodeOr atom) LeafTable])  $ runWith "col(0,[[f()]])" [("f",TableO (1,9) [] [])]
                 assertEqual(nodeTypeMismatch [TMLeaf (1,7) LeafTable NodeArr,TMLeaf (1,9) (NodeOr atom) LeafPlot])   $ runWith "col(0,[[f()]])" [("f",PlotO  (1,9) [] [])]
                 assertEqual(nodeTypeMismatch [TMLeaf (1,7) LeafTable NodeArr,TMLeaf (1,9) (NodeOr atom) NodeArr])    $ run     "col(0,[[[]]])"
                 assertEqual(nodeTypeMismatch [TMLeaf (1,7) LeafTable NodeArr,TMLeaf (1,9) (NodeOr atom) NodeObj])    $ run     "col(0,[[{}]])"

test_Plot = do   assertEqual(leafTypeMismatch (1,6) NodeArr LeafTable) $ runWith "plot(f(),[],{})" [("f",TableO (1,6) [] [])]
                 assertEqual(leafTypeMismatch (1,6) NodeArr LeafPlot)  $ runWith "plot(f(),[],{})" [("f",PlotO  (1,6) [] [])]
                 assertEqual(leafTypeMismatch (1,6) NodeArr NodeObj)   $ run     "plot({},[],{})"
                 assertEqual(leafTypeMismatch (1,6) NodeArr LeafStr)   $ run     "plot(\"\",[],{})"
                 assertEqual(leafTypeMismatch (1,6) NodeArr LeafNum)   $ run     "plot(0,[],{})"
                 assertEqual(leafTypeMismatch (1,6) NodeArr LeafBool)  $ run     "plot(false,[],{})"
                 assertEqual(leafTypeMismatch (1,6) NodeArr LeafNull)  $ run     "plot(null,[],{})"

                 assertEqual(leafTypeMismatch (1,6) NodeArr LeafTable) $ runWith "plot(f(),[],0)" [("f",TableO (1,6) [] [])]
                 assertEqual(leafTypeMismatch (1,6) NodeArr LeafPlot)  $ runWith "plot(f(),[],0)" [("f",PlotO  (1,6) [] [])]
                 assertEqual(leafTypeMismatch (1,6) NodeArr NodeObj)   $ run     "plot({},[],0)"
                 assertEqual(leafTypeMismatch (1,6) NodeArr LeafStr)   $ run     "plot(\"\",[],0)"
                 assertEqual(leafTypeMismatch (1,6) NodeArr LeafNum)   $ run     "plot(0,[],0)"
                 assertEqual(leafTypeMismatch (1,6) NodeArr LeafBool)  $ run     "plot(false,[],0)"
                 assertEqual(leafTypeMismatch (1,6) NodeArr LeafNull)  $ run     "plot(null,[],0)"

                 assertEqual(leafTypeMismatch (1,6) NodeArr LeafTable) $ runWith "plot(f(),0,{})" [("f",TableO (1,6) [] [])]
                 assertEqual(leafTypeMismatch (1,6) NodeArr LeafPlot)  $ runWith "plot(f(),0,{})" [("f",PlotO  (1,6) [] [])]
                 assertEqual(leafTypeMismatch (1,6) NodeArr NodeObj)   $ run     "plot({},0,{})"
                 assertEqual(leafTypeMismatch (1,6) NodeArr LeafStr)   $ run     "plot(\"\",0,{})"
                 assertEqual(leafTypeMismatch (1,6) NodeArr LeafNum)   $ run     "plot(0,0,{})"
                 assertEqual(leafTypeMismatch (1,6) NodeArr LeafBool)  $ run     "plot(false,0,{})"
                 assertEqual(leafTypeMismatch (1,6) NodeArr LeafNull)  $ run     "plot(null,0,{})"

                 assertEqual(leafTypeMismatch (1,6) NodeArr LeafTable) $ runWith "plot(f(),0,0)" [("f",TableO (1,6) [] [])]
                 assertEqual(leafTypeMismatch (1,6) NodeArr LeafPlot)  $ runWith "plot(f(),0,0)" [("f",PlotO  (1,6) [] [])]
                 assertEqual(leafTypeMismatch (1,6) NodeArr NodeObj)   $ run     "plot({},0,0)"
                 assertEqual(leafTypeMismatch (1,6) NodeArr LeafStr)   $ run     "plot(\"\",0,0)"
                 assertEqual(leafTypeMismatch (1,6) NodeArr LeafNum)   $ run     "plot(0,0,0)"
                 assertEqual(leafTypeMismatch (1,6) NodeArr LeafBool)  $ run     "plot(false,0,0)"
                 assertEqual(leafTypeMismatch (1,6) NodeArr LeafNull)  $ run     "plot(null,0,0)"

                 assertEqual(leafTypeMismatch (1,7) LeafNum LeafTable) $ runWith "plot([f()],[],{})" [("f",TableO (1,7) [] [])]
                 assertEqual(leafTypeMismatch (1,7) LeafNum LeafPlot)  $ runWith "plot([f()],[],{})" [("f",PlotO  (1,7) [] [])]
                 assertEqual(leafTypeMismatch (1,7) LeafNum NodeArr)   $ run     "plot([[]],[],{})"
                 assertEqual(leafTypeMismatch (1,7) LeafNum NodeObj)   $ run     "plot([{}],[],{})"
                 assertEqual(leafTypeMismatch (1,7) LeafNum LeafStr)   $ run     "plot([\"\"],[],{})"
                 assertEqual(leafTypeMismatch (1,7) LeafNum LeafBool)  $ run     "plot([false],[],{})"
                 assertEqual(leafTypeMismatch (1,7) LeafNum LeafNull)  $ run     "plot([null],[],{})"

                 assertEqual(leafTypeMismatch (1,7) LeafNum LeafTable) $ runWith "plot([f()],[],0)" [("f",TableO (1,7) [] [])]
                 assertEqual(leafTypeMismatch (1,7) LeafNum LeafPlot)  $ runWith "plot([f()],[],0)" [("f",PlotO  (1,7) [] [])]
                 assertEqual(leafTypeMismatch (1,7) LeafNum NodeArr)   $ run     "plot([[]],[],0)"
                 assertEqual(leafTypeMismatch (1,7) LeafNum NodeObj)   $ run     "plot([{}],[],0)"
                 assertEqual(leafTypeMismatch (1,7) LeafNum LeafStr)   $ run     "plot([\"\"],[],0)"
                 assertEqual(leafTypeMismatch (1,7) LeafNum LeafBool)  $ run     "plot([false],[],0)"
                 assertEqual(leafTypeMismatch (1,7) LeafNum LeafNull)  $ run     "plot([null],[],0)"

                 assertEqual(leafTypeMismatch (1,7) LeafNum LeafTable) $ runWith "plot([f()],0,{})" [("f",TableO (1,7) [] [])]
                 assertEqual(leafTypeMismatch (1,7) LeafNum LeafPlot)  $ runWith "plot([f()],0,{})" [("f",PlotO  (1,7) [] [])]
                 assertEqual(leafTypeMismatch (1,7) LeafNum NodeArr)   $ run     "plot([[]],0,{})"
                 assertEqual(leafTypeMismatch (1,7) LeafNum NodeObj)   $ run     "plot([{}],0,{})"
                 assertEqual(leafTypeMismatch (1,7) LeafNum LeafStr)   $ run     "plot([\"\"],0,{})"
                 assertEqual(leafTypeMismatch (1,7) LeafNum LeafBool)  $ run     "plot([false],0,{})"
                 assertEqual(leafTypeMismatch (1,7) LeafNum LeafNull)  $ run     "plot([null],0,{})"

                 assertEqual(leafTypeMismatch (1,7) LeafNum LeafTable) $ runWith "plot([f()],0,0)" [("f",TableO (1,7) [] [])]
                 assertEqual(leafTypeMismatch (1,7) LeafNum LeafPlot)  $ runWith "plot([f()],0,0)" [("f",PlotO  (1,7) [] [])]
                 assertEqual(leafTypeMismatch (1,7) LeafNum NodeArr)   $ run     "plot([[]],0,0)"
                 assertEqual(leafTypeMismatch (1,7) LeafNum NodeObj)   $ run     "plot([{}],0,0)"
                 assertEqual(leafTypeMismatch (1,7) LeafNum LeafStr)   $ run     "plot([\"\"],0,0)"
                 assertEqual(leafTypeMismatch (1,7) LeafNum LeafBool)  $ run     "plot([false],0,0)"
                 assertEqual(leafTypeMismatch (1,7) LeafNum LeafNull)  $ run     "plot([null],0,0)"

                 assertEqual(leafTypeMismatch (1,9) NodeArr LeafTable) $ runWith "plot([],f(),{})" [("f",TableO (1,9) [] [])]
                 assertEqual(leafTypeMismatch (1,9) NodeArr LeafPlot)  $ runWith "plot([],f(),{})" [("f",PlotO  (1,9) [] [])]
                 assertEqual(leafTypeMismatch (1,9) NodeArr NodeObj)   $ run     "plot([],{},{})"
                 assertEqual(leafTypeMismatch (1,9) NodeArr LeafStr)   $ run     "plot([],\"\",{})"
                 assertEqual(leafTypeMismatch (1,9) NodeArr LeafNum)   $ run     "plot([],0,{})"
                 assertEqual(leafTypeMismatch (1,9) NodeArr LeafBool)  $ run     "plot([],false,{})"
                 assertEqual(leafTypeMismatch (1,9) NodeArr LeafNull)  $ run     "plot([],null,{})"

                 assertEqual(leafTypeMismatch (1,9) NodeArr LeafTable) $ runWith "plot([],f(),0)" [("f",TableO (1,9) [] [])]
                 assertEqual(leafTypeMismatch (1,9) NodeArr LeafPlot)  $ runWith "plot([],f(),0)" [("f",PlotO  (1,9) [] [])]
                 assertEqual(leafTypeMismatch (1,9) NodeArr NodeObj)   $ run     "plot([],{},0)"
                 assertEqual(leafTypeMismatch (1,9) NodeArr LeafStr)   $ run     "plot([],\"\",0)"
                 assertEqual(leafTypeMismatch (1,9) NodeArr LeafNum)   $ run     "plot([],0,0)"
                 assertEqual(leafTypeMismatch (1,9) NodeArr LeafBool)  $ run     "plot([],false,0)"
                 assertEqual(leafTypeMismatch (1,9) NodeArr LeafNull)  $ run     "plot([],null,0)"

                 assertEqual(leafTypeMismatch (1,10) LeafNum LeafTable) $ runWith "plot([],[f()],{})" [("f",TableO (1,10) [] [])]
                 assertEqual(leafTypeMismatch (1,10) LeafNum LeafPlot)  $ runWith "plot([],[f()],{})" [("f",PlotO  (1,10) [] [])]
                 assertEqual(leafTypeMismatch (1,10) LeafNum NodeArr)   $ run     "plot([],[[]],{})"
                 assertEqual(leafTypeMismatch (1,10) LeafNum NodeObj)   $ run     "plot([],[{}],{})"
                 assertEqual(leafTypeMismatch (1,10) LeafNum LeafStr)   $ run     "plot([],[\"\"],{})"
                 assertEqual(leafTypeMismatch (1,10) LeafNum LeafBool)  $ run     "plot([],[false],{})"
                 assertEqual(leafTypeMismatch (1,10) LeafNum LeafNull)  $ run     "plot([],[null],{})"

                 assertEqual(leafTypeMismatch (1,10) LeafNum LeafTable) $ runWith "plot([],[f()],0)" [("f",TableO (1,10) [] [])]
                 assertEqual(leafTypeMismatch (1,10) LeafNum LeafPlot)  $ runWith "plot([],[f()],0)" [("f",PlotO  (1,10) [] [])]
                 assertEqual(leafTypeMismatch (1,10) LeafNum NodeArr)   $ run     "plot([],[[]],0)"
                 assertEqual(leafTypeMismatch (1,10) LeafNum NodeObj)   $ run     "plot([],[{}],0)"
                 assertEqual(leafTypeMismatch (1,10) LeafNum LeafStr)   $ run     "plot([],[\"\"],0)"
                 assertEqual(leafTypeMismatch (1,10) LeafNum LeafBool)  $ run     "plot([],[false],0)"
                 assertEqual(leafTypeMismatch (1,10) LeafNum LeafNull)  $ run     "plot([],[null],0)"

                 assertEqual(leafTypeMismatch (1,12) NodeObj LeafTable) $ runWith "plot([],[],f())" [("f",TableO (1,12) [] [])]
                 assertEqual(leafTypeMismatch (1,12) NodeObj LeafPlot)  $ runWith "plot([],[],f())" [("f",PlotO  (1,12) [] [])]
                 assertEqual(leafTypeMismatch (1,12) NodeObj NodeArr)   $ run     "plot([],[],[])"
                 assertEqual(leafTypeMismatch (1,12) NodeObj LeafStr)   $ run     "plot([],[],\"\")"
                 assertEqual(leafTypeMismatch (1,12) NodeObj LeafNum)   $ run     "plot([],[],0)"
                 assertEqual(leafTypeMismatch (1,12) NodeObj LeafBool)  $ run     "plot([],[],false)"
                 assertEqual(leafTypeMismatch (1,12) NodeObj LeafNull)  $ run     "plot([],[],null)"

                 assertEqual(leafTypeMismatch (1,15) LeafStr LeafTable) $ runWith "plot([],[],{x:f()})" [("f",TableO (1,15) [] [])]
                 assertEqual(leafTypeMismatch (1,15) LeafStr LeafPlot)  $ runWith "plot([],[],{x:f()})" [("f",PlotO  (1,15) [] [])]
                 assertEqual(leafTypeMismatch (1,15) LeafStr NodeArr)   $ run     "plot([],[],{x:[]})"
                 assertEqual(leafTypeMismatch (1,15) LeafStr NodeObj)   $ run     "plot([],[],{x:{}})"
                 assertEqual(leafTypeMismatch (1,15) LeafStr LeafNum)   $ run     "plot([],[],{x:0})"
                 assertEqual(leafTypeMismatch (1,15) LeafStr LeafBool)  $ run     "plot([],[],{x:false})"
                 assertEqual(leafTypeMismatch (1,15) LeafStr LeafNull)  $ run     "plot([],[],{x:null})"
