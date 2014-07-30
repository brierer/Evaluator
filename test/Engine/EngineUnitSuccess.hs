{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Engine.EngineUnitSuccess where

import Data.ExpObj
import Test.Framework

import Engine.EngineUnitSuccessUtils
import Engine.EngineUnitUtils
import Marshall.MarshallUtils

{-# ANN module "HLint: ignore Use camelCase" #-}

test_Show = do   assertEqual (Right $ ObjO (1,1) [("result",ArrO (1,6) [])])                                      $ run     "show([])"
                 assertEqual (Right $ ObjO (1,1) [("result",ArrO (1,6) [TableO (1,7) [] []])])                    $ runWith "show([f()])" [("f",TableO (1,7) [] [])]
                 assertEqual (Right $ ObjO (1,1) [("result",ArrO (1,6) [PlotO (1,11) [] []])])                    $ runWith "show([f()])" [("f",PlotO (1,11) [] [])]

                 assertEqual (Right $ ObjO (1,1) [("result",ArrO (1,6) [TableO (1,7) [] [],PlotO (1,11) [] []])]) $ runWith "show([f(),g()])" [("f",TableO (1,7) [] []),("g",PlotO (1,11) [] [])]
                 assertEqual (Right $ ObjO (1,1) [("result",ArrO (1,6) [PlotO (1,11) [] [],TableO (1,7) [] []])]) $ runWith "show([g(),f()])" [("f",TableO (1,7) [] []),("g",PlotO (1,11) [] [])]

                 assertEqual (Right $ ObjO (1,1) [("result",ArrO (1,6) [])])                                      $ runWith "show(f())" [("f",ArrO (1,6) [])]
                 assertEqual (Right $ ObjO (1,1) [("result",ArrO (1,6) [TableO (1,6) [] []])])                    $ runWith "show(f())" [("f",ArrO (1,6) [TableO (1,6) [] []])]
                 assertEqual (Right $ ObjO (1,1) [("result",ArrO (1,6) [PlotO  (1,6) [] []])])                    $ runWith "show(f())" [("f",ArrO (1,6) [PlotO ( 1,6) [] []])]

                 assertEqual (Right $ ObjO (1,1) [("result",ArrO (1,6) [TableO (1,6) [] [],PlotO  (1,6) [] []])]) $ runWith "show(f())" [("f",ArrO (1,6) [TableO (1,6) [] [],PlotO  (1,6) [] []])]
                 assertEqual (Right $ ObjO (1,1) [("result",ArrO (1,6) [PlotO  (1,6) [] [],TableO (1,6) [] []])]) $ runWith "show(f())" [("f",ArrO (1,6) [PlotO  (1,6) [] [],TableO (1,6) [] []])]

test_Multi = do  assertEqual (Right $ NumO (1,1) 1) $ run     "multi([1])"
                 assertEqual (Right $ NumO (1,1) 2) $ run     "multi([1,2])"
                 assertEqual (Right $ NumO (1,1) 6) $ run     "multi([1,2,3])"
                 assertEqual (Right $ NumO (1,1) 0) $ run     "multi([1,2,3,0])"

                 assertEqual (Right $ NumO (1,1) 1) $ runWith "multi([f()])"       [("f",numO 1)]
                 assertEqual (Right $ NumO (1,1) 2) $ runWith "multi([f(),2])"     [("f",numO 1)]
                 assertEqual (Right $ NumO (1,1) 6) $ runWith "multi([f(),2,3])"   [("f",numO 1)]
                 assertEqual (Right $ NumO (1,1) 0) $ runWith "multi([f(),2,3,0])" [("f",numO 1)]

                 assertEqual (Right $ NumO (1,1) 1) $ runWith "multi(f())" [("f",ArrO (1,8) [numO 1])]
                 assertEqual (Right $ NumO (1,1) 2) $ runWith "multi(f())" [("f",ArrO (1,8) [numO 1,numO 2])]
                 assertEqual (Right $ NumO (1,1) 6) $ runWith "multi(f())" [("f",ArrO (1,8) [numO 1,numO 2,numO 3])]
                 assertEqual (Right $ NumO (1,1) 0) $ runWith "multi(f())" [("f",ArrO (1,8) [numO 1,numO 2,numO 3,numO 0])]

test_Mean = do   assertEqual (Right $ NumO (1,1) 1) $ run     "mean([1])"
                 assertEqual (Right $ NumO (1,1) 2) $ run     "mean([1,3])"
                 assertEqual (Right $ NumO (1,1) 2) $ run     "mean([1,2,3])"
                 assertEqual (Right $ NumO (1,1) 0) $ run     "mean([1,2,3,-6])"

                 assertEqual (Right $ NumO (1,1) 1) $ runWith "mean([f()])"       [("f",numO 1)]
                 assertEqual (Right $ NumO (1,1) 2) $ runWith "mean([f(),3])"     [("f",numO 1)]
                 assertEqual (Right $ NumO (1,1) 2) $ runWith "mean([f(),2,3])"   [("f",numO 1)]
                 assertEqual (Right $ NumO (1,1) 0) $ runWith "mean([f(),2,3,-6])" [("f",numO 1)]

                 assertEqual (Right $ NumO (1,1) 1) $ runWith "mean(f())" [("f",ArrO (1,8) [numO 1])]
                 assertEqual (Right $ NumO (1,1) 2) $ runWith "mean(f())" [("f",ArrO (1,8) [numO 1,numO 3])]
                 assertEqual (Right $ NumO (1,1) 2) $ runWith "mean(f())" [("f",ArrO (1,8) [numO 1,numO 2,numO 3])]
                 assertEqual (Right $ NumO (1,1) 0) $ runWith "mean(f())" [("f",ArrO (1,8) [numO 1,numO 2,numO 3,numO (-6)])]

test_Desc = let  sk = (-1.0182337649086284); ku = (-0.7696000000000001) in do
                 assertEqual (show $ eval $ mkDesc (1,1) 1 1 1  0    (0/0) (0/0)) $ show $ run     "descriptive([1])"
                 assertEqual (       eval $ mkDesc (1,1) 2 4 2  1     0 (-2))            $ run     "descriptive([1,3])"
                 assertEqual (       eval $ mkDesc (1,1) 3 6 2 (2/3)  0 (-1.5))          $ run     "descriptive([1,2,3])"
                 assertEqual (       eval $ mkDesc (1,1) 4 0 0 12.5  sk  ku)             $ run     "descriptive([1,2,3,-6])"

                 assertEqual (show $ eval $ mkDesc (1,1) 1 1 1  0    (0/0) (0/0)) $ show $ runWith "descriptive([f()])"        [("f",numO 1)]
                 assertEqual (       eval $ mkDesc (1,1) 2 4 2  1     0 (-2))            $ runWith "descriptive([f(),3])"      [("f",numO 1)]
                 assertEqual (       eval $ mkDesc (1,1) 3 6 2 (2/3)  0 (-1.5))          $ runWith "descriptive([f(),2,3])"    [("f",numO 1)]
                 assertEqual (       eval $ mkDesc (1,1) 4 0 0 12.5  sk  ku)             $ runWith "descriptive([f(),2,3,-6])" [("f",numO 1)]

                 assertEqual (show $ eval $ mkDesc (1,1) 1 1 1  0    (0/0) (0/0)) $ show $ runWith "descriptive(f())" [("f",ArrO (1,8) [numO 1])]
                 assertEqual (       eval $ mkDesc (1,1) 2 4 2  1     0 (-2))            $ runWith "descriptive(f())" [("f",ArrO (1,8) [numO 1,numO 3])]
                 assertEqual (       eval $ mkDesc (1,1) 3 6 2 (2/3)  0 (-1.5))          $ runWith "descriptive(f())" [("f",ArrO (1,8) [numO 1,numO 2,numO 3])]
                 assertEqual (       eval $ mkDesc (1,1) 4 0 0 12.5  sk  ku)             $ runWith "descriptive(f())" [("f",ArrO (1,8) [numO 1,numO 2,numO 3,numO (-6)])]

test_Table = do  assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0]] [])                                                $ run     "table([[0]],{})"
                 assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0]] [])                                                $ run     "table([[0]],{not:[]})"
                 assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0]] [StrO (1,19) "1"])                                 $ run     "table([[0]],{col:[\"1\"]})"
                 assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0]] [StrO (1,26) "1"])                                 $ run     "table([[0]],{not:[],col:[\"1\"]})"

                 assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0],[NumO (1,13) 1]] [])                                $ run     "table([[0],[1]],{})"
                 assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0],[NumO (1,13) 1]] [])                                $ run     "table([[0],[1]],{not:[]})"
                 assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0],[NumO (1,13) 1]] [StrO (1,23) "1",StrO (1,27) "2"]) $ run     "table([[0],[1]],{col:[\"1\",\"2\"]})"
                 assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0],[NumO (1,13) 1]] [StrO (1,30) "1",StrO (1,34) "2"]) $ run     "table([[0],[1]],{not:[],col:[\"1\",\"2\"]})"

                 assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0]] [])                                                $ runWith "table(f(),{})"                         [("f",ArrO (0,0) [ArrO (0,0) [NumO (1,9) 0]])]
                 assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0]] [])                                                $ runWith "table(f(),{not:[]})"                   [("f",ArrO (0,0) [ArrO (0,0) [NumO (1,9) 0]])]
                 assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0]] [StrO (1,17) "1"])                                 $ runWith "table(f(),{col:[\"1\"]})"              [("f",ArrO (0,0) [ArrO (0,0) [NumO (1,9) 0]])]
                 assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0]] [StrO (1,24) "1"])                                 $ runWith "table(f(),{not:[],col:[\"1\"]})"       [("f",ArrO (0,0) [ArrO (0,0) [NumO (1,9) 0]])]

                 assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0],[NumO (1,13) 1]] [])                                $ runWith "table(f(),{})"                         [("f",ArrO (0,0) [ArrO (0,0) [NumO (1,9) 0],ArrO (0,0) [NumO (1,13) 1]])]
                 assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0],[NumO (1,13) 1]] [])                                $ runWith "table(f(),{not:[]})"                   [("f",ArrO (0,0) [ArrO (0,0) [NumO (1,9) 0],ArrO (0,0) [NumO (1,13) 1]])]
                 assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0],[NumO (1,13) 1]] [StrO (1,17) "1",StrO (1,21) "2"]) $ runWith "table(f(),{col:[\"1\",\"2\"]})"        [("f",ArrO (0,0) [ArrO (0,0) [NumO (1,9) 0],ArrO (0,0) [NumO (1,13) 1]])]
                 assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0],[NumO (1,13) 1]] [StrO (1,24) "1",StrO (1,28) "2"]) $ runWith "table(f(),{not:[],col:[\"1\",\"2\"]})" [("f",ArrO (0,0) [ArrO (0,0) [NumO (1,9) 0],ArrO (0,0) [NumO (1,13) 1]])]

                 assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0]] [])                                                $ runWith "table([[0]],f())"     [("f",ObjO (0,0) [])]
                 assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0]] [])                                                $ runWith "table([[0]],f())"     [("f",ObjO (0,0) [("not",ArrO (0,0) [])])]
                 assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0]] [StrO (1,19) "1"])                                 $ runWith "table([[0]],f())"     [("f",ObjO (0,0) [("col",ArrO (0,0) [StrO (1,19) "1"])])]
                 assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0]] [StrO (1,26) "1"])                                 $ runWith "table([[0]],f())"     [("f",ObjO (0,0) [("not",ArrO (0,0) []),("col",ArrO (0,0) [StrO (1,26) "1"])])]

                 assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0],[NumO (1,13) 1]] [])                                $ runWith "table([[0],[1]],f())" [("f",ObjO (0,0) [])]
                 assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0],[NumO (1,13) 1]] [])                                $ runWith "table([[0],[1]],f())" [("f",ObjO (0,0) [("not",ArrO (0,0) [])])]
                 assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0],[NumO (1,13) 1]] [StrO (1,23) "1",StrO (1,27) "2"]) $ runWith "table([[0],[1]],f())" [("f",ObjO (0,0) [("col",ArrO (0,0) [StrO (1,23) "1",StrO (1,27) "2"])])]
                 assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0],[NumO (1,13) 1]] [StrO (1,34) "1",StrO (1,38) "2"]) $ runWith "table([[0],[1]],f())" [("f",ObjO (0,0) [("not",ArrO (0,0) []),("col",ArrO (0,0) [StrO (1,34) "1",StrO (1,38) "2"])])]

                 assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0]] [])                                                $ runWith "table(f(),g())" [("f",ArrO (0,0) [ArrO (0,0) [NumO (1,9) 0]]),                             ("g",ObjO (0,0) [])]
                 assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0]] [])                                                $ runWith "table(g(),f())" [("g",ArrO (0,0) [ArrO (0,0) [NumO (1,9) 0]]),                             ("f",ObjO (0,0) [("not",ArrO (0,0) [])])]
                 assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0]] [StrO (1,17) "1"])                                 $ runWith "table(f(),g())" [("f",ArrO (0,0) [ArrO (0,0) [NumO (1,9) 0]]),                             ("g",ObjO (0,0) [("col",ArrO (0,0) [StrO (1,17) "1"])])]
                 assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0]] [StrO (1,28) "1"])                                 $ runWith "table(g(),f())" [("g",ArrO (0,0) [ArrO (0,0) [NumO (1,9) 0]]),                             ("f",ObjO (0,0) [("not",ArrO (0,0) []),("col",ArrO (0,0) [StrO (1,28) "1"])])]

                 assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0],[NumO (1,13) 1]] [])                                $ runWith "table(f(),g())" [("f",ArrO (0,0) [ArrO (0,0) [NumO (1,9) 0],ArrO (0,0) [NumO (1,13) 1]]),  ("g",ObjO (0,0) [])]
                 assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0],[NumO (1,13) 1]] [])                                $ runWith "table(g(),f())" [("g",ArrO (0,0) [ArrO (0,0) [NumO (1,9) 0],ArrO (0,0) [NumO (1,13) 1]]),  ("f",ObjO (0,0) [("not",ArrO (0,0) [])])]
                 assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0],[NumO (1,13) 1]] [StrO (1,17) "1",StrO (1,21) "2"]) $ runWith "table(f(),g())" [("f",ArrO (0,0) [ArrO (0,0) [NumO (1,9) 0],ArrO (0,0) [NumO (1,13) 1]]),  ("g",ObjO (0,0) [("col",ArrO (0,0) [StrO (1,17) "1",StrO (1,21) "2"])])]
                 assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0],[NumO (1,13) 1]] [StrO (1,28) "1",StrO (1,32) "2"]) $ runWith "table(g(),f())" [("g",ArrO (0,0) [ArrO (0,0) [NumO (1,9) 0],ArrO (0,0) [NumO (1,13) 1]]),  ("f",ObjO (0,0) [("not",ArrO (0,0) []),("col",ArrO (0,0) [StrO (1,28) "1",StrO (1,32) "2"])])]

test_NTimes = do assertEqual (Right $ ArrO (1,1) [])                           $ run     "nTimes(1,0)"
                 assertEqual (Right $ ArrO (1,1) [NumO (1,8) 0])               $ run     "nTimes(0,1)"
                 assertEqual (Right $ ArrO (1,1) [NumO (1,8) 1, NumO (1,8) 1]) $ run     "nTimes(1,2)"

                 assertEqual (Right $ ArrO (1,1) [])                           $ runWith "nTimes(f(),0)" [("f",NumO (1,8) 1)]
                 assertEqual (Right $ ArrO (1,1) [NumO (1,8) 0])               $ runWith "nTimes(f(),1)" [("f",NumO (1,8) 0)]
                 assertEqual (Right $ ArrO (1,1) [NumO (1,8) 1, NumO (1,8) 1]) $ runWith "nTimes(f(),2)" [("f",NumO (1,8) 1)]

                 assertEqual (Right $ ArrO (1,1) [])                           $ runWith "nTimes(1,f())" [("f",NumO (0,0) 0)]
                 assertEqual (Right $ ArrO (1,1) [NumO (1,8) 0])               $ runWith "nTimes(0,f())" [("f",NumO (0,0) 1)]
                 assertEqual (Right $ ArrO (1,1) [NumO (1,8) 1, NumO (1,8) 1]) $ runWith "nTimes(1,f())" [("f",NumO (0,0) 2)]

                 assertEqual (Right $ ArrO (1,1) [])                           $ runWith "nTimes(f(),g())" [("f",NumO (1,8) 1),("g",NumO (0,0) 0)]
                 assertEqual (Right $ ArrO (1,1) [NumO (1,8) 0])               $ runWith "nTimes(g(),f())" [("g",NumO (1,8) 0),("f",NumO (0,0) 1)]
                 assertEqual (Right $ ArrO (1,1) [NumO (1,8) 1, NumO (1,8) 1]) $ runWith "nTimes(f(),g())" [("f",NumO (1,8) 1),("g",NumO (0,0) 2)]

test_Take = do   assertEqual (Right $ TableO (1,1) [] [])                                                        $ runWith "take(1,f())" [("f",TableO (0,0) [] [])]
                 assertEqual (Right $ TableO (1,1) [[NumO (0,0) 0]] [])                                          $ runWith "take(1,f())" [("f",TableO (0,0) [[NumO (0,0) 0]] [])]
                 assertEqual (Right $ TableO (1,1) [[NumO (0,0) 0]] [])                                          $ runWith "take(2,f())" [("f",TableO (0,0) [[NumO (0,0) 0]] [])]
                 assertEqual (Right $ TableO (1,1) [[NumO (0,0) 0]] [])                                          $ runWith "take(3,f())" [("f",TableO (0,0) [[NumO (0,0) 0]] [])]
                 assertEqual (Right $ TableO (1,1) [[NumO (0,0) 0],[NumO (1,0) 0]] [])                           $ runWith "take(1,f())" [("f",TableO (0,0) [[NumO (0,0) 0,NumO (0,1) 1],[NumO (1,0) 0,NumO (1,1) 1]] [])]
                 assertEqual (Right $ TableO (1,1) [[NumO (0,0) 0,NumO (0,1) 1],[NumO (1,0) 0,NumO (1,1) 1]] []) $ runWith "take(2,f())" [("f",TableO (0,0) [[NumO (0,0) 0,NumO (0,1) 1],[NumO (1,0) 0,NumO (1,1) 1]] [])]
                 assertEqual (Right $ TableO (1,1) [[NumO (0,0) 0,NumO (0,1) 1],[NumO (1,0) 0,NumO (1,1) 1]] []) $ runWith "take(3,f())" [("f",TableO (0,0) [[NumO (0,0) 0,NumO (0,1) 1],[NumO (1,0) 0,NumO (1,1) 1]] [])]

                 assertEqual (Right $ TableO (1,1) [] [])                                                        $ runWith "take(f(),g())" [("f",NumO (0,0) 1),("g",TableO (0,0) [] [])]
                 assertEqual (Right $ TableO (1,1) [[NumO (0,0) 0]] [])                                          $ runWith "take(f(),g())" [("f",NumO (0,0) 1),("g",TableO (0,0) [[NumO (0,0) 0]] [])]
                 assertEqual (Right $ TableO (1,1) [[NumO (0,0) 0]] [])                                          $ runWith "take(f(),g())" [("f",NumO (0,0) 2),("g",TableO (0,0) [[NumO (0,0) 0]] [])]
                 assertEqual (Right $ TableO (1,1) [[NumO (0,0) 0]] [])                                          $ runWith "take(f(),g())" [("f",NumO (0,0) 3),("g",TableO (0,0) [[NumO (0,0) 0]] [])]
                 assertEqual (Right $ TableO (1,1) [[NumO (0,0) 0],[NumO (1,0) 0]] [])                           $ runWith "take(f(),g())" [("f",NumO (0,0) 1),("g",TableO (0,0) [[NumO (0,0) 0,NumO (0,1) 1],[NumO (1,0) 0,NumO (1,1) 1]] [])]
                 assertEqual (Right $ TableO (1,1) [[NumO (0,0) 0,NumO (0,1) 1],[NumO (1,0) 0,NumO (1,1) 1]] []) $ runWith "take(f(),g())" [("f",NumO (0,0) 2),("g",TableO (0,0) [[NumO (0,0) 0,NumO (0,1) 1],[NumO (1,0) 0,NumO (1,1) 1]] [])]
                 assertEqual (Right $ TableO (1,1) [[NumO (0,0) 0,NumO (0,1) 1],[NumO (1,0) 0,NumO (1,1) 1]] []) $ runWith "take(f(),g())" [("f",NumO (0,0) 3),("g",TableO (0,0) [[NumO (0,0) 0,NumO (0,1) 1],[NumO (1,0) 0,NumO (1,1) 1]] [])]

                 assertEqual (Right $ ArrO (1,1) [])                            $ run     "take(0,[])"
                 assertEqual (Right $ ArrO (1,1) [])                            $ run     "take(1,[])"
                 assertEqual (Right $ ArrO (1,1) [])                            $ run     "take(0,[0])"
                 assertEqual (Right $ ArrO (1,1) [NumO (1,9) 0])                $ run     "take(1,[0])"
                 assertEqual (Right $ ArrO (1,1) [NumO (1,9) 0])                $ run     "take(2,[0])"
                 assertEqual (Right $ ArrO (1,1) [])                            $ run     "take(0,[0,1])"
                 assertEqual (Right $ ArrO (1,1) [NumO (1,9) 0])                $ run     "take(1,[0,1])"
                 assertEqual (Right $ ArrO (1,1) [NumO (1,9) 0,NumO (1,11) 1])  $ run     "take(2,[0,1])"
                 assertEqual (Right $ ArrO (1,1) [NumO (1,9) 0,NumO (1,11) 1])  $ run     "take(3,[0,1])"

                 assertEqual (Right $ ArrO (1,1) [])                            $ runWith "take(f(),[])"    [("f",NumO (0,0) 0)]
                 assertEqual (Right $ ArrO (1,1) [])                            $ runWith "take(f(),[])"    [("f",NumO (0,0) 1)]
                 assertEqual (Right $ ArrO (1,1) [])                            $ runWith "take(f(),[0])"   [("f",NumO (0,0) 0)]
                 assertEqual (Right $ ArrO (1,1) [NumO (1,11) 0])               $ runWith "take(f(),[0])"   [("f",NumO (0,0) 1)]
                 assertEqual (Right $ ArrO (1,1) [NumO (1,11) 0])               $ runWith "take(f(),[0])"   [("f",NumO (0,0) 2)]
                 assertEqual (Right $ ArrO (1,1) [])                            $ runWith "take(f(),[0,1])" [("f",NumO (0,0) 0)]
                 assertEqual (Right $ ArrO (1,1) [NumO (1,11) 0])               $ runWith "take(f(),[0,1])" [("f",NumO (0,0) 1)]
                 assertEqual (Right $ ArrO (1,1) [NumO (1,11) 0,NumO (1,13) 1]) $ runWith "take(f(),[0,1])" [("f",NumO (0,0) 2)]
                 assertEqual (Right $ ArrO (1,1) [NumO (1,11) 0,NumO (1,13) 1]) $ runWith "take(f(),[0,1])" [("f",NumO (0,0) 3)]

                 assertEqual (Right $ ArrO (1,1) [])                            $ runWith "take(0,f())"     [("f",ArrO (0,0) [])]
                 assertEqual (Right $ ArrO (1,1) [])                            $ runWith "take(1,f())"     [("f",ArrO (0,0) [])]
                 assertEqual (Right $ ArrO (1,1) [])                            $ runWith "take(0,f())"     [("f",ArrO (0,0) [NumO (1,9) 0])]
                 assertEqual (Right $ ArrO (1,1) [NumO (1,9) 0])                $ runWith "take(1,f())"     [("f",ArrO (0,0) [NumO (1,9) 0])]
                 assertEqual (Right $ ArrO (1,1) [NumO (1,9) 0])                $ runWith "take(2,f())"     [("f",ArrO (0,0) [NumO (1,9) 0])]
                 assertEqual (Right $ ArrO (1,1) [])                            $ runWith "take(0,f())"     [("f",ArrO (0,0) [NumO (1,9) 0,NumO (1,11) 1])]
                 assertEqual (Right $ ArrO (1,1) [NumO (1,9) 0])                $ runWith "take(1,f())"     [("f",ArrO (0,0) [NumO (1,9) 0,NumO (1,11) 1])]
                 assertEqual (Right $ ArrO (1,1) [NumO (1,9) 0,NumO (1,11) 1])  $ runWith "take(2,f())"     [("f",ArrO (0,0) [NumO (1,9) 0,NumO (1,11) 1])]
                 assertEqual (Right $ ArrO (1,1) [NumO (1,9) 0,NumO (1,11) 1])  $ runWith "take(3,f())"     [("f",ArrO (0,0) [NumO (1,9) 0,NumO (1,11) 1])]

                 assertEqual (Right $ ArrO (1,1) [])                            $ runWith "take(f(),g())"   [("f",NumO (0,0) 0),("g",ArrO (0,0) [])]
                 assertEqual (Right $ ArrO (1,1) [])                            $ runWith "take(f(),g())"   [("f",NumO (0,0) 1),("g",ArrO (0,0) [])]
                 assertEqual (Right $ ArrO (1,1) [])                            $ runWith "take(f(),g())"   [("f",NumO (0,0) 0),("g",ArrO (0,0) [NumO (1,11) 0])]
                 assertEqual (Right $ ArrO (1,1) [NumO (1,11) 0])               $ runWith "take(f(),g())"   [("f",NumO (0,0) 1),("g",ArrO (0,0) [NumO (1,11) 0])]
                 assertEqual (Right $ ArrO (1,1) [NumO (1,11) 0])               $ runWith "take(f(),g())"   [("f",NumO (0,0) 2),("g",ArrO (0,0) [NumO (1,11) 0])]
                 assertEqual (Right $ ArrO (1,1) [])                            $ runWith "take(f(),g())"   [("f",NumO (0,0) 0),("g",ArrO (0,0) [NumO (1,11) 0,NumO (1,13) 1])]
                 assertEqual (Right $ ArrO (1,1) [NumO (1,11) 0])               $ runWith "take(f(),g())"   [("f",NumO (0,0) 1),("g",ArrO (0,0) [NumO (1,11) 0,NumO (1,13) 1])]
                 assertEqual (Right $ ArrO (1,1) [NumO (1,11) 0,NumO (1,13) 1]) $ runWith "take(f(),g())"   [("f",NumO (0,0) 2),("g",ArrO (0,0) [NumO (1,11) 0,NumO (1,13) 1])]
                 assertEqual (Right $ ArrO (1,1) [NumO (1,11) 0,NumO (1,13) 1]) $ runWith "take(f(),g())"   [("f",NumO (0,0) 3),("g",ArrO (0,0) [NumO (1,11) 0,NumO (1,13) 1])]

test_Sort = do   assertEqual (Right $ TableO (1,1) [[NumO (0,0) 0],[NumO (0,1) 1]] [])                                                     $ runWith "sort(0,f())" [("f",TableO (0,0) [[NumO (0,0) 0],[NumO (0,1) 1]] [])]
                 assertEqual (Right $ TableO (1,1) [[NumO (0,0) 0],[NumO (0,1) 1]] [])                                                     $ runWith "sort(1,f())" [("f",TableO (0,0) [[NumO (0,0) 0],[NumO (0,1) 1]] [])]
                 assertEqual (Right $ TableO (1,1) [[NumO (0,1) 0,NumO (0,0) 1,NumO (0,2) 2],[NumO (1,1) 2,NumO (1,0) 3,NumO (1,2) 1]] []) $ runWith "sort(0,f())" [("f",TableO (0,0) [[NumO (0,0) 1,NumO (0,1) 0,NumO (0,2) 2],[NumO (1,0) 3,NumO (1,1) 2,NumO (1,2) 1]] [])]
                 assertEqual (Right $ TableO (1,1) [[NumO (0,2) 2,NumO (0,1) 0,NumO (0,0) 1],[NumO (1,2) 1,NumO (1,1) 2,NumO (1,0) 3]] []) $ runWith "sort(1,f())" [("f",TableO (0,0) [[NumO (0,0) 1,NumO (0,1) 0,NumO (0,2) 2],[NumO (1,0) 3,NumO (1,1) 2,NumO (1,2) 1]] [])]

                 assertEqual (Right $ TableO (1,1) [[NumO (0,0) 0],[NumO (0,1) 1]] [])                                                     $ runWith "sort(f(),g())" [("f",NumO (1,6) 0),("g",TableO (0,0) [[NumO (0,0) 0],[NumO (0,1) 1]] [])]
                 assertEqual (Right $ TableO (1,1) [[NumO (0,0) 0],[NumO (0,1) 1]] [])                                                     $ runWith "sort(f(),g())" [("f",NumO (1,6) 1),("g",TableO (0,0) [[NumO (0,0) 0],[NumO (0,1) 1]] [])]
                 assertEqual (Right $ TableO (1,1) [[NumO (0,1) 0,NumO (0,0) 1,NumO (0,2) 2],[NumO (1,1) 2,NumO (1,0) 3,NumO (1,2) 1]] []) $ runWith "sort(f(),g())" [("f",NumO (1,6) 0),("g",TableO (0,0) [[NumO (0,0) 1,NumO (0,1) 0,NumO (0,2) 2],[NumO (1,0) 3,NumO (1,1) 2,NumO (1,2) 1]] [])]
                 assertEqual (Right $ TableO (1,1) [[NumO (0,2) 2,NumO (0,1) 0,NumO (0,0) 1],[NumO (1,2) 1,NumO (1,1) 2,NumO (1,0) 3]] []) $ runWith "sort(f(),g())" [("f",NumO (1,6) 1),("g",TableO (0,0) [[NumO (0,0) 1,NumO (0,1) 0,NumO (0,2) 2],[NumO (1,0) 3,NumO (1,1) 2,NumO (1,2) 1]] [])]

                 assertEqual (Right $ ArrO (1,1) [ArrO (1,9) [NumO (1,10) 0],                            ArrO (1,13) [NumO (1,14) 1]])                             $ run     "sort(0,[[0],[1]])"
                 assertEqual (Right $ ArrO (1,1) [ArrO (1,9) [NumO (1,10) 0],                            ArrO (1,13) [NumO (1,14) 1]])                             $ run     "sort(1,[[0],[1]])"
                 assertEqual (Right $ ArrO (1,1) [ArrO (1,9) [NumO (1,12) 0,NumO (1,10) 1,NumO (1,14) 2],ArrO (1,17) [NumO (1,20) 2,NumO (1,18) 3,NumO (1,22) 1]]) $ run     "sort(0,[[1,0,2],[3,2,1]])"
                 assertEqual (Right $ ArrO (1,1) [ArrO (1,9) [NumO (1,14) 2,NumO (1,12) 0,NumO (1,10) 1],ArrO (1,17) [NumO (1,22) 1,NumO (1,20) 2,NumO (1,18) 3]]) $ run     "sort(1,[[1,0,2],[3,2,1]])"

                 assertEqual (Right $ ArrO (1,1) [ArrO (1,9) [NumO (1,10) 0],                            ArrO (1,13) [NumO (1,14) 1]])                             $ runWith "sort(0,f())"                 [("f",ArrO (0,0) [ArrO (1,9) [NumO (1,10) 0],                            ArrO (1,13) [NumO (1,14) 1]])]
                 assertEqual (Right $ ArrO (1,1) [ArrO (1,9) [NumO (1,10) 0],                            ArrO (1,13) [NumO (1,14) 1]])                             $ runWith "sort(1,f())"                 [("f",ArrO (0,0) [ArrO (1,9) [NumO (1,10) 0],                            ArrO (1,13) [NumO (1,14) 1]])]
                 assertEqual (Right $ ArrO (1,1) [ArrO (1,9) [NumO (1,12) 0,NumO (1,10) 1,NumO (1,14) 2],ArrO (1,17) [NumO (1,20) 2,NumO (1,18) 3,NumO (1,22) 1]]) $ runWith "sort(0,f())"                 [("f",ArrO (0,0) [ArrO (1,9) [NumO (1,10) 1,NumO (1,12) 0,NumO (1,14) 2],ArrO (1,17) [NumO (1,18) 3,NumO (1,20) 2,NumO (1,22) 1]])]
                 assertEqual (Right $ ArrO (1,1) [ArrO (1,9) [NumO (1,14) 2,NumO (1,12) 0,NumO (1,10) 1],ArrO (1,17) [NumO (1,22) 1,NumO (1,20) 2,NumO (1,18) 3]]) $ runWith "sort(1,f())"                 [("f",ArrO (0,0) [ArrO (1,9) [NumO (1,10) 1,NumO (1,12) 0,NumO (1,14) 2],ArrO (1,17) [NumO (1,18) 3,NumO (1,20) 2,NumO (1,22) 1]])]

                 assertEqual (Right $ ArrO (1,1) [ArrO (1,11) [NumO (1,12) 0],                             ArrO (1,15) [NumO (1,16) 1]])                            $ runWith "sort(f(),[[0],[1]])"         [("f",NumO (0,0) 0)]
                 assertEqual (Right $ ArrO (1,1) [ArrO (1,11) [NumO (1,12) 0],                             ArrO (1,15) [NumO (1,16) 1]])                            $ runWith "sort(f(),[[0],[1]])"         [("f",NumO (0,0) 1)]
                 assertEqual (Right $ ArrO (1,1) [ArrO (1,11) [NumO (1,14) 0,NumO (1,12) 1,NumO (1,16) 2],ArrO (1,19) [NumO (1,22) 2,NumO (1,20) 3,NumO (1,24) 1]]) $ runWith "sort(f(),[[1,0,2],[3,2,1]])" [("f",NumO (0,0) 0)]
                 assertEqual (Right $ ArrO (1,1) [ArrO (1,11) [NumO (1,16) 2,NumO (1,14) 0,NumO (1,12) 1],ArrO (1,19) [NumO (1,24) 1,NumO (1,22) 2,NumO (1,20) 3]]) $ runWith "sort(f(),[[1,0,2],[3,2,1]])" [("f",NumO (0,0) 1)]

                 assertEqual (Right $ ArrO (1,1) [ArrO (1,9) [NumO (1,10) 0],                            ArrO (1,13) [NumO (1,14) 1]])                             $ runWith "sort(f(),g())"               [("f",NumO (0,0) 0),("g",ArrO (1,1) [ArrO (1,9) [NumO (1,10) 0],                            ArrO (1,13) [NumO (1,14) 1]])]
                 assertEqual (Right $ ArrO (1,1) [ArrO (1,9) [NumO (1,10) 0],                            ArrO (1,13) [NumO (1,14) 1]])                             $ runWith "sort(f(),g())"               [("f",NumO (0,0) 1),("g",ArrO (1,1) [ArrO (1,9) [NumO (1,10) 0],                            ArrO (1,13) [NumO (1,14) 1]])]
                 assertEqual (Right $ ArrO (1,1) [ArrO (1,9) [NumO (1,12) 0,NumO (1,10) 1,NumO (1,14) 2],ArrO (1,17) [NumO (1,20) 2,NumO (1,18) 3,NumO (1,22) 1]]) $ runWith "sort(f(),g())"               [("f",NumO (0,0) 0),("g",ArrO (1,1) [ArrO (1,9) [NumO (1,10) 1,NumO (1,12) 0,NumO (1,14) 2],ArrO (1,17) [NumO (1,18) 3,NumO (1,20) 2,NumO (1,22) 1]])]
                 assertEqual (Right $ ArrO (1,1) [ArrO (1,9) [NumO (1,14) 2,NumO (1,12) 0,NumO (1,10) 1],ArrO (1,17) [NumO (1,22) 1,NumO (1,20) 2,NumO (1,18) 3]]) $ runWith "sort(f(),g())"               [("f",NumO (0,0) 1),("g",ArrO (1,1) [ArrO (1,9) [NumO (1,10) 1,NumO (1,12) 0,NumO (1,14) 2],ArrO (1,17) [NumO (1,18) 3,NumO (1,20) 2,NumO (1,22) 1]])]

test_Col = do    assertEqual (Right $ ArrO (1,1) [NumO (0,0) 0])                              $ runWith "col(0,f())" [("f",TableO (0,0) [[NumO (0,0) 0],[NumO (0,1) 1]] [])]
                 assertEqual (Right $ ArrO (1,1) [NumO (0,1) 1])                              $ runWith "col(1,f())" [("f",TableO (0,0) [[NumO (0,0) 0],[NumO (0,1) 1]] [])]
                 assertEqual (Right $ ArrO (1,1) [NumO (0,0) 1,NumO (0,1) 0,NumO (0,2) 2])    $ runWith "col(0,f())" [("f",TableO (0,0) [[NumO (0,0) 1,NumO (0,1) 0,NumO (0,2) 2],[NumO (1,0) 3,NumO (1,1) 2,NumO (1,2) 1]] [])]
                 assertEqual (Right $ ArrO (1,1) [NumO (1,0) 3,NumO (1,1) 2,NumO (1,2) 1])    $ runWith "col(1,f())" [("f",TableO (0,0) [[NumO (0,0) 1,NumO (0,1) 0,NumO (0,2) 2],[NumO (1,0) 3,NumO (1,1) 2,NumO (1,2) 1]] [])]

                 assertEqual (Right $ ArrO (1,1) [NumO (0,0) 0])                              $ runWith "col(f(),g())" [("f",NumO (1,6) 0),("g",TableO (0,0) [[NumO (0,0) 0],[NumO (0,1) 1]] [])]
                 assertEqual (Right $ ArrO (1,1) [NumO (0,1) 1])                              $ runWith "col(f(),g())" [("f",NumO (1,6) 1),("g",TableO (0,0) [[NumO (0,0) 0],[NumO (0,1) 1]] [])]
                 assertEqual (Right $ ArrO (1,1) [NumO (0,0) 1,NumO (0,1) 0,NumO (0,2) 2])    $ runWith "col(f(),g())" [("f",NumO (1,6) 0),("g",TableO (0,0) [[NumO (0,0) 1,NumO (0,1) 0,NumO (0,2) 2],[NumO (1,0) 3,NumO (1,1) 2,NumO (1,2) 1]] [])]
                 assertEqual (Right $ ArrO (1,1) [NumO (1,0) 3,NumO (1,1) 2,NumO (1,2) 1])    $ runWith "col(f(),g())" [("f",NumO (1,6) 1),("g",TableO (0,0) [[NumO (0,0) 1,NumO (0,1) 0,NumO (0,2) 2],[NumO (1,0) 3,NumO (1,1) 2,NumO (1,2) 1]] [])]

                 assertEqual (Right $ ArrO (1,1) [NumO (1,9)  0])                             $ run     "col(0,[[0],[1]])"
                 assertEqual (Right $ ArrO (1,1) [NumO (1,13) 1])                             $ run     "col(1,[[0],[1]])"
                 assertEqual (Right $ ArrO (1,1) [NumO (1,9)  1,NumO (1,11) 0,NumO (1,13) 2]) $ run     "col(0,[[1,0,2],[3,2,1]])"
                 assertEqual (Right $ ArrO (1,1) [NumO (1,17) 3,NumO (1,19) 2,NumO (1,21) 1]) $ run     "col(1,[[1,0,2],[3,2,1]])"

                 assertEqual (Right $ ArrO (1,1) [NumO (1,10) 0])                             $ runWith "col(0,f())"                 [("f",ArrO (0,0) [ArrO (1,9) [NumO (1,10) 0],                            ArrO (1,13) [NumO (1,14) 1]])]
                 assertEqual (Right $ ArrO (1,1) [NumO (1,14) 1])                             $ runWith "col(1,f())"                 [("f",ArrO (0,0) [ArrO (1,9) [NumO (1,10) 0],                            ArrO (1,13) [NumO (1,14) 1]])]
                 assertEqual (Right $ ArrO (1,1) [NumO (1,10) 1,NumO (1,12) 0,NumO (1,14) 2]) $ runWith "col(0,f())"                 [("f",ArrO (0,0) [ArrO (1,9) [NumO (1,10) 1,NumO (1,12) 0,NumO (1,14) 2],ArrO (1,17) [NumO (1,18) 3,NumO (1,20) 2,NumO (1,22) 1]])]
                 assertEqual (Right $ ArrO (1,1) [NumO (1,18) 3,NumO (1,20) 2,NumO (1,22) 1]) $ runWith "col(1,f())"                 [("f",ArrO (0,0) [ArrO (1,9) [NumO (1,10) 1,NumO (1,12) 0,NumO (1,14) 2],ArrO (1,17) [NumO (1,18) 3,NumO (1,20) 2,NumO (1,22) 1]])]

                 assertEqual (Right $ ArrO (1,1) [NumO (1,11) 0])                             $ runWith "col(f(),[[0],[1]])"         [("f",NumO (0,0) 0)]
                 assertEqual (Right $ ArrO (1,1) [NumO (1,15) 1])                             $ runWith "col(f(),[[0],[1]])"         [("f",NumO (0,0) 1)]
                 assertEqual (Right $ ArrO (1,1) [NumO (1,11) 1,NumO (1,13) 0,NumO (1,15) 2]) $ runWith "col(f(),[[1,0,2],[3,2,1]])" [("f",NumO (0,0) 0)]
                 assertEqual (Right $ ArrO (1,1) [NumO (1,19) 3,NumO (1,21) 2,NumO (1,23) 1]) $ runWith "col(f(),[[1,0,2],[3,2,1]])" [("f",NumO (0,0) 1)]

                 assertEqual (Right $ ArrO (1,1) [NumO (1,10) 0])                             $ runWith "col(f(),g())"               [("f",NumO (0,0) 0),("g",ArrO (1,1) [ArrO (1,9) [NumO (1,10) 0],                            ArrO (1,13) [NumO (1,14) 1]])]
                 assertEqual (Right $ ArrO (1,1) [NumO (1,14) 1])                             $ runWith "col(f(),g())"               [("f",NumO (0,0) 1),("g",ArrO (1,1) [ArrO (1,9) [NumO (1,10) 0],                            ArrO (1,13) [NumO (1,14) 1]])]
                 assertEqual (Right $ ArrO (1,1) [NumO (1,10) 1,NumO (1,12) 0,NumO (1,14) 2]) $ runWith "col(f(),g())"               [("f",NumO (0,0) 0),("g",ArrO (1,1) [ArrO (1,9) [NumO (1,10) 1,NumO (1,12) 0,NumO (1,14) 2],ArrO (1,17) [NumO (1,18) 3,NumO (1,20) 2,NumO (1,22) 1]])]
                 assertEqual (Right $ ArrO (1,1) [NumO (1,18) 3,NumO (1,20) 2,NumO (1,22) 1]) $ runWith "col(f(),g())"               [("f",NumO (0,0) 1),("g",ArrO (1,1) [ArrO (1,9) [NumO (1,10) 1,NumO (1,12) 0,NumO (1,14) 2],ArrO (1,17) [NumO (1,18) 3,NumO (1,20) 2,NumO (1,22) 1]])]
