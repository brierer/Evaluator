{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Engine.EngineUnitSuccess where

import Test.Framework

import Engine.EngineUnitSuccessUtils
import Engine.EngineUnitUtils
import Marshall.MarshallUtils

{-# ANN module "HLint: ignore Use camelCase" #-}

test_Show = do   assertEqual (Right $ mkObjO (1,1) [("result",mkArrO (1,6) [])])                                      $ run     "show([])"
                 assertEqual (Right $ mkObjO (1,1) [("result",mkArrO (1,6) [mkTableO (1,7) [] []])])                    $ runWith "show([f()])" [("f",mkTableO (1,7) [] [])]
                 assertEqual (Right $ mkObjO (1,1) [("result",mkArrO (1,6) [mkPlotO (1,11) [] []])])                    $ runWith "show([f()])" [("f",mkPlotO (1,11) [] [])]

                 assertEqual (Right $ mkObjO (1,1) [("result",mkArrO (1,6) [mkTableO (1,7) [] [],mkPlotO (1,11) [] []])]) $ runWith "show([f(),g()])" [("f",mkTableO (1,7) [] []),("g",mkPlotO (1,11) [] [])]
                 assertEqual (Right $ mkObjO (1,1) [("result",mkArrO (1,6) [mkPlotO (1,11) [] [],mkTableO (1,7) [] []])]) $ runWith "show([g(),f()])" [("f",mkTableO (1,7) [] []),("g",mkPlotO (1,11) [] [])]

                 assertEqual (Right $ mkObjO (1,1) [("result",mkArrO (1,6) [])])                                      $ runWith "show(f())" [("f",mkArrO (1,6) [])]
                 assertEqual (Right $ mkObjO (1,1) [("result",mkArrO (1,6) [mkTableO (1,6) [] []])])                    $ runWith "show(f())" [("f",mkArrO (1,6) [mkTableO (1,6) [] []])]
                 assertEqual (Right $ mkObjO (1,1) [("result",mkArrO (1,6) [mkPlotO  (1,6) [] []])])                    $ runWith "show(f())" [("f",mkArrO (1,6) [mkPlotO ( 1,6) [] []])]

                 assertEqual (Right $ mkObjO (1,1) [("result",mkArrO (1,6) [mkTableO (1,6) [] [],mkPlotO  (1,6) [] []])]) $ runWith "show(f())" [("f",mkArrO (1,6) [mkTableO (1,6) [] [],mkPlotO  (1,6) [] []])]
                 assertEqual (Right $ mkObjO (1,1) [("result",mkArrO (1,6) [mkPlotO  (1,6) [] [],mkTableO (1,6) [] []])]) $ runWith "show(f())" [("f",mkArrO (1,6) [mkPlotO  (1,6) [] [],mkTableO (1,6) [] []])]

test_Multi = do  assertEqual (Right $ mkNumO' (1,1) 1) $ run     "multi([1])"
                 assertEqual (Right $ mkNumO' (1,1) 2) $ run     "multi([1,2])"
                 assertEqual (Right $ mkNumO' (1,1) 6) $ run     "multi([1,2,3])"
                 assertEqual (Right $ mkNumO' (1,1) 0) $ run     "multi([1,2,3,0])"

                 assertEqual (Right $ mkNumO' (1,1) 1) $ runWith "multi([f()])"       [("f",numO 1)]
                 assertEqual (Right $ mkNumO' (1,1) 2) $ runWith "multi([f(),2])"     [("f",numO 1)]
                 assertEqual (Right $ mkNumO' (1,1) 6) $ runWith "multi([f(),2,3])"   [("f",numO 1)]
                 assertEqual (Right $ mkNumO' (1,1) 0) $ runWith "multi([f(),2,3,0])" [("f",numO 1)]

                 assertEqual (Right $ mkNumO' (1,1) 1) $ runWith "multi(f())" [("f",mkArrO (1,8) [numO 1])]
                 assertEqual (Right $ mkNumO' (1,1) 2) $ runWith "multi(f())" [("f",mkArrO (1,8) [numO 1,numO 2])]
                 assertEqual (Right $ mkNumO' (1,1) 6) $ runWith "multi(f())" [("f",mkArrO (1,8) [numO 1,numO 2,numO 3])]
                 assertEqual (Right $ mkNumO' (1,1) 0) $ runWith "multi(f())" [("f",mkArrO (1,8) [numO 1,numO 2,numO 3,numO 0])]

test_Mean = do   assertEqual (Right $ mkNumO' (1,1) 1) $ run     "mean([1])"
                 assertEqual (Right $ mkNumO' (1,1) 2) $ run     "mean([1,3])"
                 assertEqual (Right $ mkNumO' (1,1) 2) $ run     "mean([1,2,3])"
                 assertEqual (Right $ mkNumO' (1,1) 0) $ run     "mean([1,2,3,-6])"

                 assertEqual (Right $ mkNumO' (1,1) 1) $ runWith "mean([f()])"       [("f",numO 1)]
                 assertEqual (Right $ mkNumO' (1,1) 2) $ runWith "mean([f(),3])"     [("f",numO 1)]
                 assertEqual (Right $ mkNumO' (1,1) 2) $ runWith "mean([f(),2,3])"   [("f",numO 1)]
                 assertEqual (Right $ mkNumO' (1,1) 0) $ runWith "mean([f(),2,3,-6])" [("f",numO 1)]

                 assertEqual (Right $ mkNumO' (1,1) 1) $ runWith "mean(f())" [("f",mkArrO (1,8) [numO 1])]
                 assertEqual (Right $ mkNumO' (1,1) 2) $ runWith "mean(f())" [("f",mkArrO (1,8) [numO 1,numO 3])]
                 assertEqual (Right $ mkNumO' (1,1) 2) $ runWith "mean(f())" [("f",mkArrO (1,8) [numO 1,numO 2,numO 3])]
                 assertEqual (Right $ mkNumO' (1,1) 0) $ runWith "mean(f())" [("f",mkArrO (1,8) [numO 1,numO 2,numO 3,numO (-6)])]

test_Desc = let  sk = (-1.0182337649086284); ku = (-0.7696000000000001) in do
                 assertEqual (show $ eval $ mkDesc (1,1) 1 1 1  0    (0/0) (0/0)) $ show $ run     "descriptive([1])"
                 assertEqual (       eval $ mkDesc (1,1) 2 4 2  1     0 (-2))            $ run     "descriptive([1,3])"
                 assertEqual (       eval $ mkDesc (1,1) 3 6 2 (2/3)  0 (-1.5))          $ run     "descriptive([1,2,3])"
                 assertEqual (       eval $ mkDesc (1,1) 4 0 0 12.5  sk  ku)             $ run     "descriptive([1,2,3,-6])"

                 assertEqual (show $ eval $ mkDesc (1,1) 1 1 1  0    (0/0) (0/0)) $ show $ runWith "descriptive([f()])"        [("f",numO 1)]
                 assertEqual (       eval $ mkDesc (1,1) 2 4 2  1     0 (-2))            $ runWith "descriptive([f(),3])"      [("f",numO 1)]
                 assertEqual (       eval $ mkDesc (1,1) 3 6 2 (2/3)  0 (-1.5))          $ runWith "descriptive([f(),2,3])"    [("f",numO 1)]
                 assertEqual (       eval $ mkDesc (1,1) 4 0 0 12.5  sk  ku)             $ runWith "descriptive([f(),2,3,-6])" [("f",numO 1)]

                 assertEqual (show $ eval $ mkDesc (1,1) 1 1 1  0    (0/0) (0/0)) $ show $ runWith "descriptive(f())" [("f",mkArrO (1,8) [numO 1])]
                 assertEqual (       eval $ mkDesc (1,1) 2 4 2  1     0 (-2))            $ runWith "descriptive(f())" [("f",mkArrO (1,8) [numO 1,numO 3])]
                 assertEqual (       eval $ mkDesc (1,1) 3 6 2 (2/3)  0 (-1.5))          $ runWith "descriptive(f())" [("f",mkArrO (1,8) [numO 1,numO 2,numO 3])]
                 assertEqual (       eval $ mkDesc (1,1) 4 0 0 12.5  sk  ku)             $ runWith "descriptive(f())" [("f",mkArrO (1,8) [numO 1,numO 2,numO 3,numO (-6)])]

test_Table = do  assertEqual (Right $ mkTableO (1,1) [[mkNumO (1,9) 0]] [])                                                $ run     "table([[0]],{})"
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (1,9) 0]] [])                                                $ run     "table([[0]],{not:[]})"
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (1,9) 0]] [mkStrO (1,19) "1"])                                 $ run     "table([[0]],{col:[\"1\"]})"
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (1,9) 0]] [mkStrO (1,26) "1"])                                 $ run     "table([[0]],{not:[],col:[\"1\"]})"

                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (1,9) 0],[mkNumO (1,13) 1]] [])                                $ run     "table([[0],[1]],{})"
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (1,9) 0],[mkNumO (1,13) 1]] [])                                $ run     "table([[0],[1]],{not:[]})"
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (1,9) 0],[mkNumO (1,13) 1]] [mkStrO (1,23) "1",mkStrO (1,27) "2"]) $ run     "table([[0],[1]],{col:[\"1\",\"2\"]})"
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (1,9) 0],[mkNumO (1,13) 1]] [mkStrO (1,30) "1",mkStrO (1,34) "2"]) $ run     "table([[0],[1]],{not:[],col:[\"1\",\"2\"]})"

                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (1,9) 0]] [])                                                $ runWith "table(f(),{})"                         [("f",mkArrO (0,0) [mkArrO (0,0) [mkNumO (1,9) 0]])]
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (1,9) 0]] [])                                                $ runWith "table(f(),{not:[]})"                   [("f",mkArrO (0,0) [mkArrO (0,0) [mkNumO (1,9) 0]])]
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (1,9) 0]] [mkStrO (1,17) "1"])                                 $ runWith "table(f(),{col:[\"1\"]})"              [("f",mkArrO (0,0) [mkArrO (0,0) [mkNumO (1,9) 0]])]
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (1,9) 0]] [mkStrO (1,24) "1"])                                 $ runWith "table(f(),{not:[],col:[\"1\"]})"       [("f",mkArrO (0,0) [mkArrO (0,0) [mkNumO (1,9) 0]])]

                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (1,9) 0],[mkNumO (1,13) 1]] [])                                $ runWith "table(f(),{})"                         [("f",mkArrO (0,0) [mkArrO (0,0) [mkNumO (1,9) 0],mkArrO (0,0) [mkNumO (1,13) 1]])]
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (1,9) 0],[mkNumO (1,13) 1]] [])                                $ runWith "table(f(),{not:[]})"                   [("f",mkArrO (0,0) [mkArrO (0,0) [mkNumO (1,9) 0],mkArrO (0,0) [mkNumO (1,13) 1]])]
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (1,9) 0],[mkNumO (1,13) 1]] [mkStrO (1,17) "1",mkStrO (1,21) "2"]) $ runWith "table(f(),{col:[\"1\",\"2\"]})"        [("f",mkArrO (0,0) [mkArrO (0,0) [mkNumO (1,9) 0],mkArrO (0,0) [mkNumO (1,13) 1]])]
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (1,9) 0],[mkNumO (1,13) 1]] [mkStrO (1,24) "1",mkStrO (1,28) "2"]) $ runWith "table(f(),{not:[],col:[\"1\",\"2\"]})" [("f",mkArrO (0,0) [mkArrO (0,0) [mkNumO (1,9) 0],mkArrO (0,0) [mkNumO (1,13) 1]])]

                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (1,9) 0]] [])                                                $ runWith "table([[0]],f())"     [("f",mkObjO (0,0) [])]
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (1,9) 0]] [])                                                $ runWith "table([[0]],f())"     [("f",mkObjO (0,0) [("not",mkArrO (0,0) [])])]
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (1,9) 0]] [mkStrO (1,19) "1"])                                 $ runWith "table([[0]],f())"     [("f",mkObjO (0,0) [("col",mkArrO (0,0) [mkStrO (1,19) "1"])])]
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (1,9) 0]] [mkStrO (1,26) "1"])                                 $ runWith "table([[0]],f())"     [("f",mkObjO (0,0) [("not",mkArrO (0,0) []),("col",mkArrO (0,0) [mkStrO (1,26) "1"])])]

                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (1,9) 0],[mkNumO (1,13) 1]] [])                                $ runWith "table([[0],[1]],f())" [("f",mkObjO (0,0) [])]
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (1,9) 0],[mkNumO (1,13) 1]] [])                                $ runWith "table([[0],[1]],f())" [("f",mkObjO (0,0) [("not",mkArrO (0,0) [])])]
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (1,9) 0],[mkNumO (1,13) 1]] [mkStrO (1,23) "1",mkStrO (1,27) "2"]) $ runWith "table([[0],[1]],f())" [("f",mkObjO (0,0) [("col",mkArrO (0,0) [mkStrO (1,23) "1",mkStrO (1,27) "2"])])]
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (1,9) 0],[mkNumO (1,13) 1]] [mkStrO (1,34) "1",mkStrO (1,38) "2"]) $ runWith "table([[0],[1]],f())" [("f",mkObjO (0,0) [("not",mkArrO (0,0) []),("col",mkArrO (0,0) [mkStrO (1,34) "1",mkStrO (1,38) "2"])])]

                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (1,9) 0]] [])                                                $ runWith "table(f(),g())" [("f",mkArrO (0,0) [mkArrO (0,0) [mkNumO (1,9) 0]]),                             ("g",mkObjO (0,0) [])]
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (1,9) 0]] [])                                                $ runWith "table(g(),f())" [("g",mkArrO (0,0) [mkArrO (0,0) [mkNumO (1,9) 0]]),                             ("f",mkObjO (0,0) [("not",mkArrO (0,0) [])])]
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (1,9) 0]] [mkStrO (1,17) "1"])                                 $ runWith "table(f(),g())" [("f",mkArrO (0,0) [mkArrO (0,0) [mkNumO (1,9) 0]]),                             ("g",mkObjO (0,0) [("col",mkArrO (0,0) [mkStrO (1,17) "1"])])]
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (1,9) 0]] [mkStrO (1,28) "1"])                                 $ runWith "table(g(),f())" [("g",mkArrO (0,0) [mkArrO (0,0) [mkNumO (1,9) 0]]),                             ("f",mkObjO (0,0) [("not",mkArrO (0,0) []),("col",mkArrO (0,0) [mkStrO (1,28) "1"])])]

                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (1,9) 0],[mkNumO (1,13) 1]] [])                                $ runWith "table(f(),g())" [("f",mkArrO (0,0) [mkArrO (0,0) [mkNumO (1,9) 0],mkArrO (0,0) [mkNumO (1,13) 1]]),  ("g",mkObjO (0,0) [])]
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (1,9) 0],[mkNumO (1,13) 1]] [])                                $ runWith "table(g(),f())" [("g",mkArrO (0,0) [mkArrO (0,0) [mkNumO (1,9) 0],mkArrO (0,0) [mkNumO (1,13) 1]]),  ("f",mkObjO (0,0) [("not",mkArrO (0,0) [])])]
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (1,9) 0],[mkNumO (1,13) 1]] [mkStrO (1,17) "1",mkStrO (1,21) "2"]) $ runWith "table(f(),g())" [("f",mkArrO (0,0) [mkArrO (0,0) [mkNumO (1,9) 0],mkArrO (0,0) [mkNumO (1,13) 1]]),  ("g",mkObjO (0,0) [("col",mkArrO (0,0) [mkStrO (1,17) "1",mkStrO (1,21) "2"])])]
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (1,9) 0],[mkNumO (1,13) 1]] [mkStrO (1,28) "1",mkStrO (1,32) "2"]) $ runWith "table(g(),f())" [("g",mkArrO (0,0) [mkArrO (0,0) [mkNumO (1,9) 0],mkArrO (0,0) [mkNumO (1,13) 1]]),  ("f",mkObjO (0,0) [("not",mkArrO (0,0) []),("col",mkArrO (0,0) [mkStrO (1,28) "1",mkStrO (1,32) "2"])])]

test_NTimes = do assertEqual (Right $ mkArrO (1,1) [])                           $ run     "nTimes(1,0)"
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,8) 0])               $ run     "nTimes(0,1)"
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,8) 1, mkNumO (1,8) 1]) $ run     "nTimes(1,2)"

                 assertEqual (Right $ mkArrO (1,1) [])                           $ runWith "nTimes(f(),0)" [("f",mkNumO (1,8) 1)]
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,8) 0])               $ runWith "nTimes(f(),1)" [("f",mkNumO (1,8) 0)]
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,8) 1, mkNumO (1,8) 1]) $ runWith "nTimes(f(),2)" [("f",mkNumO (1,8) 1)]

                 assertEqual (Right $ mkArrO (1,1) [])                           $ runWith "nTimes(1,f())" [("f",mkNumO (0,0) 0)]
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,8) 0])               $ runWith "nTimes(0,f())" [("f",mkNumO (0,0) 1)]
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,8) 1, mkNumO (1,8) 1]) $ runWith "nTimes(1,f())" [("f",mkNumO (0,0) 2)]

                 assertEqual (Right $ mkArrO (1,1) [])                           $ runWith "nTimes(f(),g())" [("f",mkNumO (1,8) 1),("g",mkNumO (0,0) 0)]
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,8) 0])               $ runWith "nTimes(g(),f())" [("g",mkNumO (1,8) 0),("f",mkNumO (0,0) 1)]
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,8) 1, mkNumO (1,8) 1]) $ runWith "nTimes(f(),g())" [("f",mkNumO (1,8) 1),("g",mkNumO (0,0) 2)]

test_Take = do   assertEqual (Right $ mkTableO (1,1) [] [])                                                        $ runWith "take(1,f())" [("f",mkTableO (0,0) [] [])]
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (0,0) 0]] [])                                          $ runWith "take(1,f())" [("f",mkTableO (0,0) [[mkNumO (0,0) 0]] [])]
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (0,0) 0]] [])                                          $ runWith "take(2,f())" [("f",mkTableO (0,0) [[mkNumO (0,0) 0]] [])]
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (0,0) 0]] [])                                          $ runWith "take(3,f())" [("f",mkTableO (0,0) [[mkNumO (0,0) 0]] [])]
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (0,0) 0],[mkNumO (1,0) 0]] [])                           $ runWith "take(1,f())" [("f",mkTableO (0,0) [[mkNumO (0,0) 0,mkNumO (0,1) 1],[mkNumO (1,0) 0,mkNumO (1,1) 1]] [])]
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (0,0) 0,mkNumO (0,1) 1],[mkNumO (1,0) 0,mkNumO (1,1) 1]] []) $ runWith "take(2,f())" [("f",mkTableO (0,0) [[mkNumO (0,0) 0,mkNumO (0,1) 1],[mkNumO (1,0) 0,mkNumO (1,1) 1]] [])]
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (0,0) 0,mkNumO (0,1) 1],[mkNumO (1,0) 0,mkNumO (1,1) 1]] []) $ runWith "take(3,f())" [("f",mkTableO (0,0) [[mkNumO (0,0) 0,mkNumO (0,1) 1],[mkNumO (1,0) 0,mkNumO (1,1) 1]] [])]

                 assertEqual (Right $ mkTableO (1,1) [] [])                                                        $ runWith "take(f(),g())" [("f",mkNumO (0,0) 1),("g",mkTableO (0,0) [] [])]
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (0,0) 0]] [])                                          $ runWith "take(f(),g())" [("f",mkNumO (0,0) 1),("g",mkTableO (0,0) [[mkNumO (0,0) 0]] [])]
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (0,0) 0]] [])                                          $ runWith "take(f(),g())" [("f",mkNumO (0,0) 2),("g",mkTableO (0,0) [[mkNumO (0,0) 0]] [])]
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (0,0) 0]] [])                                          $ runWith "take(f(),g())" [("f",mkNumO (0,0) 3),("g",mkTableO (0,0) [[mkNumO (0,0) 0]] [])]
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (0,0) 0],[mkNumO (1,0) 0]] [])                           $ runWith "take(f(),g())" [("f",mkNumO (0,0) 1),("g",mkTableO (0,0) [[mkNumO (0,0) 0,mkNumO (0,1) 1],[mkNumO (1,0) 0,mkNumO (1,1) 1]] [])]
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (0,0) 0,mkNumO (0,1) 1],[mkNumO (1,0) 0,mkNumO (1,1) 1]] []) $ runWith "take(f(),g())" [("f",mkNumO (0,0) 2),("g",mkTableO (0,0) [[mkNumO (0,0) 0,mkNumO (0,1) 1],[mkNumO (1,0) 0,mkNumO (1,1) 1]] [])]
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (0,0) 0,mkNumO (0,1) 1],[mkNumO (1,0) 0,mkNumO (1,1) 1]] []) $ runWith "take(f(),g())" [("f",mkNumO (0,0) 3),("g",mkTableO (0,0) [[mkNumO (0,0) 0,mkNumO (0,1) 1],[mkNumO (1,0) 0,mkNumO (1,1) 1]] [])]

                 assertEqual (Right $ mkArrO (1,1) [])                            $ run     "take(0,[])"
                 assertEqual (Right $ mkArrO (1,1) [])                            $ run     "take(1,[])"
                 assertEqual (Right $ mkArrO (1,1) [])                            $ run     "take(0,[0])"
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,9) 0])                $ run     "take(1,[0])"
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,9) 0])                $ run     "take(2,[0])"
                 assertEqual (Right $ mkArrO (1,1) [])                            $ run     "take(0,[0,1])"
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,9) 0])                $ run     "take(1,[0,1])"
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,9) 0,mkNumO (1,11) 1])  $ run     "take(2,[0,1])"
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,9) 0,mkNumO (1,11) 1])  $ run     "take(3,[0,1])"

                 assertEqual (Right $ mkArrO (1,1) [])                            $ runWith "take(f(),[])"    [("f",mkNumO (0,0) 0)]
                 assertEqual (Right $ mkArrO (1,1) [])                            $ runWith "take(f(),[])"    [("f",mkNumO (0,0) 1)]
                 assertEqual (Right $ mkArrO (1,1) [])                            $ runWith "take(f(),[0])"   [("f",mkNumO (0,0) 0)]
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,11) 0])               $ runWith "take(f(),[0])"   [("f",mkNumO (0,0) 1)]
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,11) 0])               $ runWith "take(f(),[0])"   [("f",mkNumO (0,0) 2)]
                 assertEqual (Right $ mkArrO (1,1) [])                            $ runWith "take(f(),[0,1])" [("f",mkNumO (0,0) 0)]
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,11) 0])               $ runWith "take(f(),[0,1])" [("f",mkNumO (0,0) 1)]
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,11) 0,mkNumO (1,13) 1]) $ runWith "take(f(),[0,1])" [("f",mkNumO (0,0) 2)]
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,11) 0,mkNumO (1,13) 1]) $ runWith "take(f(),[0,1])" [("f",mkNumO (0,0) 3)]

                 assertEqual (Right $ mkArrO (1,1) [])                            $ runWith "take(0,f())"     [("f",mkArrO (0,0) [])]
                 assertEqual (Right $ mkArrO (1,1) [])                            $ runWith "take(1,f())"     [("f",mkArrO (0,0) [])]
                 assertEqual (Right $ mkArrO (1,1) [])                            $ runWith "take(0,f())"     [("f",mkArrO (0,0) [mkNumO (1,9) 0])]
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,9) 0])                $ runWith "take(1,f())"     [("f",mkArrO (0,0) [mkNumO (1,9) 0])]
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,9) 0])                $ runWith "take(2,f())"     [("f",mkArrO (0,0) [mkNumO (1,9) 0])]
                 assertEqual (Right $ mkArrO (1,1) [])                            $ runWith "take(0,f())"     [("f",mkArrO (0,0) [mkNumO (1,9) 0,mkNumO (1,11) 1])]
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,9) 0])                $ runWith "take(1,f())"     [("f",mkArrO (0,0) [mkNumO (1,9) 0,mkNumO (1,11) 1])]
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,9) 0,mkNumO (1,11) 1])  $ runWith "take(2,f())"     [("f",mkArrO (0,0) [mkNumO (1,9) 0,mkNumO (1,11) 1])]
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,9) 0,mkNumO (1,11) 1])  $ runWith "take(3,f())"     [("f",mkArrO (0,0) [mkNumO (1,9) 0,mkNumO (1,11) 1])]

                 assertEqual (Right $ mkArrO (1,1) [])                            $ runWith "take(f(),g())"   [("f",mkNumO (0,0) 0),("g",mkArrO (0,0) [])]
                 assertEqual (Right $ mkArrO (1,1) [])                            $ runWith "take(f(),g())"   [("f",mkNumO (0,0) 1),("g",mkArrO (0,0) [])]
                 assertEqual (Right $ mkArrO (1,1) [])                            $ runWith "take(f(),g())"   [("f",mkNumO (0,0) 0),("g",mkArrO (0,0) [mkNumO (1,11) 0])]
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,11) 0])               $ runWith "take(f(),g())"   [("f",mkNumO (0,0) 1),("g",mkArrO (0,0) [mkNumO (1,11) 0])]
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,11) 0])               $ runWith "take(f(),g())"   [("f",mkNumO (0,0) 2),("g",mkArrO (0,0) [mkNumO (1,11) 0])]
                 assertEqual (Right $ mkArrO (1,1) [])                            $ runWith "take(f(),g())"   [("f",mkNumO (0,0) 0),("g",mkArrO (0,0) [mkNumO (1,11) 0,mkNumO (1,13) 1])]
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,11) 0])               $ runWith "take(f(),g())"   [("f",mkNumO (0,0) 1),("g",mkArrO (0,0) [mkNumO (1,11) 0,mkNumO (1,13) 1])]
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,11) 0,mkNumO (1,13) 1]) $ runWith "take(f(),g())"   [("f",mkNumO (0,0) 2),("g",mkArrO (0,0) [mkNumO (1,11) 0,mkNumO (1,13) 1])]
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,11) 0,mkNumO (1,13) 1]) $ runWith "take(f(),g())"   [("f",mkNumO (0,0) 3),("g",mkArrO (0,0) [mkNumO (1,11) 0,mkNumO (1,13) 1])]

test_Sort = do   assertEqual (Right $ mkTableO (1,1) [[mkNumO (0,0) 0],[mkNumO (0,1) 1]] [])                                                     $ runWith "sort(0,f())" [("f",mkTableO (0,0) [[mkNumO (0,0) 0],[mkNumO (0,1) 1]] [])]
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (0,0) 0],[mkNumO (0,1) 1]] [])                                                     $ runWith "sort(1,f())" [("f",mkTableO (0,0) [[mkNumO (0,0) 0],[mkNumO (0,1) 1]] [])]
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (0,1) 0,mkNumO (0,0) 1,mkNumO (0,2) 2],[mkNumO (1,1) 2,mkNumO (1,0) 3,mkNumO (1,2) 1]] []) $ runWith "sort(0,f())" [("f",mkTableO (0,0) [[mkNumO (0,0) 1,mkNumO (0,1) 0,mkNumO (0,2) 2],[mkNumO (1,0) 3,mkNumO (1,1) 2,mkNumO (1,2) 1]] [])]
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (0,2) 2,mkNumO (0,1) 0,mkNumO (0,0) 1],[mkNumO (1,2) 1,mkNumO (1,1) 2,mkNumO (1,0) 3]] []) $ runWith "sort(1,f())" [("f",mkTableO (0,0) [[mkNumO (0,0) 1,mkNumO (0,1) 0,mkNumO (0,2) 2],[mkNumO (1,0) 3,mkNumO (1,1) 2,mkNumO (1,2) 1]] [])]

                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (0,0) 0],[mkNumO (0,1) 1]] [])                                                     $ runWith "sort(f(),g())" [("f",mkNumO (1,6) 0),("g",mkTableO (0,0) [[mkNumO (0,0) 0],[mkNumO (0,1) 1]] [])]
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (0,0) 0],[mkNumO (0,1) 1]] [])                                                     $ runWith "sort(f(),g())" [("f",mkNumO (1,6) 1),("g",mkTableO (0,0) [[mkNumO (0,0) 0],[mkNumO (0,1) 1]] [])]
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (0,1) 0,mkNumO (0,0) 1,mkNumO (0,2) 2],[mkNumO (1,1) 2,mkNumO (1,0) 3,mkNumO (1,2) 1]] []) $ runWith "sort(f(),g())" [("f",mkNumO (1,6) 0),("g",mkTableO (0,0) [[mkNumO (0,0) 1,mkNumO (0,1) 0,mkNumO (0,2) 2],[mkNumO (1,0) 3,mkNumO (1,1) 2,mkNumO (1,2) 1]] [])]
                 assertEqual (Right $ mkTableO (1,1) [[mkNumO (0,2) 2,mkNumO (0,1) 0,mkNumO (0,0) 1],[mkNumO (1,2) 1,mkNumO (1,1) 2,mkNumO (1,0) 3]] []) $ runWith "sort(f(),g())" [("f",mkNumO (1,6) 1),("g",mkTableO (0,0) [[mkNumO (0,0) 1,mkNumO (0,1) 0,mkNumO (0,2) 2],[mkNumO (1,0) 3,mkNumO (1,1) 2,mkNumO (1,2) 1]] [])]

                 assertEqual (Right $ mkArrO (1,1) [mkArrO (1,9) [mkNumO (1,10) 0],                            mkArrO (1,13) [mkNumO (1,14) 1]])                             $ run     "sort(0,[[0],[1]])"
                 assertEqual (Right $ mkArrO (1,1) [mkArrO (1,9) [mkNumO (1,10) 0],                            mkArrO (1,13) [mkNumO (1,14) 1]])                             $ run     "sort(1,[[0],[1]])"
                 assertEqual (Right $ mkArrO (1,1) [mkArrO (1,9) [mkNumO (1,12) 0,mkNumO (1,10) 1,mkNumO (1,14) 2],mkArrO (1,17) [mkNumO (1,20) 2,mkNumO (1,18) 3,mkNumO (1,22) 1]]) $ run     "sort(0,[[1,0,2],[3,2,1]])"
                 assertEqual (Right $ mkArrO (1,1) [mkArrO (1,9) [mkNumO (1,14) 2,mkNumO (1,12) 0,mkNumO (1,10) 1],mkArrO (1,17) [mkNumO (1,22) 1,mkNumO (1,20) 2,mkNumO (1,18) 3]]) $ run     "sort(1,[[1,0,2],[3,2,1]])"

                 assertEqual (Right $ mkArrO (1,1) [mkArrO (1,9) [mkNumO (1,10) 0],                            mkArrO (1,13) [mkNumO (1,14) 1]])                             $ runWith "sort(0,f())"                 [("f",mkArrO (0,0) [mkArrO (1,9) [mkNumO (1,10) 0],                            mkArrO (1,13) [mkNumO (1,14) 1]])]
                 assertEqual (Right $ mkArrO (1,1) [mkArrO (1,9) [mkNumO (1,10) 0],                            mkArrO (1,13) [mkNumO (1,14) 1]])                             $ runWith "sort(1,f())"                 [("f",mkArrO (0,0) [mkArrO (1,9) [mkNumO (1,10) 0],                            mkArrO (1,13) [mkNumO (1,14) 1]])]
                 assertEqual (Right $ mkArrO (1,1) [mkArrO (1,9) [mkNumO (1,12) 0,mkNumO (1,10) 1,mkNumO (1,14) 2],mkArrO (1,17) [mkNumO (1,20) 2,mkNumO (1,18) 3,mkNumO (1,22) 1]]) $ runWith "sort(0,f())"                 [("f",mkArrO (0,0) [mkArrO (1,9) [mkNumO (1,10) 1,mkNumO (1,12) 0,mkNumO (1,14) 2],mkArrO (1,17) [mkNumO (1,18) 3,mkNumO (1,20) 2,mkNumO (1,22) 1]])]
                 assertEqual (Right $ mkArrO (1,1) [mkArrO (1,9) [mkNumO (1,14) 2,mkNumO (1,12) 0,mkNumO (1,10) 1],mkArrO (1,17) [mkNumO (1,22) 1,mkNumO (1,20) 2,mkNumO (1,18) 3]]) $ runWith "sort(1,f())"                 [("f",mkArrO (0,0) [mkArrO (1,9) [mkNumO (1,10) 1,mkNumO (1,12) 0,mkNumO (1,14) 2],mkArrO (1,17) [mkNumO (1,18) 3,mkNumO (1,20) 2,mkNumO (1,22) 1]])]

                 assertEqual (Right $ mkArrO (1,1) [mkArrO (1,11) [mkNumO (1,12) 0],                             mkArrO (1,15) [mkNumO (1,16) 1]])                            $ runWith "sort(f(),[[0],[1]])"         [("f",mkNumO (0,0) 0)]
                 assertEqual (Right $ mkArrO (1,1) [mkArrO (1,11) [mkNumO (1,12) 0],                             mkArrO (1,15) [mkNumO (1,16) 1]])                            $ runWith "sort(f(),[[0],[1]])"         [("f",mkNumO (0,0) 1)]
                 assertEqual (Right $ mkArrO (1,1) [mkArrO (1,11) [mkNumO (1,14) 0,mkNumO (1,12) 1,mkNumO (1,16) 2],mkArrO (1,19) [mkNumO (1,22) 2,mkNumO (1,20) 3,mkNumO (1,24) 1]]) $ runWith "sort(f(),[[1,0,2],[3,2,1]])" [("f",mkNumO (0,0) 0)]
                 assertEqual (Right $ mkArrO (1,1) [mkArrO (1,11) [mkNumO (1,16) 2,mkNumO (1,14) 0,mkNumO (1,12) 1],mkArrO (1,19) [mkNumO (1,24) 1,mkNumO (1,22) 2,mkNumO (1,20) 3]]) $ runWith "sort(f(),[[1,0,2],[3,2,1]])" [("f",mkNumO (0,0) 1)]

                 assertEqual (Right $ mkArrO (1,1) [mkArrO (1,9) [mkNumO (1,10) 0],                            mkArrO (1,13) [mkNumO (1,14) 1]])                             $ runWith "sort(f(),g())"               [("f",mkNumO (0,0) 0),("g",mkArrO (1,1) [mkArrO (1,9) [mkNumO (1,10) 0],                            mkArrO (1,13) [mkNumO (1,14) 1]])]
                 assertEqual (Right $ mkArrO (1,1) [mkArrO (1,9) [mkNumO (1,10) 0],                            mkArrO (1,13) [mkNumO (1,14) 1]])                             $ runWith "sort(f(),g())"               [("f",mkNumO (0,0) 1),("g",mkArrO (1,1) [mkArrO (1,9) [mkNumO (1,10) 0],                            mkArrO (1,13) [mkNumO (1,14) 1]])]
                 assertEqual (Right $ mkArrO (1,1) [mkArrO (1,9) [mkNumO (1,12) 0,mkNumO (1,10) 1,mkNumO (1,14) 2],mkArrO (1,17) [mkNumO (1,20) 2,mkNumO (1,18) 3,mkNumO (1,22) 1]]) $ runWith "sort(f(),g())"               [("f",mkNumO (0,0) 0),("g",mkArrO (1,1) [mkArrO (1,9) [mkNumO (1,10) 1,mkNumO (1,12) 0,mkNumO (1,14) 2],mkArrO (1,17) [mkNumO (1,18) 3,mkNumO (1,20) 2,mkNumO (1,22) 1]])]
                 assertEqual (Right $ mkArrO (1,1) [mkArrO (1,9) [mkNumO (1,14) 2,mkNumO (1,12) 0,mkNumO (1,10) 1],mkArrO (1,17) [mkNumO (1,22) 1,mkNumO (1,20) 2,mkNumO (1,18) 3]]) $ runWith "sort(f(),g())"               [("f",mkNumO (0,0) 1),("g",mkArrO (1,1) [mkArrO (1,9) [mkNumO (1,10) 1,mkNumO (1,12) 0,mkNumO (1,14) 2],mkArrO (1,17) [mkNumO (1,18) 3,mkNumO (1,20) 2,mkNumO (1,22) 1]])]

test_Col = do    assertEqual (Right $ mkArrO (1,1) [mkNumO (0,0) 0])                              $ runWith "col(0,f())" [("f",mkTableO (0,0) [[mkNumO (0,0) 0],[mkNumO (0,1) 1]] [])]
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (0,1) 1])                              $ runWith "col(1,f())" [("f",mkTableO (0,0) [[mkNumO (0,0) 0],[mkNumO (0,1) 1]] [])]
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (0,0) 1,mkNumO (0,1) 0,mkNumO (0,2) 2])    $ runWith "col(0,f())" [("f",mkTableO (0,0) [[mkNumO (0,0) 1,mkNumO (0,1) 0,mkNumO (0,2) 2],[mkNumO (1,0) 3,mkNumO (1,1) 2,mkNumO (1,2) 1]] [])]
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,0) 3,mkNumO (1,1) 2,mkNumO (1,2) 1])    $ runWith "col(1,f())" [("f",mkTableO (0,0) [[mkNumO (0,0) 1,mkNumO (0,1) 0,mkNumO (0,2) 2],[mkNumO (1,0) 3,mkNumO (1,1) 2,mkNumO (1,2) 1]] [])]

                 assertEqual (Right $ mkArrO (1,1) [mkNumO (0,0) 0])                              $ runWith "col(f(),g())" [("f",mkNumO (1,6) 0),("g",mkTableO (0,0) [[mkNumO (0,0) 0],[mkNumO (0,1) 1]] [])]
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (0,1) 1])                              $ runWith "col(f(),g())" [("f",mkNumO (1,6) 1),("g",mkTableO (0,0) [[mkNumO (0,0) 0],[mkNumO (0,1) 1]] [])]
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (0,0) 1,mkNumO (0,1) 0,mkNumO (0,2) 2])    $ runWith "col(f(),g())" [("f",mkNumO (1,6) 0),("g",mkTableO (0,0) [[mkNumO (0,0) 1,mkNumO (0,1) 0,mkNumO (0,2) 2],[mkNumO (1,0) 3,mkNumO (1,1) 2,mkNumO (1,2) 1]] [])]
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,0) 3,mkNumO (1,1) 2,mkNumO (1,2) 1])    $ runWith "col(f(),g())" [("f",mkNumO (1,6) 1),("g",mkTableO (0,0) [[mkNumO (0,0) 1,mkNumO (0,1) 0,mkNumO (0,2) 2],[mkNumO (1,0) 3,mkNumO (1,1) 2,mkNumO (1,2) 1]] [])]

                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,9)  0])                             $ run     "col(0,[[0],[1]])"
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,13) 1])                             $ run     "col(1,[[0],[1]])"
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,9)  1,mkNumO (1,11) 0,mkNumO (1,13) 2]) $ run     "col(0,[[1,0,2],[3,2,1]])"
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,17) 3,mkNumO (1,19) 2,mkNumO (1,21) 1]) $ run     "col(1,[[1,0,2],[3,2,1]])"

                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,10) 0])                             $ runWith "col(0,f())"                 [("f",mkArrO (0,0) [mkArrO (1,9) [mkNumO (1,10) 0],                            mkArrO (1,13) [mkNumO (1,14) 1]])]
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,14) 1])                             $ runWith "col(1,f())"                 [("f",mkArrO (0,0) [mkArrO (1,9) [mkNumO (1,10) 0],                            mkArrO (1,13) [mkNumO (1,14) 1]])]
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,10) 1,mkNumO (1,12) 0,mkNumO (1,14) 2]) $ runWith "col(0,f())"                 [("f",mkArrO (0,0) [mkArrO (1,9) [mkNumO (1,10) 1,mkNumO (1,12) 0,mkNumO (1,14) 2],mkArrO (1,17) [mkNumO (1,18) 3,mkNumO (1,20) 2,mkNumO (1,22) 1]])]
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,18) 3,mkNumO (1,20) 2,mkNumO (1,22) 1]) $ runWith "col(1,f())"                 [("f",mkArrO (0,0) [mkArrO (1,9) [mkNumO (1,10) 1,mkNumO (1,12) 0,mkNumO (1,14) 2],mkArrO (1,17) [mkNumO (1,18) 3,mkNumO (1,20) 2,mkNumO (1,22) 1]])]

                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,11) 0])                             $ runWith "col(f(),[[0],[1]])"         [("f",mkNumO (0,0) 0)]
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,15) 1])                             $ runWith "col(f(),[[0],[1]])"         [("f",mkNumO (0,0) 1)]
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,11) 1,mkNumO (1,13) 0,mkNumO (1,15) 2]) $ runWith "col(f(),[[1,0,2],[3,2,1]])" [("f",mkNumO (0,0) 0)]
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,19) 3,mkNumO (1,21) 2,mkNumO (1,23) 1]) $ runWith "col(f(),[[1,0,2],[3,2,1]])" [("f",mkNumO (0,0) 1)]

                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,10) 0])                             $ runWith "col(f(),g())"               [("f",mkNumO (0,0) 0),("g",mkArrO (1,1) [mkArrO (1,9) [mkNumO (1,10) 0],                            mkArrO (1,13) [mkNumO (1,14) 1]])]
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,14) 1])                             $ runWith "col(f(),g())"               [("f",mkNumO (0,0) 1),("g",mkArrO (1,1) [mkArrO (1,9) [mkNumO (1,10) 0],                            mkArrO (1,13) [mkNumO (1,14) 1]])]
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,10) 1,mkNumO (1,12) 0,mkNumO (1,14) 2]) $ runWith "col(f(),g())"               [("f",mkNumO (0,0) 0),("g",mkArrO (1,1) [mkArrO (1,9) [mkNumO (1,10) 1,mkNumO (1,12) 0,mkNumO (1,14) 2],mkArrO (1,17) [mkNumO (1,18) 3,mkNumO (1,20) 2,mkNumO (1,22) 1]])]
                 assertEqual (Right $ mkArrO (1,1) [mkNumO (1,18) 3,mkNumO (1,20) 2,mkNumO (1,22) 1]) $ runWith "col(f(),g())"               [("f",mkNumO (0,0) 1),("g",mkArrO (1,1) [mkArrO (1,9) [mkNumO (1,10) 1,mkNumO (1,12) 0,mkNumO (1,14) 2],mkArrO (1,17) [mkNumO (1,18) 3,mkNumO (1,20) 2,mkNumO (1,22) 1]])]
