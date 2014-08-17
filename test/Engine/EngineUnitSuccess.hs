{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Engine.EngineUnitSuccess where

import Test.Framework

import Engine.EngineUnitSuccessUtils
import Engine.EngineUnitUtils
import Marshall.MarshallUtils

{-# ANN module "HLint: ignore Use camelCase" #-}

test_Show = do   assertEqual' (Right $ mkObjC (1,1) [("result",mkArrC (1,6) [])])                                          $ run     "show([])"
                 assertEqual' (Right $ mkObjC (1,1) [("result",mkArrC (1,6) [mkTableC (1,7) [] []])])                      $ runWith "show([f()])" [("f",mkTableC (1,7) [] [])]
                 assertEqual' (Right $ mkObjC (1,1) [("result",mkArrC (1,6) [mkPlotC (1,11) [] []])])                      $ runWith "show([f()])" [("f",mkPlotC (1,11) [] [])]

                 assertEqual' (Right $ mkObjC (1,1) [("result",mkArrC (1,6) [mkTableC (1,7) [] [],mkPlotC (1,11) [] []])]) $ runWith "show([f(),g()])" [("f",mkTableC (1,7) [] []),("g",mkPlotC (1,11) [] [])]
                 assertEqual' (Right $ mkObjC (1,1) [("result",mkArrC (1,6) [mkPlotC (1,11) [] [],mkTableC (1,7) [] []])]) $ runWith "show([g(),f()])" [("f",mkTableC (1,7) [] []),("g",mkPlotC (1,11) [] [])]

                 assertEqual' (Right $ mkObjC (1,1) [("result",mkArrC (1,6) [])])                                          $ runWith "show(f())" [("f",mkArrC (1,6) [])]
                 assertEqual' (Right $ mkObjC (1,1) [("result",mkArrC (1,6) [mkTableC (1,6) [] []])])                      $ runWith "show(f())" [("f",mkArrC (1,6) [mkTableC (1,6) [] []])]
                 assertEqual' (Right $ mkObjC (1,1) [("result",mkArrC (1,6) [mkPlotC  (1,6) [] []])])                      $ runWith "show(f())" [("f",mkArrC (1,6) [mkPlotC ( 1,6) [] []])]

                 assertEqual' (Right $ mkObjC (1,1) [("result",mkArrC (1,6) [mkTableC (1,6) [] [],mkPlotC  (1,6) [] []])]) $ runWith "show(f())" [("f",mkArrC (1,6) [mkTableC (1,6) [] [],mkPlotC  (1,6) [] []])]
                 assertEqual' (Right $ mkObjC (1,1) [("result",mkArrC (1,6) [mkPlotC  (1,6) [] [],mkTableC (1,6) [] []])]) $ runWith "show(f())" [("f",mkArrC (1,6) [mkPlotC  (1,6) [] [],mkTableC (1,6) [] []])]

test_Multi = do  assertEqual' (Right $ mkNumC (1,1) 1) $ run     "multi([1])"
                 assertEqual' (Right $ mkNumC (1,1) 2) $ run     "multi([1,2])"
                 assertEqual' (Right $ mkNumC (1,1) 6) $ run     "multi([1,2,3])"
                 assertEqual' (Right $ mkNumC (1,1) 0) $ run     "multi([1,2,3,0])"

                 assertEqual' (Right $ mkNumC (1,1) 1) $ runWith "multi([f()])"       [("f",numO 1)]
                 assertEqual' (Right $ mkNumC (1,1) 2) $ runWith "multi([f(),2])"     [("f",numO 1)]
                 assertEqual' (Right $ mkNumC (1,1) 6) $ runWith "multi([f(),2,3])"   [("f",numO 1)]
                 assertEqual' (Right $ mkNumC (1,1) 0) $ runWith "multi([f(),2,3,0])" [("f",numO 1)]

                 assertEqual' (Right $ mkNumC (1,1) 1) $ runWith "multi(f())" [("f",mkArrC (1,8) [numO 1])]
                 assertEqual' (Right $ mkNumC (1,1) 2) $ runWith "multi(f())" [("f",mkArrC (1,8) [numO 1,numO 2])]
                 assertEqual' (Right $ mkNumC (1,1) 6) $ runWith "multi(f())" [("f",mkArrC (1,8) [numO 1,numO 2,numO 3])]
                 assertEqual' (Right $ mkNumC (1,1) 0) $ runWith "multi(f())" [("f",mkArrC (1,8) [numO 1,numO 2,numO 3,numO 0])]

test_Mean = do   assertEqual' (Right $ mkNumC (1,1) 1) $ run     "mean([1])"
                 assertEqual' (Right $ mkNumC (1,1) 2) $ run     "mean([1,3])"
                 assertEqual' (Right $ mkNumC (1,1) 2) $ run     "mean([1,2,3])"
                 assertEqual' (Right $ mkNumC (1,1) 0) $ run     "mean([1,2,3,-6])"

                 assertEqual' (Right $ mkNumC (1,1) 1) $ runWith "mean([f()])"       [("f",numO 1)]
                 assertEqual' (Right $ mkNumC (1,1) 2) $ runWith "mean([f(),3])"     [("f",numO 1)]
                 assertEqual' (Right $ mkNumC (1,1) 2) $ runWith "mean([f(),2,3])"   [("f",numO 1)]
                 assertEqual' (Right $ mkNumC (1,1) 0) $ runWith "mean([f(),2,3,-6])" [("f",numO 1)]

                 assertEqual' (Right $ mkNumC (1,1) 1) $ runWith "mean(f())" [("f",mkArrC (1,8) [numO 1])]
                 assertEqual' (Right $ mkNumC (1,1) 2) $ runWith "mean(f())" [("f",mkArrC (1,8) [numO 1,numO 3])]
                 assertEqual' (Right $ mkNumC (1,1) 2) $ runWith "mean(f())" [("f",mkArrC (1,8) [numO 1,numO 2,numO 3])]
                 assertEqual' (Right $ mkNumC (1,1) 0) $ runWith "mean(f())" [("f",mkArrC (1,8) [numO 1,numO 2,numO 3,numO (-6)])]

test_Desc = let  sk = (-1.0182337649086284); ku = (-0.7696000000000001) in do
                 assertEqual' (show $ eval $ mkDesc (1,1) 1 1 1  0    (0/0) (0/0)) $ show $ run     "descriptive([1])"
                 assertEqual' (       eval $ mkDesc (1,1) 2 4 2  1     0 (-2))            $ run     "descriptive([1,3])"
                 assertEqual' (       eval $ mkDesc (1,1) 3 6 2 (2/3)  0 (-1.5))          $ run     "descriptive([1,2,3])"
                 assertEqual' (       eval $ mkDesc (1,1) 4 0 0 12.5  sk  ku)             $ run     "descriptive([1,2,3,-6])"

                 assertEqual' (show $ eval $ mkDesc (1,1) 1 1 1  0    (0/0) (0/0)) $ show $ runWith "descriptive([f()])"        [("f",numO 1)]
                 assertEqual' (       eval $ mkDesc (1,1) 2 4 2  1     0 (-2))            $ runWith "descriptive([f(),3])"      [("f",numO 1)]
                 assertEqual' (       eval $ mkDesc (1,1) 3 6 2 (2/3)  0 (-1.5))          $ runWith "descriptive([f(),2,3])"    [("f",numO 1)]
                 assertEqual' (       eval $ mkDesc (1,1) 4 0 0 12.5  sk  ku)             $ runWith "descriptive([f(),2,3,-6])" [("f",numO 1)]

                 assertEqual' (show $ eval $ mkDesc (1,1) 1 1 1  0    (0/0) (0/0)) $ show $ runWith "descriptive(f())" [("f",mkArrC (1,8) [numO 1])]
                 assertEqual' (       eval $ mkDesc (1,1) 2 4 2  1     0 (-2))            $ runWith "descriptive(f())" [("f",mkArrC (1,8) [numO 1,numO 3])]
                 assertEqual' (       eval $ mkDesc (1,1) 3 6 2 (2/3)  0 (-1.5))          $ runWith "descriptive(f())" [("f",mkArrC (1,8) [numO 1,numO 2,numO 3])]
                 assertEqual' (       eval $ mkDesc (1,1) 4 0 0 12.5  sk  ku)             $ runWith "descriptive(f())" [("f",mkArrC (1,8) [numO 1,numO 2,numO 3,numO (-6)])]

test_Table = do  assertEqual' (Right $ mkTableC (1,1) [[mkNumU (1,9) 0]] [])                                                      $ run     "table([[0]],{})"
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (1,9) 0]] [])                                                      $ run     "table([[0]],{not:[]})"
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (1,9) 0]] [mkStrU (1,19) "1"])                                     $ run     "table([[0]],{col:[\"1\"]})"
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (1,9) 0]] [mkStrU (1,26) "1"])                                     $ run     "table([[0]],{not:[],col:[\"1\"]})"

                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (1,9) 0],[mkNumU (1,13) 1]] [])                                    $ run     "table([[0],[1]],{})"
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (1,9) 0],[mkNumU (1,13) 1]] [])                                    $ run     "table([[0],[1]],{not:[]})"
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (1,9) 0],[mkNumU (1,13) 1]] [mkStrU (1,23) "1",mkStrU (1,27) "2"]) $ run     "table([[0],[1]],{col:[\"1\",\"2\"]})"
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (1,9) 0],[mkNumU (1,13) 1]] [mkStrU (1,30) "1",mkStrU (1,34) "2"]) $ run     "table([[0],[1]],{not:[],col:[\"1\",\"2\"]})"

                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (1,9) 0]] [])                                                      $ runWith "table(f(),{})"                         [("f",mkArrC (0,0) [mkArrC (0,0) [mkNumU (1,9) 0]])]
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (1,9) 0]] [])                                                      $ runWith "table(f(),{not:[]})"                   [("f",mkArrC (0,0) [mkArrC (0,0) [mkNumU (1,9) 0]])]
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (1,9) 0]] [mkStrU (1,17) "1"])                                     $ runWith "table(f(),{col:[\"1\"]})"              [("f",mkArrC (0,0) [mkArrC (0,0) [mkNumU (1,9) 0]])]
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (1,9) 0]] [mkStrU (1,24) "1"])                                     $ runWith "table(f(),{not:[],col:[\"1\"]})"       [("f",mkArrC (0,0) [mkArrC (0,0) [mkNumU (1,9) 0]])]

                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (1,9) 0],[mkNumU (1,13) 1]] [])                                    $ runWith "table(f(),{})"                         [("f",mkArrC (0,0) [mkArrC (0,0) [mkNumU (1,9) 0],mkArrC (0,0) [mkNumU (1,13) 1]])]
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (1,9) 0],[mkNumU (1,13) 1]] [])                                    $ runWith "table(f(),{not:[]})"                   [("f",mkArrC (0,0) [mkArrC (0,0) [mkNumU (1,9) 0],mkArrC (0,0) [mkNumU (1,13) 1]])]
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (1,9) 0],[mkNumU (1,13) 1]] [mkStrU (1,17) "1",mkStrU (1,21) "2"]) $ runWith "table(f(),{col:[\"1\",\"2\"]})"        [("f",mkArrC (0,0) [mkArrC (0,0) [mkNumU (1,9) 0],mkArrC (0,0) [mkNumU (1,13) 1]])]
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (1,9) 0],[mkNumU (1,13) 1]] [mkStrU (1,24) "1",mkStrU (1,28) "2"]) $ runWith "table(f(),{not:[],col:[\"1\",\"2\"]})" [("f",mkArrC (0,0) [mkArrC (0,0) [mkNumU (1,9) 0],mkArrC (0,0) [mkNumU (1,13) 1]])]

                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (1,9) 0]] [])                                                      $ runWith "table([[0]],f())"     [("f",mkObjC (0,0) [])]
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (1,9) 0]] [])                                                      $ runWith "table([[0]],f())"     [("f",mkObjC (0,0) [("not",mkArrC (0,0) [])])]
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (1,9) 0]] [mkStrU (1,19) "1"])                                     $ runWith "table([[0]],f())"     [("f",mkObjC (0,0) [("col",mkArrC (0,0) [mkStrU (1,19) "1"])])]
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (1,9) 0]] [mkStrU (1,26) "1"])                                     $ runWith "table([[0]],f())"     [("f",mkObjC (0,0) [("not",mkArrC (0,0) []),("col",mkArrC (0,0) [mkStrU (1,26) "1"])])]

                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (1,9) 0],[mkNumU (1,13) 1]] [])                                    $ runWith "table([[0],[1]],f())" [("f",mkObjC (0,0) [])]
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (1,9) 0],[mkNumU (1,13) 1]] [])                                    $ runWith "table([[0],[1]],f())" [("f",mkObjC (0,0) [("not",mkArrC (0,0) [])])]
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (1,9) 0],[mkNumU (1,13) 1]] [mkStrU (1,23) "1",mkStrU (1,27) "2"]) $ runWith "table([[0],[1]],f())" [("f",mkObjC (0,0) [("col",mkArrC (0,0) [mkStrU (1,23) "1",mkStrU (1,27) "2"])])]
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (1,9) 0],[mkNumU (1,13) 1]] [mkStrU (1,34) "1",mkStrU (1,38) "2"]) $ runWith "table([[0],[1]],f())" [("f",mkObjC (0,0) [("not",mkArrC (0,0) []),("col",mkArrC (0,0) [mkStrU (1,34) "1",mkStrU (1,38) "2"])])]

                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (1,9) 0]] [])                                                      $ runWith "table(f(),g())" [("f",mkArrC (0,0) [mkArrC (0,0) [mkNumU (1,9) 0]]),                             ("g",mkObjC (0,0) [])]
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (1,9) 0]] [])                                                      $ runWith "table(g(),f())" [("g",mkArrC (0,0) [mkArrC (0,0) [mkNumU (1,9) 0]]),                             ("f",mkObjC (0,0) [("not",mkArrC (0,0) [])])]
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (1,9) 0]] [mkStrU (1,17) "1"])                                     $ runWith "table(f(),g())" [("f",mkArrC (0,0) [mkArrC (0,0) [mkNumU (1,9) 0]]),                             ("g",mkObjC (0,0) [("col",mkArrC (0,0) [mkStrU (1,17) "1"])])]
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (1,9) 0]] [mkStrU (1,28) "1"])                                     $ runWith "table(g(),f())" [("g",mkArrC (0,0) [mkArrC (0,0) [mkNumU (1,9) 0]]),                             ("f",mkObjC (0,0) [("not",mkArrC (0,0) []),("col",mkArrC (0,0) [mkStrU (1,28) "1"])])]

                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (1,9) 0],[mkNumU (1,13) 1]] [])                                    $ runWith "table(f(),g())" [("f",mkArrC (0,0) [mkArrC (0,0) [mkNumU (1,9) 0],mkArrC (0,0) [mkNumU (1,13) 1]]),  ("g",mkObjC (0,0) [])]
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (1,9) 0],[mkNumU (1,13) 1]] [])                                    $ runWith "table(g(),f())" [("g",mkArrC (0,0) [mkArrC (0,0) [mkNumU (1,9) 0],mkArrC (0,0) [mkNumU (1,13) 1]]),  ("f",mkObjC (0,0) [("not",mkArrC (0,0) [])])]
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (1,9) 0],[mkNumU (1,13) 1]] [mkStrU (1,17) "1",mkStrU (1,21) "2"]) $ runWith "table(f(),g())" [("f",mkArrC (0,0) [mkArrC (0,0) [mkNumU (1,9) 0],mkArrC (0,0) [mkNumU (1,13) 1]]),  ("g",mkObjC (0,0) [("col",mkArrC (0,0) [mkStrU (1,17) "1",mkStrU (1,21) "2"])])]
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (1,9) 0],[mkNumU (1,13) 1]] [mkStrU (1,28) "1",mkStrU (1,32) "2"]) $ runWith "table(g(),f())" [("g",mkArrC (0,0) [mkArrC (0,0) [mkNumU (1,9) 0],mkArrC (0,0) [mkNumU (1,13) 1]]),  ("f",mkObjC (0,0) [("not",mkArrC (0,0) []),("col",mkArrC (0,0) [mkStrU (1,28) "1",mkStrU (1,32) "2"])])]

test_NTimes = do assertEqual' (Right $ mkArrC (1,1) [])                               $ run     "nTimes(1,0)"
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,8) 0])                 $ run     "nTimes(0,1)"
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,8) 1, mkNumU (1,8) 1]) $ run     "nTimes(1,2)"

                 assertEqual' (Right $ mkArrC (1,1) [])                               $ runWith "nTimes(f(),0)" [("f",mkNumU (1,8) 1)]
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,8) 0])                 $ runWith "nTimes(f(),1)" [("f",mkNumU (1,8) 0)]
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,8) 1, mkNumU (1,8) 1]) $ runWith "nTimes(f(),2)" [("f",mkNumU (1,8) 1)]

                 assertEqual' (Right $ mkArrC (1,1) [])                               $ runWith "nTimes(1,f())" [("f",mkNumU (0,0) 0)]
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,8) 0])                 $ runWith "nTimes(0,f())" [("f",mkNumU (0,0) 1)]
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,8) 1, mkNumU (1,8) 1]) $ runWith "nTimes(1,f())" [("f",mkNumU (0,0) 2)]

                 assertEqual' (Right $ mkArrC (1,1) [])                               $ runWith "nTimes(f(),g())" [("f",mkNumU (1,8) 1),("g",mkNumU (0,0) 0)]
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,8) 0])                 $ runWith "nTimes(g(),f())" [("g",mkNumU (1,8) 0),("f",mkNumU (0,0) 1)]
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,8) 1, mkNumU (1,8) 1]) $ runWith "nTimes(f(),g())" [("f",mkNumU (1,8) 1),("g",mkNumU (0,0) 2)]

test_Take = do   assertEqual' (Right $ mkTableC (1,1) [] [])                                                                $ runWith "take(1,f())" [("f",mkTableC (0,0) [] [])]
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (0,0) 0]] [])                                                $ runWith "take(1,f())" [("f",mkTableC (0,0) [[mkNumU (0,0) 0]] [])]
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (0,0) 0]] [])                                                $ runWith "take(2,f())" [("f",mkTableC (0,0) [[mkNumU (0,0) 0]] [])]
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (0,0) 0]] [])                                                $ runWith "take(3,f())" [("f",mkTableC (0,0) [[mkNumU (0,0) 0]] [])]
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (0,0) 0],[mkNumU (1,0) 0]] [])                               $ runWith "take(1,f())" [("f",mkTableC (0,0) [[mkNumU (0,0) 0,mkNumU (0,1) 1],[mkNumU (1,0) 0,mkNumU (1,1) 1]] [])]
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (0,0) 0,mkNumU (0,1) 1],[mkNumU (1,0) 0,mkNumU (1,1) 1]] []) $ runWith "take(2,f())" [("f",mkTableC (0,0) [[mkNumU (0,0) 0,mkNumU (0,1) 1],[mkNumU (1,0) 0,mkNumU (1,1) 1]] [])]
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (0,0) 0,mkNumU (0,1) 1],[mkNumU (1,0) 0,mkNumU (1,1) 1]] []) $ runWith "take(3,f())" [("f",mkTableC (0,0) [[mkNumU (0,0) 0,mkNumU (0,1) 1],[mkNumU (1,0) 0,mkNumU (1,1) 1]] [])]

                 assertEqual' (Right $ mkTableC (1,1) [] [])                                                                $ runWith "take(f(),g())" [("f",mkNumU (0,0) 1),("g",mkTableC (0,0) [] [])]
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (0,0) 0]] [])                                                $ runWith "take(f(),g())" [("f",mkNumU (0,0) 1),("g",mkTableC (0,0) [[mkNumU (0,0) 0]] [])]
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (0,0) 0]] [])                                                $ runWith "take(f(),g())" [("f",mkNumU (0,0) 2),("g",mkTableC (0,0) [[mkNumU (0,0) 0]] [])]
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (0,0) 0]] [])                                                $ runWith "take(f(),g())" [("f",mkNumU (0,0) 3),("g",mkTableC (0,0) [[mkNumU (0,0) 0]] [])]
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (0,0) 0],[mkNumU (1,0) 0]] [])                               $ runWith "take(f(),g())" [("f",mkNumU (0,0) 1),("g",mkTableC (0,0) [[mkNumU (0,0) 0,mkNumU (0,1) 1],[mkNumU (1,0) 0,mkNumU (1,1) 1]] [])]
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (0,0) 0,mkNumU (0,1) 1],[mkNumU (1,0) 0,mkNumU (1,1) 1]] []) $ runWith "take(f(),g())" [("f",mkNumU (0,0) 2),("g",mkTableC (0,0) [[mkNumU (0,0) 0,mkNumU (0,1) 1],[mkNumU (1,0) 0,mkNumU (1,1) 1]] [])]
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (0,0) 0,mkNumU (0,1) 1],[mkNumU (1,0) 0,mkNumU (1,1) 1]] []) $ runWith "take(f(),g())" [("f",mkNumU (0,0) 3),("g",mkTableC (0,0) [[mkNumU (0,0) 0,mkNumU (0,1) 1],[mkNumU (1,0) 0,mkNumU (1,1) 1]] [])]

                 assertEqual' (Right $ mkArrC (1,1) [])                                $ run     "take(0,[])"
                 assertEqual' (Right $ mkArrC (1,1) [])                                $ run     "take(1,[])"
                 assertEqual' (Right $ mkArrC (1,1) [])                                $ run     "take(0,[0])"
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,9) 0])                  $ run     "take(1,[0])"
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,9) 0])                  $ run     "take(2,[0])"
                 assertEqual' (Right $ mkArrC (1,1) [])                                $ run     "take(0,[0,1])"
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,9) 0])                  $ run     "take(1,[0,1])"
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,9) 0,mkNumU (1,11) 1])  $ run     "take(2,[0,1])"
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,9) 0,mkNumU (1,11) 1])  $ run     "take(3,[0,1])"

                 assertEqual' (Right $ mkArrC (1,1) [])                                $ runWith "take(f(),[])"    [("f",mkNumU (0,0) 0)]
                 assertEqual' (Right $ mkArrC (1,1) [])                                $ runWith "take(f(),[])"    [("f",mkNumU (0,0) 1)]
                 assertEqual' (Right $ mkArrC (1,1) [])                                $ runWith "take(f(),[0])"   [("f",mkNumU (0,0) 0)]
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,11) 0])                 $ runWith "take(f(),[0])"   [("f",mkNumU (0,0) 1)]
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,11) 0])                 $ runWith "take(f(),[0])"   [("f",mkNumU (0,0) 2)]
                 assertEqual' (Right $ mkArrC (1,1) [])                                $ runWith "take(f(),[0,1])" [("f",mkNumU (0,0) 0)]
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,11) 0])                 $ runWith "take(f(),[0,1])" [("f",mkNumU (0,0) 1)]
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,11) 0,mkNumU (1,13) 1]) $ runWith "take(f(),[0,1])" [("f",mkNumU (0,0) 2)]
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,11) 0,mkNumU (1,13) 1]) $ runWith "take(f(),[0,1])" [("f",mkNumU (0,0) 3)]

                 assertEqual' (Right $ mkArrC (1,1) [])                                $ runWith "take(0,f())"     [("f",mkArrC (0,0) [])]
                 assertEqual' (Right $ mkArrC (1,1) [])                                $ runWith "take(1,f())"     [("f",mkArrC (0,0) [])]
                 assertEqual' (Right $ mkArrC (1,1) [])                                $ runWith "take(0,f())"     [("f",mkArrC (0,0) [mkNumU (1,9) 0])]
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,9) 0])                  $ runWith "take(1,f())"     [("f",mkArrC (0,0) [mkNumU (1,9) 0])]
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,9) 0])                  $ runWith "take(2,f())"     [("f",mkArrC (0,0) [mkNumU (1,9) 0])]
                 assertEqual' (Right $ mkArrC (1,1) [])                                $ runWith "take(0,f())"     [("f",mkArrC (0,0) [mkNumU (1,9) 0,mkNumU (1,11) 1])]
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,9) 0])                  $ runWith "take(1,f())"     [("f",mkArrC (0,0) [mkNumU (1,9) 0,mkNumU (1,11) 1])]
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,9) 0,mkNumU (1,11) 1])  $ runWith "take(2,f())"     [("f",mkArrC (0,0) [mkNumU (1,9) 0,mkNumU (1,11) 1])]
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,9) 0,mkNumU (1,11) 1])  $ runWith "take(3,f())"     [("f",mkArrC (0,0) [mkNumU (1,9) 0,mkNumU (1,11) 1])]

                 assertEqual' (Right $ mkArrC (1,1) [])                                $ runWith "take(f(),g())"   [("f",mkNumU (0,0) 0),("g",mkArrC (0,0) [])]
                 assertEqual' (Right $ mkArrC (1,1) [])                                $ runWith "take(f(),g())"   [("f",mkNumU (0,0) 1),("g",mkArrC (0,0) [])]
                 assertEqual' (Right $ mkArrC (1,1) [])                                $ runWith "take(f(),g())"   [("f",mkNumU (0,0) 0),("g",mkArrC (0,0) [mkNumU (1,11) 0])]
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,11) 0])                 $ runWith "take(f(),g())"   [("f",mkNumU (0,0) 1),("g",mkArrC (0,0) [mkNumU (1,11) 0])]
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,11) 0])                 $ runWith "take(f(),g())"   [("f",mkNumU (0,0) 2),("g",mkArrC (0,0) [mkNumU (1,11) 0])]
                 assertEqual' (Right $ mkArrC (1,1) [])                                $ runWith "take(f(),g())"   [("f",mkNumU (0,0) 0),("g",mkArrC (0,0) [mkNumU (1,11) 0,mkNumU (1,13) 1])]
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,11) 0])                 $ runWith "take(f(),g())"   [("f",mkNumU (0,0) 1),("g",mkArrC (0,0) [mkNumU (1,11) 0,mkNumU (1,13) 1])]
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,11) 0,mkNumU (1,13) 1]) $ runWith "take(f(),g())"   [("f",mkNumU (0,0) 2),("g",mkArrC (0,0) [mkNumU (1,11) 0,mkNumU (1,13) 1])]
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,11) 0,mkNumU (1,13) 1]) $ runWith "take(f(),g())"   [("f",mkNumU (0,0) 3),("g",mkArrC (0,0) [mkNumU (1,11) 0,mkNumU (1,13) 1])]

test_Sort = do   assertEqual' (Right $ mkTableC (1,1) [[mkNumU (0,0) 0],[mkNumU (0,1) 1]] [])                                                             $ runWith "sort(0,f())" [("f",mkTableC (0,0) [[mkNumU (0,0) 0],[mkNumU (0,1) 1]] [])]
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (0,0) 0],[mkNumU (0,1) 1]] [])                                                             $ runWith "sort(1,f())" [("f",mkTableC (0,0) [[mkNumU (0,0) 0],[mkNumU (0,1) 1]] [])]
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (0,1) 0,mkNumU (0,0) 1,mkNumU (0,2) 2],[mkNumU (1,1) 2,mkNumU (1,0) 3,mkNumU (1,2) 1]] []) $ runWith "sort(0,f())" [("f",mkTableC (0,0) [[mkNumU (0,0) 1,mkNumU (0,1) 0,mkNumU (0,2) 2],[mkNumU (1,0) 3,mkNumU (1,1) 2,mkNumU (1,2) 1]] [])]
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (0,2) 2,mkNumU (0,1) 0,mkNumU (0,0) 1],[mkNumU (1,2) 1,mkNumU (1,1) 2,mkNumU (1,0) 3]] []) $ runWith "sort(1,f())" [("f",mkTableC (0,0) [[mkNumU (0,0) 1,mkNumU (0,1) 0,mkNumU (0,2) 2],[mkNumU (1,0) 3,mkNumU (1,1) 2,mkNumU (1,2) 1]] [])]

                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (0,0) 0],[mkNumU (0,1) 1]] [])                                                             $ runWith "sort(f(),g())" [("f",mkNumU (1,6) 0),("g",mkTableC (0,0) [[mkNumU (0,0) 0],[mkNumU (0,1) 1]] [])]
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (0,0) 0],[mkNumU (0,1) 1]] [])                                                             $ runWith "sort(f(),g())" [("f",mkNumU (1,6) 1),("g",mkTableC (0,0) [[mkNumU (0,0) 0],[mkNumU (0,1) 1]] [])]
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (0,1) 0,mkNumU (0,0) 1,mkNumU (0,2) 2],[mkNumU (1,1) 2,mkNumU (1,0) 3,mkNumU (1,2) 1]] []) $ runWith "sort(f(),g())" [("f",mkNumU (1,6) 0),("g",mkTableC (0,0) [[mkNumU (0,0) 1,mkNumU (0,1) 0,mkNumU (0,2) 2],[mkNumU (1,0) 3,mkNumU (1,1) 2,mkNumU (1,2) 1]] [])]
                 assertEqual' (Right $ mkTableC (1,1) [[mkNumU (0,2) 2,mkNumU (0,1) 0,mkNumU (0,0) 1],[mkNumU (1,2) 1,mkNumU (1,1) 2,mkNumU (1,0) 3]] []) $ runWith "sort(f(),g())" [("f",mkNumU (1,6) 1),("g",mkTableC (0,0) [[mkNumU (0,0) 1,mkNumU (0,1) 0,mkNumU (0,2) 2],[mkNumU (1,0) 3,mkNumU (1,1) 2,mkNumU (1,2) 1]] [])]

                 assertEqual' (Right $ mkArrC (1,1) [mkArrC (1,9) [mkNumU (1,10) 0],                                mkArrC (1,13) [mkNumU (1,14) 1]])                                 $ run     "sort(0,[[0],[1]])"
                 assertEqual' (Right $ mkArrC (1,1) [mkArrC (1,9) [mkNumU (1,10) 0],                                mkArrC (1,13) [mkNumU (1,14) 1]])                                 $ run     "sort(1,[[0],[1]])"
                 assertEqual' (Right $ mkArrC (1,1) [mkArrC (1,9) [mkNumU (1,12) 0,mkNumU (1,10) 1,mkNumU (1,14) 2],mkArrC (1,17) [mkNumU (1,20) 2,mkNumU (1,18) 3,mkNumU (1,22) 1]]) $ run     "sort(0,[[1,0,2],[3,2,1]])"
                 assertEqual' (Right $ mkArrC (1,1) [mkArrC (1,9) [mkNumU (1,14) 2,mkNumU (1,12) 0,mkNumU (1,10) 1],mkArrC (1,17) [mkNumU (1,22) 1,mkNumU (1,20) 2,mkNumU (1,18) 3]]) $ run     "sort(1,[[1,0,2],[3,2,1]])"

                 assertEqual' (Right $ mkArrC (1,1) [mkArrC (1,9) [mkNumU (1,10) 0],                                mkArrC (1,13) [mkNumU (1,14) 1]])                                 $ runWith "sort(0,f())"                 [("f",mkArrC (0,0) [mkArrC (1,9) [mkNumU (1,10) 0],                            mkArrC (1,13) [mkNumU (1,14) 1]])]
                 assertEqual' (Right $ mkArrC (1,1) [mkArrC (1,9) [mkNumU (1,10) 0],                                mkArrC (1,13) [mkNumU (1,14) 1]])                                 $ runWith "sort(1,f())"                 [("f",mkArrC (0,0) [mkArrC (1,9) [mkNumU (1,10) 0],                            mkArrC (1,13) [mkNumU (1,14) 1]])]
                 assertEqual' (Right $ mkArrC (1,1) [mkArrC (1,9) [mkNumU (1,12) 0,mkNumU (1,10) 1,mkNumU (1,14) 2],mkArrC (1,17) [mkNumU (1,20) 2,mkNumU (1,18) 3,mkNumU (1,22) 1]]) $ runWith "sort(0,f())"                 [("f",mkArrC (0,0) [mkArrC (1,9) [mkNumU (1,10) 1,mkNumU (1,12) 0,mkNumU (1,14) 2],mkArrC (1,17) [mkNumU (1,18) 3,mkNumU (1,20) 2,mkNumU (1,22) 1]])]
                 assertEqual' (Right $ mkArrC (1,1) [mkArrC (1,9) [mkNumU (1,14) 2,mkNumU (1,12) 0,mkNumU (1,10) 1],mkArrC (1,17) [mkNumU (1,22) 1,mkNumU (1,20) 2,mkNumU (1,18) 3]]) $ runWith "sort(1,f())"                 [("f",mkArrC (0,0) [mkArrC (1,9) [mkNumU (1,10) 1,mkNumU (1,12) 0,mkNumU (1,14) 2],mkArrC (1,17) [mkNumU (1,18) 3,mkNumU (1,20) 2,mkNumU (1,22) 1]])]

                 assertEqual' (Right $ mkArrC (1,1) [mkArrC (1,11) [mkNumU (1,12) 0],                                 mkArrC (1,15) [mkNumU (1,16) 1]])                                $ runWith "sort(f(),[[0],[1]])"         [("f",mkNumU (0,0) 0)]
                 assertEqual' (Right $ mkArrC (1,1) [mkArrC (1,11) [mkNumU (1,12) 0],                                 mkArrC (1,15) [mkNumU (1,16) 1]])                                $ runWith "sort(f(),[[0],[1]])"         [("f",mkNumU (0,0) 1)]
                 assertEqual' (Right $ mkArrC (1,1) [mkArrC (1,11) [mkNumU (1,14) 0,mkNumU (1,12) 1,mkNumU (1,16) 2],mkArrC (1,19) [mkNumU (1,22) 2,mkNumU (1,20) 3,mkNumU (1,24) 1]]) $ runWith "sort(f(),[[1,0,2],[3,2,1]])" [("f",mkNumU (0,0) 0)]
                 assertEqual' (Right $ mkArrC (1,1) [mkArrC (1,11) [mkNumU (1,16) 2,mkNumU (1,14) 0,mkNumU (1,12) 1],mkArrC (1,19) [mkNumU (1,24) 1,mkNumU (1,22) 2,mkNumU (1,20) 3]]) $ runWith "sort(f(),[[1,0,2],[3,2,1]])" [("f",mkNumU (0,0) 1)]

                 assertEqual' (Right $ mkArrC (1,1) [mkArrC (1,9) [mkNumU (1,10) 0],                                mkArrC (1,13) [mkNumU (1,14) 1]])                                 $ runWith "sort(f(),g())"               [("f",mkNumU (0,0) 0),("g",mkArrC (1,1) [mkArrC (1,9) [mkNumU (1,10) 0],                            mkArrC (1,13) [mkNumU (1,14) 1]])]
                 assertEqual' (Right $ mkArrC (1,1) [mkArrC (1,9) [mkNumU (1,10) 0],                                mkArrC (1,13) [mkNumU (1,14) 1]])                                 $ runWith "sort(f(),g())"               [("f",mkNumU (0,0) 1),("g",mkArrC (1,1) [mkArrC (1,9) [mkNumU (1,10) 0],                            mkArrC (1,13) [mkNumU (1,14) 1]])]
                 assertEqual' (Right $ mkArrC (1,1) [mkArrC (1,9) [mkNumU (1,12) 0,mkNumU (1,10) 1,mkNumU (1,14) 2],mkArrC (1,17) [mkNumU (1,20) 2,mkNumU (1,18) 3,mkNumU (1,22) 1]]) $ runWith "sort(f(),g())"               [("f",mkNumU (0,0) 0),("g",mkArrC (1,1) [mkArrC (1,9) [mkNumU (1,10) 1,mkNumU (1,12) 0,mkNumU (1,14) 2],mkArrC (1,17) [mkNumU (1,18) 3,mkNumU (1,20) 2,mkNumU (1,22) 1]])]
                 assertEqual' (Right $ mkArrC (1,1) [mkArrC (1,9) [mkNumU (1,14) 2,mkNumU (1,12) 0,mkNumU (1,10) 1],mkArrC (1,17) [mkNumU (1,22) 1,mkNumU (1,20) 2,mkNumU (1,18) 3]]) $ runWith "sort(f(),g())"               [("f",mkNumU (0,0) 1),("g",mkArrC (1,1) [mkArrC (1,9) [mkNumU (1,10) 1,mkNumU (1,12) 0,mkNumU (1,14) 2],mkArrC (1,17) [mkNumU (1,18) 3,mkNumU (1,20) 2,mkNumU (1,22) 1]])]

test_Col = do    assertEqual' (Right $ mkArrC (1,1) [mkNumU (0,0) 0])                                  $ runWith "col(0,f())" [("f",mkTableC (0,0) [[mkNumU (0,0) 0],[mkNumU (0,1) 1]] [])]
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (0,1) 1])                                  $ runWith "col(1,f())" [("f",mkTableC (0,0) [[mkNumU (0,0) 0],[mkNumU (0,1) 1]] [])]
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (0,0) 1,mkNumU (0,1) 0,mkNumU (0,2) 2])    $ runWith "col(0,f())" [("f",mkTableC (0,0) [[mkNumU (0,0) 1,mkNumU (0,1) 0,mkNumU (0,2) 2],[mkNumU (1,0) 3,mkNumU (1,1) 2,mkNumU (1,2) 1]] [])]
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,0) 3,mkNumU (1,1) 2,mkNumU (1,2) 1])    $ runWith "col(1,f())" [("f",mkTableC (0,0) [[mkNumU (0,0) 1,mkNumU (0,1) 0,mkNumU (0,2) 2],[mkNumU (1,0) 3,mkNumU (1,1) 2,mkNumU (1,2) 1]] [])]

                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (0,0) 0])                                  $ runWith "col(f(),g())" [("f",mkNumU (1,6) 0),("g",mkTableC (0,0) [[mkNumU (0,0) 0],[mkNumU (0,1) 1]] [])]
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (0,1) 1])                                  $ runWith "col(f(),g())" [("f",mkNumU (1,6) 1),("g",mkTableC (0,0) [[mkNumU (0,0) 0],[mkNumU (0,1) 1]] [])]
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (0,0) 1,mkNumU (0,1) 0,mkNumU (0,2) 2])    $ runWith "col(f(),g())" [("f",mkNumU (1,6) 0),("g",mkTableC (0,0) [[mkNumU (0,0) 1,mkNumU (0,1) 0,mkNumU (0,2) 2],[mkNumU (1,0) 3,mkNumU (1,1) 2,mkNumU (1,2) 1]] [])]
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,0) 3,mkNumU (1,1) 2,mkNumU (1,2) 1])    $ runWith "col(f(),g())" [("f",mkNumU (1,6) 1),("g",mkTableC (0,0) [[mkNumU (0,0) 1,mkNumU (0,1) 0,mkNumU (0,2) 2],[mkNumU (1,0) 3,mkNumU (1,1) 2,mkNumU (1,2) 1]] [])]

                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,9)  0])                                 $ run     "col(0,[[0],[1]])"
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,13) 1])                                 $ run     "col(1,[[0],[1]])"
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,9)  1,mkNumU (1,11) 0,mkNumU (1,13) 2]) $ run     "col(0,[[1,0,2],[3,2,1]])"
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,17) 3,mkNumU (1,19) 2,mkNumU (1,21) 1]) $ run     "col(1,[[1,0,2],[3,2,1]])"

                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,10) 0])                                 $ runWith "col(0,f())"                 [("f",mkArrC (0,0) [mkArrC (1,9) [mkNumU (1,10) 0],                            mkArrC (1,13) [mkNumU (1,14) 1]])]
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,14) 1])                                 $ runWith "col(1,f())"                 [("f",mkArrC (0,0) [mkArrC (1,9) [mkNumU (1,10) 0],                            mkArrC (1,13) [mkNumU (1,14) 1]])]
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,10) 1,mkNumU (1,12) 0,mkNumU (1,14) 2]) $ runWith "col(0,f())"                 [("f",mkArrC (0,0) [mkArrC (1,9) [mkNumU (1,10) 1,mkNumU (1,12) 0,mkNumU (1,14) 2],mkArrC (1,17) [mkNumU (1,18) 3,mkNumU (1,20) 2,mkNumU (1,22) 1]])]
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,18) 3,mkNumU (1,20) 2,mkNumU (1,22) 1]) $ runWith "col(1,f())"                 [("f",mkArrC (0,0) [mkArrC (1,9) [mkNumU (1,10) 1,mkNumU (1,12) 0,mkNumU (1,14) 2],mkArrC (1,17) [mkNumU (1,18) 3,mkNumU (1,20) 2,mkNumU (1,22) 1]])]

                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,11) 0])                                 $ runWith "col(f(),[[0],[1]])"         [("f",mkNumU (0,0) 0)]
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,15) 1])                                 $ runWith "col(f(),[[0],[1]])"         [("f",mkNumU (0,0) 1)]
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,11) 1,mkNumU (1,13) 0,mkNumU (1,15) 2]) $ runWith "col(f(),[[1,0,2],[3,2,1]])" [("f",mkNumU (0,0) 0)]
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,19) 3,mkNumU (1,21) 2,mkNumU (1,23) 1]) $ runWith "col(f(),[[1,0,2],[3,2,1]])" [("f",mkNumU (0,0) 1)]

                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,10) 0])                                 $ runWith "col(f(),g())"               [("f",mkNumU (0,0) 0),("g",mkArrC (1,1) [mkArrC (1,9) [mkNumU (1,10) 0],                            mkArrC (1,13) [mkNumU (1,14) 1]])]
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,14) 1])                                 $ runWith "col(f(),g())"               [("f",mkNumU (0,0) 1),("g",mkArrC (1,1) [mkArrC (1,9) [mkNumU (1,10) 0],                            mkArrC (1,13) [mkNumU (1,14) 1]])]
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,10) 1,mkNumU (1,12) 0,mkNumU (1,14) 2]) $ runWith "col(f(),g())"               [("f",mkNumU (0,0) 0),("g",mkArrC (1,1) [mkArrC (1,9) [mkNumU (1,10) 1,mkNumU (1,12) 0,mkNumU (1,14) 2],mkArrC (1,17) [mkNumU (1,18) 3,mkNumU (1,20) 2,mkNumU (1,22) 1]])]
                 assertEqual' (Right $ mkArrC (1,1) [mkNumU (1,18) 3,mkNumU (1,20) 2,mkNumU (1,22) 1]) $ runWith "col(f(),g())"               [("f",mkNumU (0,0) 1),("g",mkArrC (1,1) [mkArrC (1,9) [mkNumU (1,10) 1,mkNumU (1,12) 0,mkNumU (1,14) 2],mkArrC (1,17) [mkNumU (1,18) 3,mkNumU (1,20) 2,mkNumU (1,22) 1]])]

assertEqual' :: (Show a,Eq a) => a -> a -> IO ()
assertEqual' = assertEqual