{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Engine.EngineUnitFailureType where

import Data.EvalError
import Data.ExpObj
import Data.Type
import Test.Framework

import Engine.EngineUnitFailureUtils

{-# ANN module "HLint: ignore Use camelCase" #-}

test_NbArgs = do     assertEqual(Left $ ArgCountMismatch (1,1) "show"  1 0) $ run "show()"
                     assertEqual(Left $ ArgCountMismatch (1,1) "show"  1 2) $ run "show(0,0)"
                     
                     assertEqual(Left $ ArgCountMismatch (1,1) "multi" 1 0) $ run "multi()"
                     assertEqual(Left $ ArgCountMismatch (1,1) "multi" 1 2) $ run "multi(0,0)"
                     
                     assertEqual(Left $ ArgCountMismatch (1,1) "mean"  1 0) $ run "mean()"
                     assertEqual(Left $ ArgCountMismatch (1,1) "mean"  1 2) $ run "mean(0,0)"
                     
                     assertEqual(Left $ ArgCountMismatch (1,1) "descriptive"  1 0) $ run "descriptive()"
                     assertEqual(Left $ ArgCountMismatch (1,1) "descriptive"  1 2) $ run "descriptive(0,0)"
                     
                     assertEqual(Left $ ArgCountMismatch (1,1) "table"  2 0) $ run "table()"
                     assertEqual(Left $ ArgCountMismatch (1,1) "table"  2 1) $ run "table(0)"
                     assertEqual(Left $ ArgCountMismatch (1,1) "table"  2 3) $ run "table(0,0,0)"
                     
                     assertEqual(Left $ ArgCountMismatch (1,1) "nTimes"  2 0) $ run "nTimes()"
                     assertEqual(Left $ ArgCountMismatch (1,1) "nTimes"  2 1) $ run "nTimes(0)"
                     assertEqual(Left $ ArgCountMismatch (1,1) "nTimes"  2 3) $ run "nTimes(0,0,0)"
                     
                     assertEqual(Left $ ArgCountMismatch (1,1) "take"  2 0) $ run "take()"
                     assertEqual(Left $ ArgCountMismatch (1,1) "take"  2 1) $ run "take(0)"
                     assertEqual(Left $ ArgCountMismatch (1,1) "take"  2 3) $ run "take(0,0,0)"
                     
                     assertEqual(Left $ ArgCountMismatch (1,1) "sort"  2 0) $ run "sort()"
                     assertEqual(Left $ ArgCountMismatch (1,1) "sort"  2 1) $ run "sort(0)"
                     assertEqual(Left $ ArgCountMismatch (1,1) "sort"  2 3) $ run "sort(0,0,0)"
                     
                     assertEqual(Left $ ArgCountMismatch (1,1) "col"  2 0) $ run "col()"
                     assertEqual(Left $ ArgCountMismatch (1,1) "col"  2 1) $ run "col(0)"
                     assertEqual(Left $ ArgCountMismatch (1,1) "col"  2 3) $ run "col(0,0,0)"
                     
                     assertEqual(Left $ ArgCountMismatch (1,1) "plot"  3 0) $ run "plot()"
                     assertEqual(Left $ ArgCountMismatch (1,1) "plot"  3 1) $ run "plot(0)"
                     assertEqual(Left $ ArgCountMismatch (1,1) "plot"  3 2) $ run "plot(0,0)"
                     assertEqual(Left $ ArgCountMismatch (1,1) "plot"  3 4) $ run "plot(0,0,0,0)"
                     
test_TypeShow = do   assertEqual(leafTypeMismatch (1,6) NodeArr LeafTable) $ runWith "show(f())" [("f",[],constF $ TableO (1,6) [] [])]
                     assertEqual(leafTypeMismatch (1,6) NodeArr LeafPlot)  $ runWith "show(f())" [("f",[],constF $ PlotO  (1,6) [] [])]
                     assertEqual(leafTypeMismatch (1,6) NodeArr NodeObj)   $ run     "show({})"
                     assertEqual(leafTypeMismatch (1,6) NodeArr LeafStr)   $ run     "show(\"\")"
                     assertEqual(leafTypeMismatch (1,6) NodeArr LeafNum)   $ run     "show(0)"
                     assertEqual(leafTypeMismatch (1,6) NodeArr LeafBool)  $ run     "show(false)"
                     assertEqual(leafTypeMismatch (1,6) NodeArr LeafNull)  $ run     "show(null)"
                     
                     assertEqual(orTypeMismatch (1,7) showable NodeArr)  $ run "show([[]])"
                     assertEqual(orTypeMismatch (1,7) showable NodeObj)  $ run "show([{}])"
                     assertEqual(orTypeMismatch (1,7) showable LeafStr)  $ run "show([\"\"])"
                     assertEqual(orTypeMismatch (1,7) showable LeafNum)  $ run "show([0])"
                     assertEqual(orTypeMismatch (1,7) showable LeafBool) $ run "show([false])"
                     assertEqual(orTypeMismatch (1,7) showable LeafNull) $ run "show([null])"

test_TypeMulti = do  assertEqual(leafTypeMismatch (1,7) NodeArr LeafTable) $ runWith "multi(f())" [("f",[],constF $ TableO (1,7) [] [])]
                     assertEqual(leafTypeMismatch (1,7) NodeArr LeafPlot)  $ runWith "multi(f())" [("f",[],constF $ PlotO  (1,7) [] [])]
                     assertEqual(leafTypeMismatch (1,7) NodeArr NodeObj)   $ run     "multi({})"
                     assertEqual(leafTypeMismatch (1,7) NodeArr LeafStr)   $ run     "multi(\"\")"
                     assertEqual(leafTypeMismatch (1,7) NodeArr LeafNum)   $ run     "multi(0)"
                     assertEqual(leafTypeMismatch (1,7) NodeArr LeafBool)  $ run     "multi(false)"
                     assertEqual(leafTypeMismatch (1,7) NodeArr LeafNull)  $ run     "multi(null)"
                     
                     assertEqual(orTypeMismatch (1,8) atom LeafTable) $ runWith "multi([f()])" [("f",[],constF $ TableO (1,8) [] [])]
                     assertEqual(orTypeMismatch (1,8) atom LeafPlot)  $ runWith "multi([f()])" [("f",[],constF $ PlotO  (1,8) [] [])]
                     assertEqual(orTypeMismatch (1,8) atom NodeArr)   $ run     "multi([[]])"
                     assertEqual(orTypeMismatch (1,8) atom NodeObj)   $ run     "multi([{}])"
                     
test_TypeMean = do   assertEqual(leafTypeMismatch (1,6) NodeArr LeafTable) $ runWith "mean(f())" [("f",[],constF $ TableO (1,6) [] [])]
                     assertEqual(leafTypeMismatch (1,6) NodeArr LeafPlot)  $ runWith "mean(f())" [("f",[],constF $ PlotO  (1,6) [] [])]
                     assertEqual(leafTypeMismatch (1,6) NodeArr NodeObj)   $ run     "mean({})"
                     assertEqual(leafTypeMismatch (1,6) NodeArr LeafStr)   $ run     "mean(\"\")"
                     assertEqual(leafTypeMismatch (1,6) NodeArr LeafNum)   $ run     "mean(0)"
                     assertEqual(leafTypeMismatch (1,6) NodeArr LeafBool)  $ run     "mean(false)"
                     assertEqual(leafTypeMismatch (1,6) NodeArr LeafNull)  $ run     "mean(null)"
                     
                     assertEqual(orTypeMismatch (1,7) atom LeafTable) $ runWith "mean([f()])" [("f",[],constF $ TableO (1,7) [] [])]
                     assertEqual(orTypeMismatch (1,7) atom LeafPlot)  $ runWith "mean([f()])" [("f",[],constF $ PlotO  (1,7) [] [])]
                     assertEqual(orTypeMismatch (1,7) atom NodeArr)   $ run     "mean([[]])"
                     assertEqual(orTypeMismatch (1,7) atom NodeObj)   $ run     "mean([{}])"
                     
test_TypeDesc = do   assertEqual(leafTypeMismatch (1,13) NodeArr LeafTable) $ runWith "descriptive(f())" [("f",[],constF $ TableO (1,13) [] [])]
                     assertEqual(leafTypeMismatch (1,13) NodeArr LeafPlot)  $ runWith "descriptive(f())" [("f",[],constF $ PlotO  (1,13) [] [])]
                     assertEqual(leafTypeMismatch (1,13) NodeArr NodeObj)   $ run     "descriptive({})"
                     assertEqual(leafTypeMismatch (1,13) NodeArr LeafStr)   $ run     "descriptive(\"\")"
                     assertEqual(leafTypeMismatch (1,13) NodeArr LeafNum)   $ run     "descriptive(0)"
                     assertEqual(leafTypeMismatch (1,13) NodeArr LeafBool)  $ run     "descriptive(false)"
                     assertEqual(leafTypeMismatch (1,13) NodeArr LeafNull)  $ run     "descriptive(null)"
                     
                     assertEqual(orTypeMismatch (1,14) atom LeafTable) $ runWith "descriptive([f()])" [("f",[],constF $ TableO (1,14) [] [])]
                     assertEqual(orTypeMismatch (1,14) atom LeafPlot)  $ runWith "descriptive([f()])" [("f",[],constF $ PlotO  (1,14) [] [])]
                     assertEqual(orTypeMismatch (1,14) atom NodeArr)   $ run     "descriptive([[]])"
                     assertEqual(orTypeMismatch (1,14) atom NodeObj)   $ run     "descriptive([{}])"
                     
test_TypeTable = do  assertEqual(leafTypeMismatch (1,7) NodeArr LeafTable) $ runWith "table(f(),{})" [("f",[],constF $ TableO (1,7) [] [])]
                     assertEqual(leafTypeMismatch (1,7) NodeArr LeafPlot)  $ runWith "table(f(),{})" [("f",[],constF $ PlotO  (1,7) [] [])]
                     assertEqual(leafTypeMismatch (1,7) NodeArr NodeObj)   $ run     "table({},{})"
                     assertEqual(leafTypeMismatch (1,7) NodeArr LeafStr)   $ run     "table(\"\",{})"
                     assertEqual(leafTypeMismatch (1,7) NodeArr LeafNum)   $ run     "table(0,{})"
                     assertEqual(leafTypeMismatch (1,7) NodeArr LeafBool)  $ run     "table(false,{})"
                     assertEqual(leafTypeMismatch (1,7) NodeArr LeafNull)  $ run     "table(null,{})"
                     
                     assertEqual(leafTypeMismatch (1,7) NodeArr LeafTable) $ runWith "table(f(),0)" [("f",[],constF $ TableO (1,7) [] [])]
                     assertEqual(leafTypeMismatch (1,7) NodeArr LeafPlot)  $ runWith "table(f(),0)" [("f",[],constF $ PlotO  (1,7) [] [])]
                     assertEqual(leafTypeMismatch (1,7) NodeArr NodeObj)   $ run     "table({},0)"
                     assertEqual(leafTypeMismatch (1,7) NodeArr LeafStr)   $ run     "table(\"\",0)"
                     assertEqual(leafTypeMismatch (1,7) NodeArr LeafNum)   $ run     "table(0,0)"
                     assertEqual(leafTypeMismatch (1,7) NodeArr LeafBool)  $ run     "table(false,0)"
                     assertEqual(leafTypeMismatch (1,7) NodeArr LeafNull)  $ run     "table(null,0)"
                     
                     assertEqual(leafTypeMismatch (1,8) NodeArr LeafTable) $ runWith "table([f()],{})" [("f",[],constF $ TableO (1,8) [] [])]
                     assertEqual(leafTypeMismatch (1,8) NodeArr LeafPlot)  $ runWith "table([f()],{})" [("f",[],constF $ PlotO  (1,8) [] [])]
                     assertEqual(leafTypeMismatch (1,8) NodeArr NodeObj)   $ run     "table([{}],{})"
                     assertEqual(leafTypeMismatch (1,8) NodeArr LeafStr)   $ run     "table([\"\"],{})"
                     assertEqual(leafTypeMismatch (1,8) NodeArr LeafNum)   $ run     "table([0],{})"
                     assertEqual(leafTypeMismatch (1,8) NodeArr LeafBool)  $ run     "table([false],{})"
                     assertEqual(leafTypeMismatch (1,8) NodeArr LeafNull)  $ run     "table([null],{})"
                     
                     assertEqual(leafTypeMismatch (1,8) NodeArr LeafTable) $ runWith "table([f()],0)" [("f",[],constF $ TableO (1,8) [] [])]
                     assertEqual(leafTypeMismatch (1,8) NodeArr LeafPlot)  $ runWith "table([f()],0)" [("f",[],constF $ PlotO  (1,8) [] [])]
                     assertEqual(leafTypeMismatch (1,8) NodeArr NodeObj)   $ run     "table([{}],0)"
                     assertEqual(leafTypeMismatch (1,8) NodeArr LeafStr)   $ run     "table([\"\"],0)"
                     assertEqual(leafTypeMismatch (1,8) NodeArr LeafNum)   $ run     "table([0],0)"
                     assertEqual(leafTypeMismatch (1,8) NodeArr LeafBool)  $ run     "table([false],0)"
                     assertEqual(leafTypeMismatch (1,8) NodeArr LeafNull)  $ run     "table([null],0)"
                     
                     assertEqual(orTypeMismatch (1,9) atom LeafTable)    $ runWith "table([[f()]],{})" [("f",[],constF $ TableO (1,9) [] [])]
                     assertEqual(orTypeMismatch (1,9) atom LeafPlot)     $ runWith "table([[f()]],{})" [("f",[],constF $ PlotO  (1,9) [] [])]
                     assertEqual(orTypeMismatch (1,9) atom NodeArr)      $ run     "table([[[]]],{})"
                     assertEqual(orTypeMismatch (1,9) atom NodeObj)      $ run     "table([[{}]],{})"
                     
                     assertEqual(orTypeMismatch (1,9) atom LeafTable)    $ runWith "table([[f()]],0)" [("f",[],constF $ TableO (1,9) [] [])]
                     assertEqual(orTypeMismatch (1,9) atom LeafPlot)     $ runWith "table([[f()]],0)" [("f",[],constF $ PlotO  (1,9) [] [])]
                     assertEqual(orTypeMismatch (1,9) atom NodeArr)      $ run     "table([[[]]],0)"
                     assertEqual(orTypeMismatch (1,9) atom NodeObj)      $ run     "table([[{}]],0)"
                     
                     assertEqual(leafTypeMismatch (1,13) NodeObj LeafTable) $ runWith "table([[0]],f())" [("f",[],constF $ TableO (1,13) [] [])]
                     assertEqual(leafTypeMismatch (1,13) NodeObj LeafPlot)  $ runWith "table([[0]],f())" [("f",[],constF $ PlotO  (1,13) [] [])]
                     assertEqual(leafTypeMismatch (1,13) NodeObj NodeArr)   $ run     "table([[0]],[])"
                     assertEqual(leafTypeMismatch (1,13) NodeObj LeafStr)   $ run     "table([[0]],\"\")"
                     assertEqual(leafTypeMismatch (1,13) NodeObj LeafNum)   $ run     "table([[0]],0)"
                     assertEqual(leafTypeMismatch (1,13) NodeObj LeafBool)  $ run     "table([[0]],false)"
                     assertEqual(leafTypeMismatch (1,13) NodeObj LeafNull)  $ run     "table([[0]],null)"
                     
                     assertEqual(leafTypeMismatch (1,16) NodeArr LeafTable) $ runWith "table([[0]],{x:f()})" [("f",[],constF $ TableO (1,16) [] [])]
                     assertEqual(leafTypeMismatch (1,16) NodeArr LeafPlot)  $ runWith "table([[0]],{x:f()})" [("f",[],constF $ PlotO  (1,16) [] [])]
                     assertEqual(leafTypeMismatch (1,16) NodeArr NodeObj)   $ run     "table([[0]],{x:{}})"
                     assertEqual(leafTypeMismatch (1,16) NodeArr LeafStr)   $ run     "table([[0]],{x:\"\"})"
                     assertEqual(leafTypeMismatch (1,16) NodeArr LeafNum)   $ run     "table([[0]],{x:0})"
                     assertEqual(leafTypeMismatch (1,16) NodeArr LeafBool)  $ run     "table([[0]],{x:false})"
                     assertEqual(leafTypeMismatch (1,16) NodeArr LeafNull)  $ run     "table([[0]],{x:null})"
                     
                     assertEqual(leafTypeMismatch (1,17) LeafStr LeafTable) $ runWith "table([[0]],{x:[f()]})" [("f",[],constF $ TableO (1,17) [] [])]
                     assertEqual(leafTypeMismatch (1,17) LeafStr LeafPlot)  $ runWith "table([[0]],{x:[f()]})" [("f",[],constF $ PlotO  (1,17) [] [])]
                     assertEqual(leafTypeMismatch (1,17) LeafStr NodeArr)   $ run     "table([[0]],{x:[[]]})"
                     assertEqual(leafTypeMismatch (1,17) LeafStr NodeObj)   $ run     "table([[0]],{x:[{}]})"
                     assertEqual(leafTypeMismatch (1,17) LeafStr LeafNum)   $ run     "table([[0]],{x:[0]})"
                     assertEqual(leafTypeMismatch (1,17) LeafStr LeafBool)  $ run     "table([[0]],{x:[false]})"
                     assertEqual(leafTypeMismatch (1,17) LeafStr LeafNull)  $ run     "table([[0]],{x:[null]})"

test_TypeNTimes = do assertEqual(leafTypeMismatch (1,8) LeafNum LeafTable)  $ runWith "nTimes(f(),0)" [("f",[],constF $ TableO (1,8) [] [])]
                     assertEqual(leafTypeMismatch (1,8) LeafNum LeafPlot)   $ runWith "nTimes(f(),0)" [("f",[],constF $ PlotO  (1,8) [] [])]
                     assertEqual(leafTypeMismatch (1,8) LeafNum NodeArr)    $ run     "nTimes([],0)"
                     assertEqual(leafTypeMismatch (1,8) LeafNum NodeObj)    $ run     "nTimes({},0)" 
                     assertEqual(leafTypeMismatch (1,8) LeafNum LeafStr)    $ run     "nTimes(\"\",0)" 
                     assertEqual(leafTypeMismatch (1,8) LeafNum LeafBool)   $ run     "nTimes(false,0)" 
                     assertEqual(leafTypeMismatch (1,8) LeafNum LeafNull)   $ run     "nTimes(null,0)"  
                     
                     assertEqual(leafTypeMismatch (1,8) LeafNum LeafTable)  $ runWith "nTimes(f(),false)" [("f",[],constF $ TableO (1,8) [] [])]
                     assertEqual(leafTypeMismatch (1,8) LeafNum LeafPlot)   $ runWith "nTimes(f(),false)" [("f",[],constF $ PlotO  (1,8) [] [])]
                     assertEqual(leafTypeMismatch (1,8) LeafNum NodeArr)    $ run     "nTimes([],false)"
                     assertEqual(leafTypeMismatch (1,8) LeafNum NodeObj)    $ run     "nTimes({},false)" 
                     assertEqual(leafTypeMismatch (1,8) LeafNum LeafStr)    $ run     "nTimes(\"\",false)" 
                     assertEqual(leafTypeMismatch (1,8) LeafNum LeafBool)   $ run     "nTimes(false,false)" 
                     assertEqual(leafTypeMismatch (1,8) LeafNum LeafNull)   $ run     "nTimes(null,false)"  
                     
                     assertEqual(leafTypeMismatch (1,10) LeafNum LeafTable)  $ runWith "nTimes(0,f())" [("f",[],constF $ TableO (1,10) [] [])]
                     assertEqual(leafTypeMismatch (1,10) LeafNum LeafPlot)   $ runWith "nTimes(0,f())" [("f",[],constF $ PlotO  (1,10) [] [])]
                     assertEqual(leafTypeMismatch (1,10) LeafNum NodeArr)    $ run     "nTimes(0,[])"
                     assertEqual(leafTypeMismatch (1,10) LeafNum NodeObj)    $ run     "nTimes(0,{})" 
                     assertEqual(leafTypeMismatch (1,10) LeafNum LeafStr)    $ run     "nTimes(0,\"\")" 
                     assertEqual(leafTypeMismatch (1,10) LeafNum LeafBool)   $ run     "nTimes(0,false)" 
                     assertEqual(leafTypeMismatch (1,10) LeafNum LeafNull)   $ run     "nTimes(0,null)"  


test_TypeTake = do   assertEqual(leafTypeMismatch (1,6) LeafNum LeafTable)  $ runWith "take(f(),g())"   [("g",[],constF $ TableO (0,0) [] []),("f",[],constF $ TableO (1,6) [] [])]
                     assertEqual(leafTypeMismatch (1,6) LeafNum LeafPlot)   $ runWith "take(f(),g())"   [("g",[],constF $ TableO (0,0) [] []),("f",[],constF $ PlotO  (1,6) [] [])]
                     assertEqual(leafTypeMismatch (1,6) LeafNum NodeArr)    $ runWith "take([],g())"    [("g",[],constF $ TableO (0,0) [] [])]                                    
                     assertEqual(leafTypeMismatch (1,6) LeafNum NodeObj)    $ runWith "take({},g())"    [("g",[],constF $ TableO (0,0) [] [])]                                    
                     assertEqual(leafTypeMismatch (1,6) LeafNum LeafStr)    $ runWith "take(\"\",g())"  [("g",[],constF $ TableO (0,0) [] [])]                                    
                     assertEqual(leafTypeMismatch (1,6) LeafNum LeafBool)   $ runWith "take(false,g())" [("g",[],constF $ TableO (0,0) [] [])]                                    
                     assertEqual(leafTypeMismatch (1,6) LeafNum LeafNull)   $ runWith "take(null,g())"  [("g",[],constF $ TableO (0,0) [] [])]                                    
                     
                     assertEqual(leafTypeMismatch (1,6) LeafNum LeafTable)  $ runWith "take(f(),[])" [("f",[],constF $ TableO (1,6) [] [])]
                     assertEqual(leafTypeMismatch (1,6) LeafNum LeafPlot)   $ runWith "take(f(),[])" [("f",[],constF $ PlotO  (1,6) [] [])]
                     assertEqual(leafTypeMismatch (1,6) LeafNum NodeArr)    $ run     "take([],[])"
                     assertEqual(leafTypeMismatch (1,6) LeafNum NodeObj)    $ run     "take({},[])" 
                     assertEqual(leafTypeMismatch (1,6) LeafNum LeafStr)    $ run     "take(\"\",[])" 
                     assertEqual(leafTypeMismatch (1,6) LeafNum LeafBool)   $ run     "take(false,[])" 
                     assertEqual(leafTypeMismatch (1,6) LeafNum LeafNull)   $ run     "take(null,[])"  
                     
                     assertEqual(leafTypeMismatch (1,6) LeafNum LeafTable)  $ runWith "take(f(),0)" [("f",[],constF $ TableO (1,6) [] [])]
                     assertEqual(leafTypeMismatch (1,6) LeafNum LeafPlot)   $ runWith "take(f(),0)" [("f",[],constF $ PlotO  (1,6) [] [])]
                     assertEqual(leafTypeMismatch (1,6) LeafNum NodeArr)    $ run     "take([],0)"
                     assertEqual(leafTypeMismatch (1,6) LeafNum NodeObj)    $ run     "take({},0)" 
                     assertEqual(leafTypeMismatch (1,6) LeafNum LeafStr)    $ run     "take(\"\",0)" 
                     assertEqual(leafTypeMismatch (1,6) LeafNum LeafBool)   $ run     "take(false,0)" 
                     assertEqual(leafTypeMismatch (1,6) LeafNum LeafNull)   $ run     "take(null,0)"  
                     
                     assertEqual(orTypeMismatch (1,8) tableOrArr LeafPlot)   $ runWith "take(0,f())" [("f",[],constF $ PlotO  (1,8) [] [])]
                     assertEqual(orTypeMismatch (1,8) tableOrArr NodeObj)    $ run     "take(0,{})" 
                     assertEqual(orTypeMismatch (1,8) tableOrArr LeafStr)    $ run     "take(0,\"\")" 
                     assertEqual(orTypeMismatch (1,8) tableOrArr LeafBool)   $ run     "take(0,false)" 
                     assertEqual(orTypeMismatch (1,8) tableOrArr LeafNull)   $ run     "take(0,null)"  


test_TypeSort = do   assertEqual(leafTypeMismatch (1,6) LeafNum LeafTable)  $ runWith "sort(f(),g())"   [("g",[],constF $ TableO (0,0) [] []),("f",[],constF $ TableO (1,6) [] [])]
                     assertEqual(leafTypeMismatch (1,6) LeafNum LeafPlot)   $ runWith "sort(f(),g())"   [("g",[],constF $ TableO (0,0) [] []),("f",[],constF $ PlotO  (1,6) [] [])]
                     assertEqual(leafTypeMismatch (1,6) LeafNum NodeArr)    $ runWith "sort([],g())"    [("g",[],constF $ TableO (0,0) [] [])]                                    
                     assertEqual(leafTypeMismatch (1,6) LeafNum NodeObj)    $ runWith "sort({},g())"    [("g",[],constF $ TableO (0,0) [] [])]                                    
                     assertEqual(leafTypeMismatch (1,6) LeafNum LeafStr)    $ runWith "sort(\"\",g())"  [("g",[],constF $ TableO (0,0) [] [])]                                    
                     assertEqual(leafTypeMismatch (1,6) LeafNum LeafBool)   $ runWith "sort(false,g())" [("g",[],constF $ TableO (0,0) [] [])]                                    
                     assertEqual(leafTypeMismatch (1,6) LeafNum LeafNull)   $ runWith "sort(null,g())"  [("g",[],constF $ TableO (0,0) [] [])]                                    
                     
                     assertEqual(leafTypeMismatch (1,6) LeafNum LeafTable)  $ runWith "sort(f(),[])" [("f",[],constF $ TableO (1,6) [] [])]
                     assertEqual(leafTypeMismatch (1,6) LeafNum LeafPlot)   $ runWith "sort(f(),[])" [("f",[],constF $ PlotO  (1,6) [] [])]
                     assertEqual(leafTypeMismatch (1,6) LeafNum NodeArr)    $ run     "sort([],[])"
                     assertEqual(leafTypeMismatch (1,6) LeafNum NodeObj)    $ run     "sort({},[])" 
                     assertEqual(leafTypeMismatch (1,6) LeafNum LeafStr)    $ run     "sort(\"\",[])" 
                     assertEqual(leafTypeMismatch (1,6) LeafNum LeafBool)   $ run     "sort(false,[])" 
                     assertEqual(leafTypeMismatch (1,6) LeafNum LeafNull)   $ run     "sort(null,[])"  
                     
                     assertEqual(leafTypeMismatch (1,6) LeafNum LeafTable)  $ runWith "sort(f(),[[]])" [("f",[],constF $ TableO (1,6) [] [])]
                     assertEqual(leafTypeMismatch (1,6) LeafNum LeafPlot)   $ runWith "sort(f(),[[]])" [("f",[],constF $ PlotO  (1,6) [] [])]
                     assertEqual(leafTypeMismatch (1,6) LeafNum NodeArr)    $ run     "sort([],[[]])"
                     assertEqual(leafTypeMismatch (1,6) LeafNum NodeObj)    $ run     "sort({},[[]])" 
                     assertEqual(leafTypeMismatch (1,6) LeafNum LeafStr)    $ run     "sort(\"\",[[]])" 
                     assertEqual(leafTypeMismatch (1,6) LeafNum LeafBool)   $ run     "sort(false,[[]])" 
                     assertEqual(leafTypeMismatch (1,6) LeafNum LeafNull)   $ run     "sort(null,[[]])"  
                     
                     assertEqual(leafTypeMismatch (1,6) LeafNum LeafTable)  $ runWith "sort(f(),0)" [("f",[],constF $ TableO (1,6) [] [])]
                     assertEqual(leafTypeMismatch (1,6) LeafNum LeafPlot)   $ runWith "sort(f(),0)" [("f",[],constF $ PlotO  (1,6) [] [])]
                     assertEqual(leafTypeMismatch (1,6) LeafNum NodeArr)    $ run     "sort([],0)"
                     assertEqual(leafTypeMismatch (1,6) LeafNum NodeObj)    $ run     "sort({},0)" 
                     assertEqual(leafTypeMismatch (1,6) LeafNum LeafStr)    $ run     "sort(\"\",0)" 
                     assertEqual(leafTypeMismatch (1,6) LeafNum LeafBool)   $ run     "sort(false,0)" 
                     assertEqual(leafTypeMismatch (1,6) LeafNum LeafNull)   $ run     "sort(null,0)"  
                     
                     assertEqual(orTypeMismatch (1,8) tableOrArr LeafPlot) $ runWith "sort(0,f())" [("f",[],constF $ PlotO  (1,8) [] [])]
                     assertEqual(orTypeMismatch (1,8) tableOrArr NodeObj)  $ run     "sort(0,{})" 
                     assertEqual(orTypeMismatch (1,8) tableOrArr LeafStr)  $ run     "sort(0,\"\")" 
                     assertEqual(orTypeMismatch (1,8) tableOrArr LeafBool) $ run     "sort(0,false)" 
                     assertEqual(orTypeMismatch (1,8) tableOrArr LeafNull) $ run     "sort(0,null)"  
                     
                     assertEqual(nodeTypeMismatch [TMLeaf (1,8) LeafTable NodeArr,TMLeaf (1,9) NodeArr LeafTable]) $ runWith "sort(0,[f()])" [("f",[],constF $ TableO (1,9) [] [])]
                     assertEqual(nodeTypeMismatch [TMLeaf (1,8) LeafTable NodeArr,TMLeaf (1,9) NodeArr LeafPlot])  $ runWith "sort(0,[f()])" [("f",[],constF $ PlotO  (1,9) [] [])]
                     assertEqual(nodeTypeMismatch [TMLeaf (1,8) LeafTable NodeArr,TMLeaf (1,9) NodeArr NodeObj])   $ run     "sort(0,[{}])" 
                     assertEqual(nodeTypeMismatch [TMLeaf (1,8) LeafTable NodeArr,TMLeaf (1,9) NodeArr LeafStr])   $ run     "sort(0,[\"\"])"
                     assertEqual(nodeTypeMismatch [TMLeaf (1,8) LeafTable NodeArr,TMLeaf (1,9) NodeArr LeafNum])   $ run     "sort(0,[0])"  
                     assertEqual(nodeTypeMismatch [TMLeaf (1,8) LeafTable NodeArr,TMLeaf (1,9) NodeArr LeafBool])  $ run     "sort(0,[false])" 
                     assertEqual(nodeTypeMismatch [TMLeaf (1,8) LeafTable NodeArr,TMLeaf (1,9) NodeArr LeafNull])  $ run     "sort(0,[null])"  
                     
                     assertEqual(nodeTypeMismatch [TMLeaf (1,8) LeafTable NodeArr,orTMT (1,10) atom LeafTable])  $ runWith "sort(0,[[f()]])" [("f",[],constF $ TableO (1,10) [] [])]
                     assertEqual(nodeTypeMismatch [TMLeaf (1,8) LeafTable NodeArr,orTMT (1,10) atom LeafPlot])   $ runWith "sort(0,[[f()]])" [("f",[],constF $ PlotO  (1,10) [] [])]
                     assertEqual(nodeTypeMismatch [TMLeaf (1,8) LeafTable NodeArr,orTMT (1,10) atom NodeArr])    $ run     "sort(0,[[[]]])"
                     assertEqual(nodeTypeMismatch [TMLeaf (1,8) LeafTable NodeArr,orTMT (1,10) atom NodeObj])    $ run     "sort(0,[[{}]])" 

test_TypeCol = do    assertEqual(leafTypeMismatch (1,5) LeafNum LeafTable)  $ runWith "col(f(),g())"   [("g",[],constF $ TableO (0,0) [] []),("f",[],constF $ TableO (1,5) [] [])]
                     assertEqual(leafTypeMismatch (1,5) LeafNum LeafPlot)   $ runWith "col(f(),g())"   [("g",[],constF $ TableO (0,0) [] []),("f",[],constF $ PlotO  (1,5) [] [])]
                     assertEqual(leafTypeMismatch (1,5) LeafNum NodeArr)    $ runWith "col([],g())"    [("g",[],constF $ TableO (0,0) [] [])]                                    
                     assertEqual(leafTypeMismatch (1,5) LeafNum NodeObj)    $ runWith "col({},g())"    [("g",[],constF $ TableO (0,0) [] [])]                                    
                     assertEqual(leafTypeMismatch (1,5) LeafNum LeafStr)    $ runWith "col(\"\",g())"  [("g",[],constF $ TableO (0,0) [] [])]                                    
                     assertEqual(leafTypeMismatch (1,5) LeafNum LeafBool)   $ runWith "col(false,g())" [("g",[],constF $ TableO (0,0) [] [])]                                    
                     assertEqual(leafTypeMismatch (1,5) LeafNum LeafNull)   $ runWith "col(null,g())"  [("g",[],constF $ TableO (0,0) [] [])]                                    
                     
                     assertEqual(leafTypeMismatch (1,5) LeafNum LeafTable)  $ runWith "col(f(),[])" [("f",[],constF $ TableO (1,5) [] [])]
                     assertEqual(leafTypeMismatch (1,5) LeafNum LeafPlot)   $ runWith "col(f(),[])" [("f",[],constF $ PlotO  (1,5) [] [])]
                     assertEqual(leafTypeMismatch (1,5) LeafNum NodeArr)    $ run     "col([],[])"
                     assertEqual(leafTypeMismatch (1,5) LeafNum NodeObj)    $ run     "col({},[])" 
                     assertEqual(leafTypeMismatch (1,5) LeafNum LeafStr)    $ run     "col(\"\",[])" 
                     assertEqual(leafTypeMismatch (1,5) LeafNum LeafBool)   $ run     "col(false,[])" 
                     assertEqual(leafTypeMismatch (1,5) LeafNum LeafNull)   $ run     "col(null,[])"  
                     
                     assertEqual(leafTypeMismatch (1,5) LeafNum LeafTable)  $ runWith "col(f(),[[]])" [("f",[],constF $ TableO (1,5) [] [])]
                     assertEqual(leafTypeMismatch (1,5) LeafNum LeafPlot)   $ runWith "col(f(),[[]])" [("f",[],constF $ PlotO  (1,5) [] [])]
                     assertEqual(leafTypeMismatch (1,5) LeafNum NodeArr)    $ run     "col([],[[]])"
                     assertEqual(leafTypeMismatch (1,5) LeafNum NodeObj)    $ run     "col({},[[]])" 
                     assertEqual(leafTypeMismatch (1,5) LeafNum LeafStr)    $ run     "col(\"\",[[]])" 
                     assertEqual(leafTypeMismatch (1,5) LeafNum LeafBool)   $ run     "col(false,[[]])" 
                     assertEqual(leafTypeMismatch (1,5) LeafNum LeafNull)   $ run     "col(null,[[]])"  
                     
                     assertEqual(leafTypeMismatch (1,5) LeafNum LeafTable)  $ runWith "col(f(),0)" [("f",[],constF $ TableO (1,5) [] [])]
                     assertEqual(leafTypeMismatch (1,5) LeafNum LeafPlot)   $ runWith "col(f(),0)" [("f",[],constF $ PlotO  (1,5) [] [])]
                     assertEqual(leafTypeMismatch (1,5) LeafNum NodeArr)    $ run     "col([],0)"
                     assertEqual(leafTypeMismatch (1,5) LeafNum NodeObj)    $ run     "col({},0)" 
                     assertEqual(leafTypeMismatch (1,5) LeafNum LeafStr)    $ run     "col(\"\",0)" 
                     assertEqual(leafTypeMismatch (1,5) LeafNum LeafBool)   $ run     "col(false,0)" 
                     assertEqual(leafTypeMismatch (1,5) LeafNum LeafNull)   $ run     "col(null,0)"  
                     
                     assertEqual(orTypeMismatch (1,7) tableOrArr LeafPlot) $ runWith "col(0,f())" [("f",[],constF $ PlotO  (1,7) [] [])]
                     assertEqual(orTypeMismatch (1,7) tableOrArr NodeObj)  $ run     "col(0,{})" 
                     assertEqual(orTypeMismatch (1,7) tableOrArr LeafStr)  $ run     "col(0,\"\")" 
                     assertEqual(orTypeMismatch (1,7) tableOrArr LeafBool) $ run     "col(0,false)" 
                     assertEqual(orTypeMismatch (1,7) tableOrArr LeafNull) $ run     "col(0,null)"  
                     
                     assertEqual(nodeTypeMismatch [TMLeaf (1,7) LeafTable NodeArr,TMLeaf (1,8) NodeArr LeafTable]) $ runWith "col(0,[f()])" [("f",[],constF $ TableO (1,8) [] [])]
                     assertEqual(nodeTypeMismatch [TMLeaf (1,7) LeafTable NodeArr,TMLeaf (1,8) NodeArr LeafPlot])  $ runWith "col(0,[f()])" [("f",[],constF $ PlotO  (1,8) [] [])]
                     assertEqual(nodeTypeMismatch [TMLeaf (1,7) LeafTable NodeArr,TMLeaf (1,8) NodeArr NodeObj])   $ run     "col(0,[{}])" 
                     assertEqual(nodeTypeMismatch [TMLeaf (1,7) LeafTable NodeArr,TMLeaf (1,8) NodeArr LeafStr])   $ run     "col(0,[\"\"])"
                     assertEqual(nodeTypeMismatch [TMLeaf (1,7) LeafTable NodeArr,TMLeaf (1,8) NodeArr LeafNum])   $ run     "col(0,[0])"  
                     assertEqual(nodeTypeMismatch [TMLeaf (1,7) LeafTable NodeArr,TMLeaf (1,8) NodeArr LeafBool])  $ run     "col(0,[false])" 
                     assertEqual(nodeTypeMismatch [TMLeaf (1,7) LeafTable NodeArr,TMLeaf (1,8) NodeArr LeafNull])  $ run     "col(0,[null])"  
                     
                     assertEqual(nodeTypeMismatch [TMLeaf (1,7) LeafTable NodeArr,orTMT (1,9) atom LeafTable])  $ runWith "col(0,[[f()]])" [("f",[],constF $ TableO (1,9) [] [])]
                     assertEqual(nodeTypeMismatch [TMLeaf (1,7) LeafTable NodeArr,orTMT (1,9) atom LeafPlot])   $ runWith "col(0,[[f()]])" [("f",[],constF $ PlotO  (1,9) [] [])]
                     assertEqual(nodeTypeMismatch [TMLeaf (1,7) LeafTable NodeArr,orTMT (1,9) atom NodeArr])    $ run     "col(0,[[[]]])"
                     assertEqual(nodeTypeMismatch [TMLeaf (1,7) LeafTable NodeArr,orTMT (1,9) atom NodeObj])    $ run     "col(0,[[{}]])" 

test_TypePlot = do   assertEqual(leafTypeMismatch (1,6) NodeArr LeafTable) $ runWith "plot(f(),[],{})" [("f",[],constF $ TableO (1,6) [] [])]
                     assertEqual(leafTypeMismatch (1,6) NodeArr LeafPlot)  $ runWith "plot(f(),[],{})" [("f",[],constF $ PlotO  (1,6) [] [])]
                     assertEqual(leafTypeMismatch (1,6) NodeArr NodeObj)   $ run     "plot({},[],{})"
                     assertEqual(leafTypeMismatch (1,6) NodeArr LeafStr)   $ run     "plot(\"\",[],{})"  
                     assertEqual(leafTypeMismatch (1,6) NodeArr LeafNum)   $ run     "plot(0,[],{})"  
                     assertEqual(leafTypeMismatch (1,6) NodeArr LeafBool)  $ run     "plot(false,[],{})"  
                     assertEqual(leafTypeMismatch (1,6) NodeArr LeafNull)  $ run     "plot(null,[],{})"    
                     
                     assertEqual(leafTypeMismatch (1,6) NodeArr LeafTable) $ runWith "plot(f(),[],0)" [("f",[],constF $ TableO (1,6) [] [])]
                     assertEqual(leafTypeMismatch (1,6) NodeArr LeafPlot)  $ runWith "plot(f(),[],0)" [("f",[],constF $ PlotO  (1,6) [] [])]
                     assertEqual(leafTypeMismatch (1,6) NodeArr NodeObj)   $ run     "plot({},[],0)"
                     assertEqual(leafTypeMismatch (1,6) NodeArr LeafStr)   $ run     "plot(\"\",[],0)"  
                     assertEqual(leafTypeMismatch (1,6) NodeArr LeafNum)   $ run     "plot(0,[],0)"  
                     assertEqual(leafTypeMismatch (1,6) NodeArr LeafBool)  $ run     "plot(false,[],0)"  
                     assertEqual(leafTypeMismatch (1,6) NodeArr LeafNull)  $ run     "plot(null,[],0)"    
                     
                     assertEqual(leafTypeMismatch (1,6) NodeArr LeafTable) $ runWith "plot(f(),0,{})" [("f",[],constF $ TableO (1,6) [] [])]
                     assertEqual(leafTypeMismatch (1,6) NodeArr LeafPlot)  $ runWith "plot(f(),0,{})" [("f",[],constF $ PlotO  (1,6) [] [])]
                     assertEqual(leafTypeMismatch (1,6) NodeArr NodeObj)   $ run     "plot({},0,{})"
                     assertEqual(leafTypeMismatch (1,6) NodeArr LeafStr)   $ run     "plot(\"\",0,{})"  
                     assertEqual(leafTypeMismatch (1,6) NodeArr LeafNum)   $ run     "plot(0,0,{})"  
                     assertEqual(leafTypeMismatch (1,6) NodeArr LeafBool)  $ run     "plot(false,0,{})"  
                     assertEqual(leafTypeMismatch (1,6) NodeArr LeafNull)  $ run     "plot(null,0,{})"  
                     
                     assertEqual(leafTypeMismatch (1,6) NodeArr LeafTable) $ runWith "plot(f(),0,0)" [("f",[],constF $ TableO (1,6) [] [])]
                     assertEqual(leafTypeMismatch (1,6) NodeArr LeafPlot)  $ runWith "plot(f(),0,0)" [("f",[],constF $ PlotO  (1,6) [] [])]
                     assertEqual(leafTypeMismatch (1,6) NodeArr NodeObj)   $ run     "plot({},0,0)"
                     assertEqual(leafTypeMismatch (1,6) NodeArr LeafStr)   $ run     "plot(\"\",0,0)"  
                     assertEqual(leafTypeMismatch (1,6) NodeArr LeafNum)   $ run     "plot(0,0,0)"  
                     assertEqual(leafTypeMismatch (1,6) NodeArr LeafBool)  $ run     "plot(false,0,0)"  
                     assertEqual(leafTypeMismatch (1,6) NodeArr LeafNull)  $ run     "plot(null,0,0)"  
                     
                     assertEqual(leafTypeMismatch (1,7) LeafNum LeafTable) $ runWith "plot([f()],[],{})" [("f",[],constF $ TableO (1,7) [] [])]
                     assertEqual(leafTypeMismatch (1,7) LeafNum LeafPlot)  $ runWith "plot([f()],[],{})" [("f",[],constF $ PlotO  (1,7) [] [])]
                     assertEqual(leafTypeMismatch (1,7) LeafNum NodeArr)   $ run     "plot([[]],[],{})"
                     assertEqual(leafTypeMismatch (1,7) LeafNum NodeObj)   $ run     "plot([{}],[],{})"
                     assertEqual(leafTypeMismatch (1,7) LeafNum LeafStr)   $ run     "plot([\"\"],[],{})"  
                     assertEqual(leafTypeMismatch (1,7) LeafNum LeafBool)  $ run     "plot([false],[],{})"  
                     assertEqual(leafTypeMismatch (1,7) LeafNum LeafNull)  $ run     "plot([null],[],{})"    
                     
                     assertEqual(leafTypeMismatch (1,7) LeafNum LeafTable) $ runWith "plot([f()],[],0)" [("f",[],constF $ TableO (1,7) [] [])]
                     assertEqual(leafTypeMismatch (1,7) LeafNum LeafPlot)  $ runWith "plot([f()],[],0)" [("f",[],constF $ PlotO  (1,7) [] [])]
                     assertEqual(leafTypeMismatch (1,7) LeafNum NodeArr)   $ run     "plot([[]],[],0)"
                     assertEqual(leafTypeMismatch (1,7) LeafNum NodeObj)   $ run     "plot([{}],[],0)"
                     assertEqual(leafTypeMismatch (1,7) LeafNum LeafStr)   $ run     "plot([\"\"],[],0)"  
                     assertEqual(leafTypeMismatch (1,7) LeafNum LeafBool)  $ run     "plot([false],[],0)"  
                     assertEqual(leafTypeMismatch (1,7) LeafNum LeafNull)  $ run     "plot([null],[],0)"    
                     
                     assertEqual(leafTypeMismatch (1,7) LeafNum LeafTable) $ runWith "plot([f()],0,{})" [("f",[],constF $ TableO (1,7) [] [])]
                     assertEqual(leafTypeMismatch (1,7) LeafNum LeafPlot)  $ runWith "plot([f()],0,{})" [("f",[],constF $ PlotO  (1,7) [] [])]
                     assertEqual(leafTypeMismatch (1,7) LeafNum NodeArr)   $ run     "plot([[]],0,{})"
                     assertEqual(leafTypeMismatch (1,7) LeafNum NodeObj)   $ run     "plot([{}],0,{})"
                     assertEqual(leafTypeMismatch (1,7) LeafNum LeafStr)   $ run     "plot([\"\"],0,{})"  
                     assertEqual(leafTypeMismatch (1,7) LeafNum LeafBool)  $ run     "plot([false],0,{})"  
                     assertEqual(leafTypeMismatch (1,7) LeafNum LeafNull)  $ run     "plot([null],0,{})"  
                     
                     assertEqual(leafTypeMismatch (1,7) LeafNum LeafTable) $ runWith "plot([f()],0,0)" [("f",[],constF $ TableO (1,7) [] [])]
                     assertEqual(leafTypeMismatch (1,7) LeafNum LeafPlot)  $ runWith "plot([f()],0,0)" [("f",[],constF $ PlotO  (1,7) [] [])]
                     assertEqual(leafTypeMismatch (1,7) LeafNum NodeArr)   $ run     "plot([[]],0,0)"
                     assertEqual(leafTypeMismatch (1,7) LeafNum NodeObj)   $ run     "plot([{}],0,0)"
                     assertEqual(leafTypeMismatch (1,7) LeafNum LeafStr)   $ run     "plot([\"\"],0,0)"  
                     assertEqual(leafTypeMismatch (1,7) LeafNum LeafBool)  $ run     "plot([false],0,0)"  
                     assertEqual(leafTypeMismatch (1,7) LeafNum LeafNull)  $ run     "plot([null],0,0)"  
                     
                     assertEqual(leafTypeMismatch (1,9) NodeArr LeafTable) $ runWith "plot([],f(),{})" [("f",[],constF $ TableO (1,9) [] [])]
                     assertEqual(leafTypeMismatch (1,9) NodeArr LeafPlot)  $ runWith "plot([],f(),{})" [("f",[],constF $ PlotO  (1,9) [] [])]
                     assertEqual(leafTypeMismatch (1,9) NodeArr NodeObj)   $ run     "plot([],{},{})"
                     assertEqual(leafTypeMismatch (1,9) NodeArr LeafStr)   $ run     "plot([],\"\",{})"  
                     assertEqual(leafTypeMismatch (1,9) NodeArr LeafNum)   $ run     "plot([],0,{})"  
                     assertEqual(leafTypeMismatch (1,9) NodeArr LeafBool)  $ run     "plot([],false,{})"  
                     assertEqual(leafTypeMismatch (1,9) NodeArr LeafNull)  $ run     "plot([],null,{})"    
                     
                     assertEqual(leafTypeMismatch (1,9) NodeArr LeafTable) $ runWith "plot([],f(),0)" [("f",[],constF $ TableO (1,9) [] [])]
                     assertEqual(leafTypeMismatch (1,9) NodeArr LeafPlot)  $ runWith "plot([],f(),0)" [("f",[],constF $ PlotO  (1,9) [] [])]
                     assertEqual(leafTypeMismatch (1,9) NodeArr NodeObj)   $ run     "plot([],{},0)"
                     assertEqual(leafTypeMismatch (1,9) NodeArr LeafStr)   $ run     "plot([],\"\",0)"  
                     assertEqual(leafTypeMismatch (1,9) NodeArr LeafNum)   $ run     "plot([],0,0)"  
                     assertEqual(leafTypeMismatch (1,9) NodeArr LeafBool)  $ run     "plot([],false,0)"  
                     assertEqual(leafTypeMismatch (1,9) NodeArr LeafNull)  $ run     "plot([],null,0)"    
                     
                     assertEqual(leafTypeMismatch (1,10) LeafNum LeafTable) $ runWith "plot([],[f()],{})" [("f",[],constF $ TableO (1,10) [] [])]
                     assertEqual(leafTypeMismatch (1,10) LeafNum LeafPlot)  $ runWith "plot([],[f()],{})" [("f",[],constF $ PlotO  (1,10) [] [])]
                     assertEqual(leafTypeMismatch (1,10) LeafNum NodeArr)   $ run     "plot([],[[]],{})"
                     assertEqual(leafTypeMismatch (1,10) LeafNum NodeObj)   $ run     "plot([],[{}],{})"
                     assertEqual(leafTypeMismatch (1,10) LeafNum LeafStr)   $ run     "plot([],[\"\"],{})"  
                     assertEqual(leafTypeMismatch (1,10) LeafNum LeafBool)  $ run     "plot([],[false],{})"  
                     assertEqual(leafTypeMismatch (1,10) LeafNum LeafNull)  $ run     "plot([],[null],{})"    
                     
                     assertEqual(leafTypeMismatch (1,10) LeafNum LeafTable) $ runWith "plot([],[f()],0)" [("f",[],constF $ TableO (1,10) [] [])]
                     assertEqual(leafTypeMismatch (1,10) LeafNum LeafPlot)  $ runWith "plot([],[f()],0)" [("f",[],constF $ PlotO  (1,10) [] [])]
                     assertEqual(leafTypeMismatch (1,10) LeafNum NodeArr)   $ run     "plot([],[[]],0)"
                     assertEqual(leafTypeMismatch (1,10) LeafNum NodeObj)   $ run     "plot([],[{}],0)"
                     assertEqual(leafTypeMismatch (1,10) LeafNum LeafStr)   $ run     "plot([],[\"\"],0)"  
                     assertEqual(leafTypeMismatch (1,10) LeafNum LeafBool)  $ run     "plot([],[false],0)"  
                     assertEqual(leafTypeMismatch (1,10) LeafNum LeafNull)  $ run     "plot([],[null],0)"    
                     
                     assertEqual(leafTypeMismatch (1,12) NodeObj LeafTable) $ runWith "plot([],[],f())" [("f",[],constF $ TableO (1,12) [] [])]
                     assertEqual(leafTypeMismatch (1,12) NodeObj LeafPlot)  $ runWith "plot([],[],f())" [("f",[],constF $ PlotO  (1,12) [] [])]
                     assertEqual(leafTypeMismatch (1,12) NodeObj NodeArr)   $ run     "plot([],[],[])"
                     assertEqual(leafTypeMismatch (1,12) NodeObj LeafStr)   $ run     "plot([],[],\"\")"  
                     assertEqual(leafTypeMismatch (1,12) NodeObj LeafNum)   $ run     "plot([],[],0)"  
                     assertEqual(leafTypeMismatch (1,12) NodeObj LeafBool)  $ run     "plot([],[],false)"  
                     assertEqual(leafTypeMismatch (1,12) NodeObj LeafNull)  $ run     "plot([],[],null)"    
                     
                     assertEqual(leafTypeMismatch (1,15) LeafStr LeafTable) $ runWith "plot([],[],{x:f()})" [("f",[],constF $ TableO (1,15) [] [])]
                     assertEqual(leafTypeMismatch (1,15) LeafStr LeafPlot)  $ runWith "plot([],[],{x:f()})" [("f",[],constF $ PlotO  (1,15) [] [])]
                     assertEqual(leafTypeMismatch (1,15) LeafStr NodeArr)   $ run     "plot([],[],{x:[]})"
                     assertEqual(leafTypeMismatch (1,15) LeafStr NodeObj)   $ run     "plot([],[],{x:{}})"  
                     assertEqual(leafTypeMismatch (1,15) LeafStr LeafNum)   $ run     "plot([],[],{x:0})"  
                     assertEqual(leafTypeMismatch (1,15) LeafStr LeafBool)  $ run     "plot([],[],{x:false})"  
                     assertEqual(leafTypeMismatch (1,15) LeafStr LeafNull)  $ run     "plot([],[],{x:null})"    
