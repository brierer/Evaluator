{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Engine.EngineUnitFailureConstraint where

import Data.EvalError
import Data.ExpObj
import Test.Framework

import Engine.EngineUnitFailureUtils
import Marshall.MarshallUtils

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

test_MultiEmpty = do assertEqual (Left $ IllegalEmpty (1,7))  $ run     "multi([])"
                     assertEqual (Left $ IllegalEmpty (1,7))  $ runWith "multi(f())" [("f",[],constF $ ArrO (1,7) [])]

test_MeanEmpty  = do assertEqual (Left $ IllegalEmpty (1,6))  $ run     "mean([])"
                     assertEqual (Left $ IllegalEmpty (1,6))  $ runWith "mean(f())" [("f",[],constF $ ArrO (1,6) [])] 
                     
test_DescEmpty  = do assertEqual (Left $ IllegalEmpty (1,13)) $ run     "descriptive([])"
                     assertEqual (Left $ IllegalEmpty (1,13)) $ runWith "descriptive(f())" [("f",[],constF $ ArrO (1,13) [])]
                      
test_TableEmpty = do assertEqual (Left $ IllegalEmpty (1,7))  $ run     "table([],{})"
                     assertEqual (Left $ IllegalEmpty (1,8))  $ run     "table([[]],{})"
                     assertEqual (Left $ IllegalEmpty (1,12)) $ run     "table([[0],[]],{})"
                     
                     assertEqual (Left $ IllegalEmpty (1,7))  $ runWith "table(f(),{})"       [("f",[],constF $ ArrO (1,7)  [])]
                     assertEqual (Left $ IllegalEmpty (1,8))  $ runWith "table([f()],{})"     [("f",[],constF $ ArrO (1,8)  [])]
                     assertEqual (Left $ IllegalEmpty (1,13)) $ runWith "table([[0],f()],{})" [("f",[],constF $ ArrO (1,13) [])]

test_SortEmpty = do  assertEqual (Left $ IllegalEmpty (1,8))  $ run     "sort(0,[])"
                     assertEqual (Left $ IllegalEmpty (1,9))  $ run     "sort(0,[[]])"
                     assertEqual (Left $ IllegalEmpty (1,13)) $ run     "sort(0,[[0],[]])"
                     
                     assertEqual (Left $ IllegalEmpty (1,8))  $ runWith "sort(0,f())"       [("f",[],constF $ ArrO (1,8)  [])]
                     assertEqual (Left $ IllegalEmpty (1,9))  $ runWith "sort(0,[f()])"     [("f",[],constF $ ArrO (1,9)  [])]
                     assertEqual (Left $ IllegalEmpty (1,14)) $ runWith "sort(0,[[0],f()])" [("f",[],constF $ ArrO (1,14) [])]

test_ColEmpty = do   assertEqual (Left $ IllegalEmpty (1,7))  $ run     "col(0,[])"
                     assertEqual (Left $ IllegalEmpty (1,8))  $ run     "col(0,[[]])"
                     assertEqual (Left $ IllegalEmpty (1,12)) $ run     "col(0,[[0],[]])"
                     
                     assertEqual (Left $ IllegalEmpty (1,7))  $ runWith "col(0,f())"       [("f",[],constF $ ArrO (1,7)  [])]
                     assertEqual (Left $ IllegalEmpty (1,8))  $ runWith "col(0,[f()])"     [("f",[],constF $ ArrO (1,8)  [])]
                     assertEqual (Left $ IllegalEmpty (1,13)) $ runWith "col(0,[[0],f()])" [("f",[],constF $ ArrO (1,13) [])]

test_TableColumnLength = do
                     assertEqual (Left $ TableColumnLengthMismatch (1,12) 1 2) $ run     "table([[0],[0,0]],{})"
                     assertEqual (Left $ TableColumnLengthMismatch (1,14) 2 1) $ run     "table([[0,0],[0]],{})"
                     
                     assertEqual (Left $ TableColumnLengthMismatch (1,12) 1 2) $ runWith "table([[0],f()],{})"   [("f",[],constF $ ArrO (1,12) [numO 0,numO 0])]
                     assertEqual (Left $ TableColumnLengthMismatch (1,14) 2 1) $ runWith "table([[0,0],f()],{})" [("f",[],constF $ ArrO (1,14) [numO 0])]
                     
                     assertEqual (Left $ TableColumnLengthMismatch (1,12) 1 2) $ runWith "table([f(),[0,0]],{})" [("f",[],constF $ ArrO (1,8) [numO 0])]
                     assertEqual (Left $ TableColumnLengthMismatch (1,12) 2 1) $ runWith "table([f(),[0]],{})"   [("f",[],constF $ ArrO (1,8) [numO 0,NumO (1,11) 0])]
                     
                     assertEqual (Left $ TableColumnLengthMismatch (1,12) 1 2) $ runWith "table([f(),g()],{})"   [("f",[],constF $ arrO [numO 0]),             ("g",[],constF $ ArrO (1,12) [numO 0,numO 0])]    
                     assertEqual (Left $ TableColumnLengthMismatch (1,12) 2 1) $ runWith "table([f(),g()],{})"   [("f",[],constF $ arrO [numO 0,numO 0]),("g",[],constF $ ArrO (1,12) [numO 0])]                  
                          
test_TableHeaderLength = do
                     assertEqual (Left $ TableHeaderLengthMismatch (1,18) 1 2) $ run     "table([[0]],{col:[\"1\",\"2\"]})"
                     assertEqual (Left $ TableHeaderLengthMismatch (1,26) 2 1) $ run     "table([[0,0],[0,0]],{col:[\"1\"]})"
                                              
                     assertEqual (Left $ TableHeaderLengthMismatch (1,13) 1 2) $ runWith "table([[0]],f())"               [("f",[],constF $ objO [("col",ArrO (1,13) [strO "1",strO "2"])])]
                     assertEqual (Left $ TableHeaderLengthMismatch (1,21) 2 1) $ runWith "table([[0,0],[0,0]],f())"       [("f",[],constF $ objO [("col",ArrO (1,21) [strO "1"])])]
                                              
                     assertEqual (Left $ TableHeaderLengthMismatch (1,16) 1 2) $ runWith "table(f(),{col:[\"1\",\"2\"]})" [("f",[],constF $ arrO [arrO [numO 0]])]
                     assertEqual (Left $ TableHeaderLengthMismatch (1,16) 2 1) $ runWith "table(f(),{col:[\"1\"]})"       [("f",[],constF $ arrO [arrO [numO 0],arrO [numO 0]])]
                                              
                     assertEqual (Left $ TableHeaderLengthMismatch (1,11) 1 2) $ runWith "table(f(),g())"                 [("f",[],constF $ arrO [arrO [numO 0]]),                          ("g",[],constF $ objO [("col",ArrO (1,11) [strO "1",strO "2"])])]
                     assertEqual (Left $ TableHeaderLengthMismatch (1,11) 2 1) $ runWith "table(f(),g())"                 [("f",[],constF $ arrO [arrO [numO 0],arrO [numO 0]]),("g",[],constF $ objO [("col",ArrO (1,11) [strO "1"])])]
                         
--
--prop_TableColumnLengthMismatch w1aps = tableColumnLengthCase (map (un *** uns) w1aps)
--prop_TableHeaderLengthMismatch = tableHeaderLengthCase.un
--
--prop_IndexOutOfBoundsSortTable (P pf) (NumTA _ a1@(NumT pn _ _ v)) (TableOA a2tr@(TableO _ cols header)) = (v < 0 || floor v > length cols) && any (not.null) cols ==>
--  let (n,a2t,fs,expected) = mkOutOfBoundsTable pn v a2tr cols in
--   expected == applyFunc fs    pf "sort" [a1,a2t] &&
--   expected == evalStateT (sortTF pf pn n cols header) []
--prop_IndexOutOfBoundsSortTable _ x y = error $ "EngineTest::prop_IndexOutOfBoundsSortTable [Unexpected pattern ["++show x++"] and ["++show y++"]]"
--
--prop_IndexOutOfBoundsSortArray (P pf) (NumTA _ a1@(NumT pn _ _ v)) (TableValidArgs g2ss _) = (v < 0 || floor v > length g2ss) && any (not.null) g2ss ==>
--  let (n,aOfArrays,mArrays,expected) = mkOutOfBoundsArray pn v g2ss in
--   expected == applyFunc funcs pf "sort" [a1,aOfArrays] &&
--   expected == evalStateT (sortAF pf pn n mArrays) []
--prop_IndexOutOfBoundsSortArray _ x _ = error $ "EngineTest::prop_IndexOutOfBoundsSortArray [Unexpected pattern ["++show x++"]]"
--
--prop_IndexOutOfBoundsColTable (P pf) (NumTA _ a1@(NumT pn _ _ v)) (TableOA a2tr@(TableO _ cols _)) = (v < 0 || floor v > length cols) && any (not.null) cols ==>
--  let (n,a2t,fs,expected) = mkOutOfBoundsTable pn v a2tr cols in
--   expected == applyFunc fs    pf "col" [a1,a2t] &&
--   expected == evalStateT (colTF pf pn n cols) []
--prop_IndexOutOfBoundsColTable _ x y = error $ "EngineTest::prop_IndexOutOfBoundsColTable [Unexpected pattern ["++show x++"] and ["++show y++"]]"
--
--prop_IndexOutOfBoundsColArray (P pf) (NumTA _ a1@(NumT pn _ _ v)) (TableValidArgs g2ss _)  = (v < 0 || floor v > length g2ss) && any (not.null) g2ss ==>
--  let (n,aOfArrays,mArrays,expected) = mkOutOfBoundsArray pn v g2ss in
--   expected == applyFunc funcs pf "col" [a1,aOfArrays] &&
--   expected == evalStateT (colAF pf pn n mArrays) []
--prop_IndexOutOfBoundsColArray _ x _ = error $ "EngineTest::prop_IndexOutOfBoundsColArray [Unexpected pattern ["++show x++"]]"
--
