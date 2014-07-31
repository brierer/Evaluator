{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Engine.EngineUnitFailureConstraint where

import Data.EvalError
import Test.Framework

import Engine.EngineUnitUtils
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
                     assertEqual (Left $ IllegalEmpty (1,7))  $ runWith "multi(f())" [("f",mkArrC (1,7) [])]

test_MeanEmpty  = do assertEqual (Left $ IllegalEmpty (1,6))  $ run     "mean([])"
                     assertEqual (Left $ IllegalEmpty (1,6))  $ runWith "mean(f())" [("f",mkArrC (1,6) [])]

test_DescEmpty  = do assertEqual (Left $ IllegalEmpty (1,13)) $ run     "descriptive([])"
                     assertEqual (Left $ IllegalEmpty (1,13)) $ runWith "descriptive(f())" [("f",mkArrC (1,13) [])]

test_TableEmpty = do assertEqual (Left $ IllegalEmpty (1,7))  $ run     "table([],{})"
                     assertEqual (Left $ IllegalEmpty (1,8))  $ run     "table([[]],{})"
                     assertEqual (Left $ IllegalEmpty (1,12)) $ run     "table([[0],[]],{})"

                     assertEqual (Left $ IllegalEmpty (1,7))  $ runWith "table(f(),{})"       [("f",mkArrC (1,7)  [])]
                     assertEqual (Left $ IllegalEmpty (1,8))  $ runWith "table([f()],{})"     [("f",mkArrC (1,8)  [])]
                     assertEqual (Left $ IllegalEmpty (1,13)) $ runWith "table([[0],f()],{})" [("f",mkArrC (1,13) [])]

test_SortEmpty = do  assertEqual (Left $ IllegalEmpty (1,8))  $ run     "sort(0,[])"
                     assertEqual (Left $ IllegalEmpty (1,9))  $ run     "sort(0,[[]])"
                     assertEqual (Left $ IllegalEmpty (1,13)) $ run     "sort(0,[[0],[]])"

                     assertEqual (Left $ IllegalEmpty (1,8))  $ runWith "sort(0,f())"       [("f",mkArrC (1,8)  [])]
                     assertEqual (Left $ IllegalEmpty (1,9))  $ runWith "sort(0,[f()])"     [("f",mkArrC (1,9)  [])]
                     assertEqual (Left $ IllegalEmpty (1,14)) $ runWith "sort(0,[[0],f()])" [("f",mkArrC (1,14) [])]

test_ColEmpty = do   assertEqual (Left $ IllegalEmpty (1,7))  $ run     "col(0,[])"
                     assertEqual (Left $ IllegalEmpty (1,8))  $ run     "col(0,[[]])"
                     assertEqual (Left $ IllegalEmpty (1,12)) $ run     "col(0,[[0],[]])"

                     assertEqual (Left $ IllegalEmpty (1,7))  $ runWith "col(0,f())"       [("f",mkArrC (1,7)  [])]
                     assertEqual (Left $ IllegalEmpty (1,8))  $ runWith "col(0,[f()])"     [("f",mkArrC (1,8)  [])]
                     assertEqual (Left $ IllegalEmpty (1,13)) $ runWith "col(0,[[0],f()])" [("f",mkArrC (1,13) [])]

test_TableColumnLength = do
                     assertEqual (Left $ TableColumnLengthMismatch (1,12) 1 2) $ run     "table([[0],[0,0]],{})"
                     assertEqual (Left $ TableColumnLengthMismatch (1,14) 2 1) $ run     "table([[0,0],[0]],{})"

                     assertEqual (Left $ TableColumnLengthMismatch (1,12) 1 2) $ runWith "table([[0],f()],{})"   [("f",mkArrC (1,12) [numO 0,numO 0])]
                     assertEqual (Left $ TableColumnLengthMismatch (1,14) 2 1) $ runWith "table([[0,0],f()],{})" [("f",mkArrC (1,14) [numO 0])]

                     assertEqual (Left $ TableColumnLengthMismatch (1,12) 1 2) $ runWith "table([f(),[0,0]],{})" [("f",mkArrC (1,8) [numO 0])]
                     assertEqual (Left $ TableColumnLengthMismatch (1,12) 2 1) $ runWith "table([f(),[0]],{})"   [("f",mkArrC (1,8) [numO 0,numO 0])]

                     assertEqual (Left $ TableColumnLengthMismatch (1,12) 1 2) $ runWith "table([f(),g()],{})"   [("f",arrO [numO 0]),       ("g",mkArrC (1,12) [numO 0,numO 0])]
                     assertEqual (Left $ TableColumnLengthMismatch (1,12) 2 1) $ runWith "table([f(),g()],{})"   [("f",arrO [numO 0,numO 0]),("g",mkArrC (1,12) [numO 0])]

test_TableHeaderLength = do
                     assertEqual (Left $ TableHeaderLengthMismatch (1,18) 1 2) $ run     "table([[0]],{col:[\"1\",\"2\"]})"
                     assertEqual (Left $ TableHeaderLengthMismatch (1,26) 2 1) $ run     "table([[0,0],[0,0]],{col:[\"1\"]})"

                     assertEqual (Left $ TableHeaderLengthMismatch (1,13) 1 2) $ runWith "table([[0]],f())"               [("f",objO [("col",mkArrC (1,13) [strO "1",strO "2"])])]
                     assertEqual (Left $ TableHeaderLengthMismatch (1,21) 2 1) $ runWith "table([[0,0],[0,0]],f())"       [("f",objO [("col",mkArrC (1,21) [strO "1"])])]

                     assertEqual (Left $ TableHeaderLengthMismatch (1,16) 1 2) $ runWith "table(f(),{col:[\"1\",\"2\"]})" [("f",arrO [arrO [numO 0]])]
                     assertEqual (Left $ TableHeaderLengthMismatch (1,16) 2 1) $ runWith "table(f(),{col:[\"1\"]})"       [("f",arrO [arrO [numO 0],arrO [numO 0]])]

                     assertEqual (Left $ TableHeaderLengthMismatch (1,11) 1 2) $ runWith "table(f(),g())"                 [("f",arrO [arrO [numO 0]]),              ("g",objO [("col",mkArrC (1,11) [strO "1",strO "2"])])]
                     assertEqual (Left $ TableHeaderLengthMismatch (1,11) 2 1) $ runWith "table(f(),g())"                 [("f",arrO [arrO [numO 0],arrO [numO 0]]),("g",objO [("col",mkArrC (1,11) [strO "1"])])]

                     assertEqual (Left $ TableHeaderLengthMismatch (1,25) 1 2) $ run     "table([[0]],{not:[],col:[\"1\",\"2\"]})"
                     assertEqual (Left $ TableHeaderLengthMismatch (1,33) 2 1) $ run     "table([[0,0],[0,0]],{not:[],col:[\"1\"]})"

                     assertEqual (Left $ TableHeaderLengthMismatch (1,18) 1 2) $ runWith "table([[0]],f())"                      [("f",objO [("not",mkArrC (0,0) []),("col",mkArrC (1,18) [strO "1",strO "2"])])]
                     assertEqual (Left $ TableHeaderLengthMismatch (1,26) 2 1) $ runWith "table([[0,0],[0,0]],f())"              [("f",objO [("not",mkArrC (0,0) []),("col",mkArrC (1,26) [strO "1"])])]

                     assertEqual (Left $ TableHeaderLengthMismatch (1,23) 1 2) $ runWith "table(f(),{not:[],col:[\"1\",\"2\"]})" [("f",arrO [arrO [numO 0]])]
                     assertEqual (Left $ TableHeaderLengthMismatch (1,23) 2 1) $ runWith "table(f(),{not:[],col:[\"1\"]})"       [("f",arrO [arrO [numO 0],arrO [numO 0]])]

                     assertEqual (Left $ TableHeaderLengthMismatch (1,16) 1 2) $ runWith "table(f(),g())"                        [("f",arrO [arrO [numO 0]]),              ("g",objO [("not",mkArrC (0,0) []),("col",mkArrC (1,16) [strO "1",strO "2"])])]
                     assertEqual (Left $ TableHeaderLengthMismatch (1,16) 2 1) $ runWith "table(f(),g())"                        [("f",arrO [arrO [numO 0],arrO [numO 0]]),("g",objO [("not",mkArrC (0,0) []),("col",mkArrC (1,16) [strO "1"])])]

test_TakeTableMin = do
                     assertEqual (Left $ IllegalTakeTableLength (1,6) 1 (-1)) $ runWith "take(-1,f())"   [("f",mkTableC (0,0) [[numO 0],[numO 0]] [])]
                     assertEqual (Left $ IllegalTakeTableLength (1,6) 1  0)   $ runWith "take(0,f())"    [("f",mkTableC (0,0) [[numO 0],[numO 0]] [])]

                     assertEqual (Left $ IllegalTakeTableLength (1,6) 1 (-1)) $ runWith "take(-1,f())"   [("f",mkTableC (0,0) [[numO 0,numO 0],[numO 0,numO 0]] [])]
                     assertEqual (Left $ IllegalTakeTableLength (1,6) 1  0)   $ runWith "take(0,f())"    [("f",mkTableC (0,0) [[numO 0,numO 0],[numO 0,numO 0]] [])]

                     assertEqual (Left $ IllegalTakeTableLength (1,6) 1 (-1)) $ runWith "take(f(),g())"  [("f",mkNumU (1,6) (-1)),("g",mkTableC (0,0) [[numO 0],[numO 0]] [])]
                     assertEqual (Left $ IllegalTakeTableLength (1,6) 1  0)   $ runWith "take(g(),f())"  [("g",mkNumU (1,6) 0),   ("f",mkTableC (0,0) [[numO 0],[numO 0]] [])]

                     assertEqual (Left $ IllegalTakeTableLength (1,6) 1 (-1)) $ runWith "take(f(),g())"  [("f",mkNumU (1,6) (-1)),("g",mkTableC (0,0) [[numO 0,numO 0],[numO 0,numO 0]] [])]
                     assertEqual (Left $ IllegalTakeTableLength (1,6) 1  0)   $ runWith "take(g(),f())"  [("g",mkNumU (1,6) 0),   ("f",mkTableC (0,0) [[numO 0,numO 0],[numO 0,numO 0]] [])]

test_SortIndexOutOfBounds = do
                     assertEqual (Left $ IndexOutOfBounds (1,6) 0 0 (-1)) $ runWith "sort(-1,f())" [("f",tableO [[numO 0]] [])]
                     assertEqual (Left $ IndexOutOfBounds (1,6) 0 0  1)   $ runWith "sort(1,f())"  [("f",tableO [[numO 0]] [])]

                     assertEqual (Left $ IndexOutOfBounds (1,6) 0 1 (-1)) $ runWith "sort(-1,f())" [("f",tableO [[numO 0],[numO 0]] [])]
                     assertEqual (Left $ IndexOutOfBounds (1,6) 0 1  2)   $ runWith "sort(2,f())"  [("f",tableO [[numO 0],[numO 0]] [])]

                     assertEqual (Left $ IndexOutOfBounds (1,6) 0 2 (-1)) $ runWith "sort(-1,f())" [("f",tableO [[numO 0],[numO 0],[numO 0]] [])]
                     assertEqual (Left $ IndexOutOfBounds (1,6) 0 2  3)   $ runWith "sort(3,f())"  [("f",tableO [[numO 0],[numO 0],[numO 0]] [])]

                     assertEqual (Left $ IndexOutOfBounds (1,6) 0 0 (-1)) $ run     "sort(-1,[[0]])"
                     assertEqual (Left $ IndexOutOfBounds (1,6) 0 0  1)   $ run     "sort(1,[[0]])"

                     assertEqual (Left $ IndexOutOfBounds (1,6) 0 1 (-1)) $ run     "sort(-1,[[0],[0]])"
                     assertEqual (Left $ IndexOutOfBounds (1,6) 0 1  2)   $ run     "sort(2,[[0],[0]])"

                     assertEqual (Left $ IndexOutOfBounds (1,6) 0 2 (-1)) $ run     "sort(-1,[[0],[0],[0]])"
                     assertEqual (Left $ IndexOutOfBounds (1,6) 0 2  3)   $ run     "sort(3,[[0],[0],[0]])"

test_ColIndexOutOfBounds = do
                     assertEqual (Left $ IndexOutOfBounds (1,5) 0 0 (-1)) $ runWith "col(-1,f())" [("f",tableO [[numO 0]] [])]
                     assertEqual (Left $ IndexOutOfBounds (1,5) 0 0  1)   $ runWith "col(1,f())"  [("f",tableO [[numO 0]] [])]

                     assertEqual (Left $ IndexOutOfBounds (1,5) 0 1 (-1)) $ runWith "col(-1,f())" [("f",tableO [[numO 0],[numO 0]] [])]
                     assertEqual (Left $ IndexOutOfBounds (1,5) 0 1  2)   $ runWith "col(2,f())"  [("f",tableO [[numO 0],[numO 0]] [])]

                     assertEqual (Left $ IndexOutOfBounds (1,5) 0 2 (-1)) $ runWith "col(-1,f())" [("f",tableO [[numO 0],[numO 0],[numO 0]] [])]
                     assertEqual (Left $ IndexOutOfBounds (1,5) 0 2  3)   $ runWith "col(3,f())"  [("f",tableO [[numO 0],[numO 0],[numO 0]] [])]

                     assertEqual (Left $ IndexOutOfBounds (1,5) 0 0 (-1)) $ run     "col(-1,[[0]])"
                     assertEqual (Left $ IndexOutOfBounds (1,5) 0 0  1)   $ run     "col(1,[[0]])"

                     assertEqual (Left $ IndexOutOfBounds (1,5) 0 1 (-1)) $ run     "col(-1,[[0],[0]])"
                     assertEqual (Left $ IndexOutOfBounds (1,5) 0 1  2)   $ run     "col(2,[[0],[0]])"

                     assertEqual (Left $ IndexOutOfBounds (1,5) 0 2 (-1)) $ run     "col(-1,[[0],[0],[0]])"
                     assertEqual (Left $ IndexOutOfBounds (1,5) 0 2  3)   $ run     "col(3,[[0],[0],[0]])"

