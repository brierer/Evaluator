{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Engine.EngineUnitSuccess where

import Data.ExpObj                         
import Test.Framework      

import Engine.EngineUnitSuccessUtils
import Engine.EngineUnitUtils
import Marshall.MarshallUtils

{-# ANN module "HLint: ignore Use camelCase" #-}

test_Show = do  assertEqual (Right $ ObjO (1,1) [("result",ArrO (1,6) [])])                                      $ run     "show([])"
                assertEqual (Right $ ObjO (1,1) [("result",ArrO (1,6) [TableO (1,7) [] []])])                    $ runWith "show([f()])" [("f",[],constF $ TableO (1,7) [] [])]
                assertEqual (Right $ ObjO (1,1) [("result",ArrO (1,6) [PlotO (1,11) [] []])])                    $ runWith "show([f()])" [("f",[],constF $ PlotO (1,11) [] [])]
                          
                assertEqual (Right $ ObjO (1,1) [("result",ArrO (1,6) [TableO (1,7) [] [],PlotO (1,11) [] []])]) $ runWith "show([f(),g()])" [("f",[],constF $ TableO (1,7) [] []),("g",[],constF $ PlotO (1,11) [] [])]             
                assertEqual (Right $ ObjO (1,1) [("result",ArrO (1,6) [PlotO (1,11) [] [],TableO (1,7) [] []])]) $ runWith "show([g(),f()])" [("f",[],constF $ TableO (1,7) [] []),("g",[],constF $ PlotO (1,11) [] [])]
                
                assertEqual (Right $ ObjO (1,1) [("result",ArrO (1,6) [])])                                      $ runWith "show(f())" [("f",[],constF $ ArrO (1,6) [])]
                assertEqual (Right $ ObjO (1,1) [("result",ArrO (1,6) [TableO (1,6) [] []])])                    $ runWith "show(f())" [("f",[],constF $ ArrO (1,6) [TableO (1,6) [] []])]
                assertEqual (Right $ ObjO (1,1) [("result",ArrO (1,6) [PlotO  (1,6) [] []])])                    $ runWith "show(f())" [("f",[],constF $ ArrO (1,6) [PlotO ( 1,6) [] []])]
                          
                assertEqual (Right $ ObjO (1,1) [("result",ArrO (1,6) [TableO (1,6) [] [],PlotO  (1,6) [] []])]) $ runWith "show(f())" [("f",[],constF $ ArrO (1,6) [TableO (1,6) [] [],PlotO  (1,6) [] []])]             
                assertEqual (Right $ ObjO (1,1) [("result",ArrO (1,6) [PlotO  (1,6) [] [],TableO (1,6) [] []])]) $ runWith "show(f())" [("f",[],constF $ ArrO (1,6) [PlotO  (1,6) [] [],TableO (1,6) [] []])]


test_Multi = do assertEqual (Right $ NumO (1,1) 1) $ run     "multi([1])"
                assertEqual (Right $ NumO (1,1) 2) $ run     "multi([1,2])"
                assertEqual (Right $ NumO (1,1) 6) $ run     "multi([1,2,3])"
                assertEqual (Right $ NumO (1,1) 0) $ run     "multi([1,2,3,0])"
                
                assertEqual (Right $ NumO (1,1) 1) $ runWith "multi([f()])"       [("f",[],constF $ numO 1)]
                assertEqual (Right $ NumO (1,1) 2) $ runWith "multi([f(),2])"     [("f",[],constF $ numO 1)]
                assertEqual (Right $ NumO (1,1) 6) $ runWith "multi([f(),2,3])"   [("f",[],constF $ numO 1)]
                assertEqual (Right $ NumO (1,1) 0) $ runWith "multi([f(),2,3,0])" [("f",[],constF $ numO 1)]
                
                assertEqual (Right $ NumO (1,1) 1) $ runWith "multi(f())" [("f",[],constF $ ArrO (1,8) [numO 1])]
                assertEqual (Right $ NumO (1,1) 2) $ runWith "multi(f())" [("f",[],constF $ ArrO (1,8) [numO 1,numO 2])]
                assertEqual (Right $ NumO (1,1) 6) $ runWith "multi(f())" [("f",[],constF $ ArrO (1,8) [numO 1,numO 2,numO 3])]
                assertEqual (Right $ NumO (1,1) 0) $ runWith "multi(f())" [("f",[],constF $ ArrO (1,8) [numO 1,numO 2,numO 3,numO 0])]
                
test_Mean = do  assertEqual (Right $ NumO (1,1) 1) $ run     "mean([1])"
                assertEqual (Right $ NumO (1,1) 2) $ run     "mean([1,3])"
                assertEqual (Right $ NumO (1,1) 2) $ run     "mean([1,2,3])"
                assertEqual (Right $ NumO (1,1) 0) $ run     "mean([1,2,3,-6])"
                
                assertEqual (Right $ NumO (1,1) 1) $ runWith "mean([f()])"       [("f",[],constF $ numO 1)]
                assertEqual (Right $ NumO (1,1) 2) $ runWith "mean([f(),3])"     [("f",[],constF $ numO 1)]
                assertEqual (Right $ NumO (1,1) 2) $ runWith "mean([f(),2,3])"   [("f",[],constF $ numO 1)]
                assertEqual (Right $ NumO (1,1) 0) $ runWith "mean([f(),2,3,-6])" [("f",[],constF $ numO 1)]
                
                assertEqual (Right $ NumO (1,1) 1) $ runWith "mean(f())" [("f",[],constF $ ArrO (1,8) [numO 1])]
                assertEqual (Right $ NumO (1,1) 2) $ runWith "mean(f())" [("f",[],constF $ ArrO (1,8) [numO 1,numO 3])]
                assertEqual (Right $ NumO (1,1) 2) $ runWith "mean(f())" [("f",[],constF $ ArrO (1,8) [numO 1,numO 2,numO 3])]
                assertEqual (Right $ NumO (1,1) 0) $ runWith "mean(f())" [("f",[],constF $ ArrO (1,8) [numO 1,numO 2,numO 3,numO (-6)])]
                
test_Desc = let sk = (-1.0182337649086284); ku = (-0.7696000000000001) in do
                assertEqual (show $ eval $ mkDesc (1,1) 1 1 1  0    (0/0) (0/0)) $ show $ run     "descriptive([1])"
                assertEqual (       eval $ mkDesc (1,1) 2 4 2  1     0 (-2))            $ run     "descriptive([1,3])"
                assertEqual (       eval $ mkDesc (1,1) 3 6 2 (2/3)  0 (-1.5))          $ run     "descriptive([1,2,3])"
                assertEqual (       eval $ mkDesc (1,1) 4 0 0 12.5  sk  ku)             $ run     "descriptive([1,2,3,-6])"
                                     
                assertEqual (show $ eval $ mkDesc (1,1) 1 1 1  0    (0/0) (0/0)) $ show $ runWith "descriptive([f()])"        [("f",[],constF $ numO 1)]
                assertEqual (       eval $ mkDesc (1,1) 2 4 2  1     0 (-2))            $ runWith "descriptive([f(),3])"      [("f",[],constF $ numO 1)]
                assertEqual (       eval $ mkDesc (1,1) 3 6 2 (2/3)  0 (-1.5))          $ runWith "descriptive([f(),2,3])"    [("f",[],constF $ numO 1)]
                assertEqual (       eval $ mkDesc (1,1) 4 0 0 12.5  sk  ku)             $ runWith "descriptive([f(),2,3,-6])" [("f",[],constF $ numO 1)]
                                     
                assertEqual (show $ eval $ mkDesc (1,1) 1 1 1  0    (0/0) (0/0)) $ show $ runWith "descriptive(f())" [("f",[],constF $ ArrO (1,8) [numO 1])]
                assertEqual (       eval $ mkDesc (1,1) 2 4 2  1     0 (-2))            $ runWith "descriptive(f())" [("f",[],constF $ ArrO (1,8) [numO 1,numO 3])]
                assertEqual (       eval $ mkDesc (1,1) 3 6 2 (2/3)  0 (-1.5))          $ runWith "descriptive(f())" [("f",[],constF $ ArrO (1,8) [numO 1,numO 2,numO 3])]
                assertEqual (       eval $ mkDesc (1,1) 4 0 0 12.5  sk  ku)             $ runWith "descriptive(f())" [("f",[],constF $ ArrO (1,8) [numO 1,numO 2,numO 3,numO (-6)])]

test_Table = do assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0]] [])                                                $ run     "table([[0]],{})"
                assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0]] [])                                                $ run     "table([[0]],{not:[\"no\"]})"
                assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0]] [StrO (1,19) "1"])                                 $ run     "table([[0]],{col:[\"1\"]})"
                assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0]] [StrO (1,30) "1"])                                 $ run     "table([[0]],{not:[\"no\"],col:[\"1\"]})"
                                                                                                                      
                assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0],[NumO (1,13) 1]] [])                                $ run     "table([[0],[1]],{})"
                assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0],[NumO (1,13) 1]] [])                                $ run     "table([[0],[1]],{not:[\"no\"]})"
                assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0],[NumO (1,13) 1]] [StrO (1,23) "1",StrO (1,27) "2"]) $ run     "table([[0],[1]],{col:[\"1\",\"2\"]})"
                assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0],[NumO (1,13) 1]] [StrO (1,34) "1",StrO (1,38) "2"]) $ run     "table([[0],[1]],{not:[\"no\"],col:[\"1\",\"2\"]})"
                
                assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0]] [])                                                $ runWith "table(f(),{})"                               [("f",[],constF $ ArrO (0,0) [ArrO (0,0) [NumO (1,9) 0]])]
                assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0]] [])                                                $ runWith "table(f(),{not:[\"no\"]})"                   [("f",[],constF $ ArrO (0,0) [ArrO (0,0) [NumO (1,9) 0]])]
                assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0]] [StrO (1,17) "1"])                                 $ runWith "table(f(),{col:[\"1\"]})"                    [("f",[],constF $ ArrO (0,0) [ArrO (0,0) [NumO (1,9) 0]])]
                assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0]] [StrO (1,28) "1"])                                 $ runWith "table(f(),{not:[\"no\"],col:[\"1\"]})"       [("f",[],constF $ ArrO (0,0) [ArrO (0,0) [NumO (1,9) 0]])]

                assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0],[NumO (1,13) 1]] [])                                $ runWith "table(f(),{})"                               [("f",[],constF $ ArrO (0,0) [ArrO (0,0) [NumO (1,9) 0],ArrO (0,0) [NumO (1,13) 1]])]
                assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0],[NumO (1,13) 1]] [])                                $ runWith "table(f(),{not:[\"no\"]})"                   [("f",[],constF $ ArrO (0,0) [ArrO (0,0) [NumO (1,9) 0],ArrO (0,0) [NumO (1,13) 1]])]
                assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0],[NumO (1,13) 1]] [StrO (1,17) "1",StrO (1,21) "2"]) $ runWith "table(f(),{col:[\"1\",\"2\"]})"              [("f",[],constF $ ArrO (0,0) [ArrO (0,0) [NumO (1,9) 0],ArrO (0,0) [NumO (1,13) 1]])]
                assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0],[NumO (1,13) 1]] [StrO (1,28) "1",StrO (1,32) "2"]) $ runWith "table(f(),{not:[\"no\"],col:[\"1\",\"2\"]})" [("f",[],constF $ ArrO (0,0) [ArrO (0,0) [NumO (1,9) 0],ArrO (0,0) [NumO (1,13) 1]])]
                
                assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0]] [])                                                $ runWith "table([[0]],f())"     [("f",[],constF $ ObjO (0,0) [])]
                assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0]] [])                                                $ runWith "table([[0]],f())"     [("f",[],constF $ ObjO (0,0) [("not",ArrO (0,0) [StrO (0,0) "no"])])]
                assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0]] [StrO (1,19) "1"])                                 $ runWith "table([[0]],f())"     [("f",[],constF $ ObjO (0,0) [("col",ArrO (0,0) [StrO (1,19) "1"])])]
                assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0]] [StrO (1,30) "1"])                                 $ runWith "table([[0]],f())"     [("f",[],constF $ ObjO (0,0) [("not",ArrO (0,0) [StrO (0,0) "no"]),("col",ArrO (0,0) [StrO (1,30) "1"])])]

                assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0],[NumO (1,13) 1]] [])                                $ runWith "table([[0],[1]],f())" [("f",[],constF $ ObjO (0,0) [])]
                assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0],[NumO (1,13) 1]] [])                                $ runWith "table([[0],[1]],f())" [("f",[],constF $ ObjO (0,0) [("not",ArrO (0,0) [StrO (0,0) "no"])])]
                assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0],[NumO (1,13) 1]] [StrO (1,23) "1",StrO (1,27) "2"]) $ runWith "table([[0],[1]],f())" [("f",[],constF $ ObjO (0,0) [("col",ArrO (0,0) [StrO (1,23) "1",StrO (1,27) "2"])])]
                assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0],[NumO (1,13) 1]] [StrO (1,34) "1",StrO (1,38) "2"]) $ runWith "table([[0],[1]],f())" [("f",[],constF $ ObjO (0,0) [("not",ArrO (0,0) [StrO (0,0) "no"]),("col",ArrO (0,0) [StrO (1,34) "1",StrO (1,38) "2"])])]
                
                assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0]] [])                                                $ runWith "table(f(),g())" [("f",[],constF $ ArrO (0,0) [ArrO (0,0) [NumO (1,9) 0]]),                             ("g",[],constF $ ObjO (0,0) [])]                                                                                         
                assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0]] [])                                                $ runWith "table(g(),f())" [("g",[],constF $ ArrO (0,0) [ArrO (0,0) [NumO (1,9) 0]]),                             ("f",[],constF $ ObjO (0,0) [("not",ArrO (0,0) [StrO (0,0) "no"])])]                                                     
                assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0]] [StrO (1,17) "1"])                                 $ runWith "table(f(),g())" [("f",[],constF $ ArrO (0,0) [ArrO (0,0) [NumO (1,9) 0]]),                             ("g",[],constF $ ObjO (0,0) [("col",ArrO (0,0) [StrO (1,17) "1"])])]                                                     
                assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0]] [StrO (1,28) "1"])                                 $ runWith "table(g(),f())" [("g",[],constF $ ArrO (0,0) [ArrO (0,0) [NumO (1,9) 0]]),                             ("f",[],constF $ ObjO (0,0) [("not",ArrO (0,0) [StrO (0,0) "no"]),("col",ArrO (0,0) [StrO (1,28) "1"])])]                
                                                                                                                                                                                                                                                                                                                                                                                              
                assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0],[NumO (1,13) 1]] [])                                $ runWith "table(f(),g())" [("f",[],constF $ ArrO (0,0) [ArrO (0,0) [NumO (1,9) 0],ArrO (0,0) [NumO (1,13) 1]]),  ("g",[],constF $ ObjO (0,0) [])]                                                                                         
                assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0],[NumO (1,13) 1]] [])                                $ runWith "table(g(),f())" [("g",[],constF $ ArrO (0,0) [ArrO (0,0) [NumO (1,9) 0],ArrO (0,0) [NumO (1,13) 1]]),  ("f",[],constF $ ObjO (0,0) [("not",ArrO (0,0) [StrO (0,0) "no"])])]                                                     
                assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0],[NumO (1,13) 1]] [StrO (1,17) "1",StrO (1,21) "2"]) $ runWith "table(f(),g())" [("f",[],constF $ ArrO (0,0) [ArrO (0,0) [NumO (1,9) 0],ArrO (0,0) [NumO (1,13) 1]]),  ("g",[],constF $ ObjO (0,0) [("col",ArrO (0,0) [StrO (1,17) "1",StrO (1,21) "2"])])]                                     
                assertEqual (Right $ TableO (1,1) [[NumO (1,9) 0],[NumO (1,13) 1]] [StrO (1,28) "1",StrO (1,32) "2"]) $ runWith "table(g(),f())" [("g",[],constF $ ArrO (0,0) [ArrO (0,0) [NumO (1,9) 0],ArrO (0,0) [NumO (1,13) 1]]),  ("f",[],constF $ ObjO (0,0) [("not",ArrO (0,0) [StrO (0,0) "no"]),("col",ArrO (0,0) [StrO (1,28) "1",StrO (1,32) "2"])])]
                
--
--prop_Table (P pf) (TableValidArgs g1ss g2s) useHeader = any (not.null) g1ss ==>
--  let (g1@(ArrayT _ _ es),g2@(ObjT _ _ ps),expected) = mkTableValidArgs pf g1ss g2s useHeader in
--   expected == applyFunc funcs pf "table" [g1, g2] &&
--   expected == evalStateT (tableF pf (map unsafeMarshall es) (map unsafeMarshallP ps)) []
--
--prop_NTimes (P pf) a1@(NumTA _ (NumT p1 _ _ v1)) a2@(NumTA _ (NumT _ _ _ v2)) =
--  let expected = Right $ ArrayO pf $ replicate (floor v2) (NumO p1 v1) in
--      expected == applyFunc funcs pf "nTimes" [un a1, un a2] &&
--      expected == evalStateT (nTimesF pf (NumO p1 v1) v2) []
--prop_NTimes _ x y = error $ "EngineTest::prop_NTimes [Unexpected pattern ["++show x++"] and ["++show y++"]]"
--
--prop_TakeTable (P pf) (NumTA _ a1@(NumT _ _ _ v)) (TableOA a2tr@(TableO _ cols header)) = any (not.null) cols ==>
--  let (fs,a2t) = addFunc' "mkTable" a2tr; n = floor v; expected = Right $ TableO pf (map (take n) cols) header in
--   expected == applyFunc fs  pf "take" [a1,a2t] &&
--   expected == evalStateT (takeTF pf n cols header) []
--prop_TakeTable _ x y = error $ "EngineTest::prop_Take [Unexpected pattern ["++show x++"] and ["++show y++"]]"
--
--prop_TakeArray (P pf) (NumTA _ a1@(NumT _ _ _ v)) a2as = not (null a2as) ==>
--  let n = floor v; (a2s,a2a) = mk' a2as; a2ar = map unsafeMarshall a2s; expected = Right $ ArrayO pf $ take n a2ar in
--   expected == applyFunc funcs pf "take" [a1,a2a] &&
--   expected == evalStateT (takeAF pf n a2ar) []
--prop_TakeArray _ x y = error $ "EngineTest::prop_Take [Unexpected pattern ["++show x++"] and ["++show y++"]]"
--
--prop_SortTable (P pf) (NumTA _ a1') (TableOA a2tr@(TableO _ cols header)) = any (not.null) cols ==>
--  let (a1,n) = keepInRange a1' (length cols); (fs,a2t) = addFunc' "mkTable" a2tr; expected = Right $ TableO pf (sortTOn n cols) header in
--   expected == applyFunc fs    pf "sort" [a1,a2t] &&
--   expected == evalStateT (sortTF pf p0 n cols header) []
--prop_SortTable _ x y = error $ "EngineTest::prop_Take [Unexpected pattern ["++show x++"] and ["++show y++"]]"
--
--prop_SortArray (P pf) (NumTA _ a1') (TableValidArgs g2ss _) = any (not.null) g2ss ==>
--  let (n, a1,aOfArrays,arrays,mArrays) = mkSortColArray a1' g2ss; expected = Right $ ArrayO pf (sortAOn n $ map unsafeMarshall arrays)in
--   expected == applyFunc funcs pf "sort" [a1,aOfArrays] &&
--   expected == evalStateT (sortAF pf p0 n mArrays) []
--
--prop_ColTable (P pf) (NumTA _ a1') (TableOA a2tr@(TableO _ cols _)) = any (not.null) cols ==>
--  let (a1,n) = keepInRange a1' (length cols); (fs,a2t) = addFunc' "mkTable" a2tr; expected = Right $ ArrayO pf (cols !! n) in
--   expected == applyFunc fs    pf "col" [a1,a2t] &&
--   expected == evalStateT (colTF pf p0 n cols) []
--prop_ColTable _ x y = error $ "EngineTest::prop_Take [Unexpected pattern ["++show x++"] and ["++show y++"]]"
--
--prop_ColArray (P pf) (NumTA _ a1') (TableValidArgs g2ss _) = any (not.null) g2ss ==>
--  let (n, a1,aOfArrays,arrays,mArrays) = mkSortColArray a1' g2ss; expected = Right $ let ArrayO _ es = unsafeMarshall $ arrays !! n in ArrayO pf es in
--   expected == applyFunc funcs pf "col" [a1,aOfArrays] &&
--   expected == evalStateT (colAF pf p0 n mArrays) []
--
--{-| Mandatory type signatures -}
--prop_Show      :: P      -> [ExpOA]            -> Property
--prop_Multi     :: P -> P -> [AtomTA]           -> Property
--prop_Mean      :: P -> P -> [AtomTA]           -> Property
--prop_Desc      :: P -> P -> [AtomTA]           -> Property
--prop_Table     :: P -> TableValidArgs -> Bool  -> Property
--prop_TakeTable :: P -> NumTA -> TableOA        -> Property
--prop_TakeArray :: P -> NumTA -> [ExpTS]        -> Property
--prop_SortTable :: P -> NumTA ->  TableOA       -> Property
--prop_SortArray :: P -> NumTA -> TableValidArgs -> Property
--prop_ColTable :: P -> NumTA ->  TableOA        -> Property
--prop_ColArray :: P -> NumTA -> TableValidArgs  -> Property
