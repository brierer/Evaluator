{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Eval.Engine.EngineErrorTest where

import Prelude as P

import Control.Arrow                       
import Control.Monad.State                 
import Data.EvalError                      
import Data.ExpObj                         
import Data.ExpToken                       
import Eval.Engine                         
import Eval.Engine.EngineTestUtils         
import Eval.Function                       
import Eval.Function.FunctionEvalTestUtils1
import Eval.Function.FunctionEvalTestUtils2
import Parser.MonolithicParserTestUtils    
import Test.Framework                      

prop_NbArgs1 (P p) esTA = length esTA > 1 ==> let es = map un esTA in
    all (\name -> Left (InvalidNbOfArgs p name 1 0)           == applyFunc funcMocks p name ([] :: [ExpToken])
               && Left (InvalidNbOfArgs p name 1 (length es)) == applyFunc funcMocks p name es)
      ["show","multi","mean","descriptive"]

prop_NbArgs2 (P p) esTA = length esTA > 2 ==> let es = map un esTA in
    all (\name -> Left (InvalidNbOfArgs p name 2 0)           == applyFunc funcMocks p name ([] :: [ExpToken])
               && Left (InvalidNbOfArgs p name 2 1)           == applyFunc funcMocks p name (take 1 es)
               && Left (InvalidNbOfArgs p name 2 (length es)) == applyFunc funcMocks p name es)
      ["table","nTimes","take","sort","col"]

prop_NbArgs3 (P p) esTA = length esTA > 3 ==> let es = map un esTA in
  all (\name -> Left (InvalidNbOfArgs p name 3 0)           == applyFunc funcMocks p name ([] :: [ExpToken])
             && Left (InvalidNbOfArgs p name 3 1)           == applyFunc funcMocks p name (take 1 es)
             && Left (InvalidNbOfArgs p name 3 2)           == applyFunc funcMocks p name (take 2 es)
             && Left (InvalidNbOfArgs p name 3 (length es)) == applyFunc funcMocks p name es)
    ["plot"]

prop_TypeMismatchShow (P p) (ExpOA g1r) w1as (ExpTS w1') =
  let (fs,g1) = addFunc "tableOrPlot" g1r; (w1s,w1) = mk' w1as in (isTable g1r || isPlot g1r) && not (P.null w1s) && not (isArray w1') ==>
    withFuncs fs (arrayOf $ table <|> plot) w1  == applyFunc fs p "show" [w1]  &&
    withFuncs fs (arrayOf $ table <|> plot) w1' == applyFunc fs p "show" [w1'] &&
    success "show"                              == applyFunc fs p "show" [toArray p g1]

prop_TypeMismatchMulti (P p) g1ras w1as (ExpTS w1') =
  let (fs,g1,w1s,w1) = oneArrayOf g1ras w1as in not (P.null g1ras) && P.any (not.isAtom) w1s && not (isArray w1') ==>
    withFuncs fs (arrayOf atom) w1  == applyFunc fs p "multi" [w1]  &&
    withFuncs fs (arrayOf atom) w1' == applyFunc fs p "multi" [w1'] &&
    success "multi"                 == applyFunc fs p "multi" [g1]

prop_TypeMismatchMean (P p) g1ras w1as (ExpTS w1') =
  let (fs,g1,w1s,w1) = oneArrayOf g1ras w1as in not (P.null g1ras) && P.any (not.isAtom) w1s && not (isArray w1') ==>
    withFuncs fs (arrayOf atom) w1  == applyFunc fs p "mean" [w1]  &&
    withFuncs fs (arrayOf atom) w1' == applyFunc fs p "mean" [w1'] &&
    success "mean"                  == applyFunc fs p "mean" [g1]

prop_TypeMismatchDesc (P p) g1ras w1as (ExpTS w1') =
  let (fs,g1,w1s,w1) = oneArrayOf g1ras w1as in not (P.null g1ras) && P.any (not.isAtom) w1s && not (isArray w1') ==>
    withFuncs fs (arrayOf atom) w1        == applyFunc fs p "descriptive" [w1]  &&
    withFuncs fs (arrayOf atom) w1'       == applyFunc fs p "descriptive" [w1'] &&
    success "descriptive"                == applyFunc fs p "descriptive" [g1]

prop_TypeMismatchTable (P p) (TableValidArgs g1ss g2s) w1as (ExpTS w1') w2as (ExpTS w2') =
  let g1 = mkArr p0 $ map (mkArr p0) g1ss
      g2 = mkObj p0 [mkPair p0 "col" $ mkArr p0 g2s]
      (w1s,w1) = mk' w1as; (w2s,w2) = mkObjFrom w2as in
    P.any (not.P.null) g1ss && not (P.null g2s) && P.any (not.isArray) w1s && not (isArray w1') && P.any (not.isStr) w2s && not (isObj w2') ==>
    withFuncs funcMocks (arrayOf $ arrayOf atom) w1  == applyFunc funcMocks p "table" [w1 ,g2]  &&
    withFuncs funcMocks (arrayOf $ arrayOf atom) w1' == applyFunc funcMocks p "table" [w1',w2]  &&
    withFuncs funcMocks (objOf $ arrayOf str) w2     == applyFunc funcMocks p "table" [g1 ,w2]  &&
    withFuncs funcMocks (objOf $ arrayOf str) w2'    == applyFunc funcMocks p "table" [g1 ,w2'] &&
    success "table"                                  == applyFunc funcMocks p "table" [g1 ,g2]

prop_TypeMismatchNTimes (P p) (NumTA _ g1) (NumTA _ g2) (ExpTS w1) (ExpTS w2) = not (isNum w1) && not (isNum w2) ==>
  withFuncs funcMocks num w1           == applyFunc funcMocks p "nTimes" [w1,g2] &&
  withFuncs funcMocks num w1           == applyFunc funcMocks p "nTimes" [w1,w2] &&
  withFuncs funcMocks num w2           == applyFunc funcMocks p "nTimes" [g1,w2] &&
  success "nTimes"                     == applyFunc funcMocks p "nTimes" [g1,g2]

prop_TypeMismatchTake (P p) (NumTA _ g1) (ArrTS g2) (TableOA g2r) (ExpTS w1) (ExpTS w2) = not (isNum w1) && not (isArray w2) ==>
  let (fs,g2') = addFunc "table" g2r in
  withFuncs fs  num              w1 == applyFunc fs p "take" [w1,g2]  &&
  withFuncs fs  num              w1 == applyFunc fs p "take" [w1,g2'] &&
  withFuncs fs  num              w1 == applyFunc fs p "take" [w1,w2]  &&
  withFuncs fs (table <|> array) w2 == applyFunc fs p "take" [g1,w2]  &&
  success "take"                    == applyFunc fs p "take" [g1,g2]  &&
  success "take"                    == applyFunc fs p "take" [g1,g2']

prop_TypeMismatchSort = typeMismatchSortColCase "sort"
prop_TypeMismatchCol  = typeMismatchSortColCase "col"
    
prop_TypeMismatchPlotLine (P p) g1as g2as g3as w1as (ExpTS w1') w2as (ExpTS w2') w3as (ExpTS w3') =
  let (_,g1) = mk' g1as; (_,g2) = mk' g2as; (_,g3) = mkObjFrom' g3as; (w1s,w1) = mk' w1as; (w2s,w2) = mk' w2as; (w3s,w3) = mkObjFrom' w3as in
  P.any (not.isNum) w1s && not (isArray w1') && P.any (not.isNum) w2s && not (isArray w2') && P.any (not.isStr) w3s && not (isObj w3') ==>
    all (\xs -> withFuncs funcMocks (arrayOf num) w1  == applyFunc funcMocks p "plot" xs) [[w1 ,x  ,y] | x <- [g2,w2,w2'], y <- [g3,w3,w3']] &&
    all (\xs -> withFuncs funcMocks (arrayOf num) w1' == applyFunc funcMocks p "plot" xs) [[w1',x  ,y] | x <- [g2,w2,w2'], y <- [g3,w3,w3']] &&
    all (\xs -> withFuncs funcMocks (arrayOf num) w2  == applyFunc funcMocks p "plot" xs) [[g1 ,w2 ,x] | x <- [g3,w3,w3']] &&
    all (\xs -> withFuncs funcMocks (arrayOf num) w2' == applyFunc funcMocks p "plot" xs) [[g1 ,w2',x] | x <- [g3,w3,w3']] &&
    withFuncs funcMocks (objOf str) w3                == applyFunc funcMocks p "plot" [g1 ,g2, w3]  &&
    withFuncs funcMocks (objOf str) w3'               == applyFunc funcMocks p "plot" [g1 ,g2, w3'] &&
    success "plot"                                    == applyFunc funcMocks p "plot" [g1 ,g2 ,g3]

prop_EmptyArgMulti (P pf) (P pa) g1as = not (P.null g1as) ==>
  let (_,g1) = mk' g1as; (_,w1) = mk pa ([] :: [NumTA])
  in  Left (IllegalEmpty pa)                          == applyFunc funcMocks pf "multi" [w1] &&
      withFuncs funcMocks (nonEmpty $ arrayOf num) w1 == applyFunc funcMocks pf "multi" [w1] &&
      success "multi"                                 == applyFunc funcMocks pf "multi" [g1]

prop_EmptyArgMean (P pf) (P pa) g1as = not (P.null g1as) ==>
  let (_,g1) = mk' g1as; (_,w1) = mk pa ([] :: [NumTA])
  in  Left (IllegalEmpty pa) == applyFunc funcMocks pf "mean" [w1] &&
      success "mean"         == applyFunc funcMocks pf "mean" [g1]

prop_EmptyArgDesc (P pf) (P pa) g1as = not (P.null g1as) ==>
  let (_,g1) = mk' g1as; (_,w1) = mk pa ([] :: [NumTA])
  in  Left (IllegalEmpty pa) == applyFunc funcMocks pf "descriptive" [w1] &&
      success "descriptive"  == applyFunc funcMocks pf "descriptive" [g1]

prop_EmptyArgTable (P pf) (P pa) (TableValidArgs g1ss _) g2as  =
  let g1  = mkArr p0 $ map (mkArr p0) g1ss
      w1' = mkArr p0 $ map (mkArr p0) g1ss ++ [w1]
      (_,o) = mkObjFrom g2as; (_,w1) = mk pa ([] :: [ExpTS]) in P.any (not.P.null) g1ss ==>
    Left (IllegalEmpty pa) == applyFunc funcMocks pf "table" [w1 , o] &&
    Left (IllegalEmpty pa) == applyFunc funcMocks pf "table" [w1', o] &&
    success "table"        == applyFunc funcMocks pf "table" [g1 , o]

prop_EmptyArgSort (P pf) (P pa) (NumTA _ n) = emptySortColCase "sort" pa pf n
prop_EmptyArgCol  (P pf) (P pa) (NumTA _ n) = emptySortColCase "col"  pa pf n

prop_TableColumnLengthMismatch w1aps = tableColumnLengthCase $ map (un *** map un) w1aps
prop_TableHeaderLengthMismatch = tableHeaderLengthCase.un

prop_IndexOutOfBoundsSortTable (P pf) (NumTA _ a1@(NumT pn _ _ v)) (TableOA a2tr@(TableO _ cols header)) = (v < 0 || floor v > length cols) && P.any (not.P.null) cols ==>
  let (n,a2t,fs,expected) = mkOutOfBoundsTable pn v a2tr cols in
   expected == applyFunc fs    pf "sort" [a1,a2t] &&
   expected == evalStateT (sortTF pf pn n cols header) []
prop_IndexOutOfBoundsSortTable _ x y = error $ "EngineTest::prop_IndexOutOfBoundsSortTable [Unexpected pattern ["++show x++"] and ["++show y++"]]"

prop_IndexOutOfBoundsSortArray (P pf) (NumTA _ a1@(NumT pn _ _ v)) (TableValidArgs g2ss _) = (v < 0 || floor v > length g2ss) && P.any (not.P.null) g2ss ==>
  let (n,aOfArrays,mArrays,expected) = mkOutOfBoundsArray pn v g2ss in
   expected == applyFunc funcs pf "sort" [a1,aOfArrays] &&
   expected == evalStateT (sortAF pf pn n mArrays) []
prop_IndexOutOfBoundsSortArray _ x _ = error $ "EngineTest::prop_IndexOutOfBoundsSortArray [Unexpected pattern ["++show x++"]]"

prop_IndexOutOfBoundsColTable (P pf) (NumTA _ a1@(NumT pn _ _ v)) (TableOA a2tr@(TableO _ cols _)) = (v < 0 || floor v > length cols) && P.any (not.P.null) cols ==>
  let (n,a2t,fs,expected) = mkOutOfBoundsTable pn v a2tr cols in
   expected == applyFunc fs    pf "col" [a1,a2t] &&
   expected == evalStateT (colTF pf pn n cols) []
prop_IndexOutOfBoundsColTable _ x y = error $ "EngineTest::prop_IndexOutOfBoundsColTable [Unexpected pattern ["++show x++"] and ["++show y++"]]"

prop_IndexOutOfBoundsColArray (P pf) (NumTA _ a1@(NumT pn _ _ v)) (TableValidArgs g2ss _)  = (v < 0 || floor v > length g2ss) && P.any (not.P.null) g2ss ==>
  let (n,aOfArrays,mArrays,expected) = mkOutOfBoundsArray pn v g2ss in
   expected == applyFunc funcs pf "col" [a1,aOfArrays] &&
   expected == evalStateT (colAF pf pn n mArrays) []
prop_IndexOutOfBoundsColArray _ x _ = error $ "EngineTest::prop_IndexOutOfBoundsColArray [Unexpected pattern ["++show x++"]]"

{-| Mandatory type signatures -}
prop_NbArgs1 :: P -> [ExpTA] ->  Property
prop_NbArgs2 :: P -> [ExpTA] ->  Property
prop_NbArgs3 :: P -> [ExpTA] ->  Property

prop_TypeMismatchShow     :: P ->   ExpOA        -> [ExpTS]            ->  ExpTS                                                                                       -> Property
prop_TypeMismatchMulti    :: P ->  [AtomTA]      -> [ExpTS]            ->  ExpTS                                                                                       -> Property
prop_TypeMismatchMean     :: P ->  [AtomTA]      -> [ExpTS]            ->  ExpTS                                                                                       -> Property
prop_TypeMismatchDesc     :: P ->  [AtomTA]      -> [ExpTS]            ->  ExpTS                                                                                       -> Property
prop_TypeMismatchTable    :: P -> TableValidArgs -> [ExpTS]            ->  ExpTS            -> [(String,[ExpTS])] ->  ExpTS                                            -> Property
prop_TypeMismatchNTimes   :: P ->   NumTA        ->  NumTA             ->  ExpTS            ->  ExpTS                                                                  -> Property
prop_TypeMismatchTake     :: P ->   NumTA        ->  ArrTS             ->  TableOA          ->  ExpTS    ->  ExpTS                                                     -> Property
prop_TypeMismatchSort     :: P ->   NumTA        -> TableValidArgs     ->  TableOA          ->  ExpTS    -> [[ExpTS]] -> [ExpTS] ->  ExpTS                             -> Property
prop_TypeMismatchCol      :: P ->   NumTA        -> TableValidArgs     ->  TableOA          ->  ExpTS    -> [[ExpTS]] -> [ExpTS] ->  ExpTS                             -> Property
prop_TypeMismatchPlotLine :: P ->  [NumTA]       -> [NumTA]            ->  [(String,StrTA)] -> [ExpTS]   ->   ExpTS   -> [ExpTS] -> ExpTS -> [(String,ExpTS)] -> ExpTS -> Property

prop_EmptyArgMulti :: P -> P -> [NumTA]                              -> Property
prop_EmptyArgMean  :: P -> P -> [NumTA]                              -> Property
prop_EmptyArgDesc  :: P -> P -> [NumTA]                              -> Property
prop_EmptyArgTable :: P -> P -> TableValidArgs -> [(String,[StrTA])] -> Property
prop_EmptyArgSort  :: P -> P -> NumTA -> TableValidArgs              -> Property
prop_EmptyArgCol   :: P -> P -> NumTA -> TableValidArgs              -> Property

prop_TableColumnLengthMismatch :: [(P,[AtomTA])]  -> [(String,[StrTA])] -> Property
prop_TableHeaderLengthMismatch :: P -> TableValidArgs -> TableValidArgs -> Property

prop_IndexOutOfBoundsSortTable :: P -> NumTA ->  TableOA       -> Property
prop_IndexOutOfBoundsSortArray :: P -> NumTA -> TableValidArgs -> Property
prop_IndexOutOfBoundsColTable  :: P -> NumTA ->  TableOA       -> Property
prop_IndexOutOfBoundsColArray  :: P -> NumTA -> TableValidArgs -> Property

