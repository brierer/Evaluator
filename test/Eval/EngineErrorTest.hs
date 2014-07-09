{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Eval.EngineErrorTest where

import qualified Eval.EngineTestUtils as E (fs)

import Control.Arrow                       ((***))
import Control.Monad.State                 (evalStateT)
import Data.EvalError                      (EvalError(..))
import Data.ExpObj                         (ExpObj(..))
import Data.ExpToken                       (ExpToken(..))
import Eval.Engine                         (funcs,sortTF,sortAF,colTF,colAF)
import Eval.EngineTestUtils                (addFunc,mk,mk',mkObj,mkObj',oneArrayOfNum,success,toArray,emptyArray,emptySortColCase,
                                            tableColumnLengthCase,tableHeaderLengthCase,mkOutOfBoundsTable,mkOutOfBoundsArray)
import Eval.Function                       (table,plot,array,str,num,arrayOf,objOf,nonEmpty,(<|>),withFuncs)
import Eval.FunctionEvalTestUtils1         (ExpOA(..),TableOA(..),NumOA(..),ExpTS(..),ArrayTS(..),applyFunc)
import Eval.FunctionEvalTestUtils2         (Is(..))
import Parser.MonolithicParserTestUtils    (P(..),ExpTA(..),StrTA(..),NumTA(..),to,un,uns)
import Test.Framework                      (TestSuite,Property,makeTestSuite,makeQuickCheckTest,makeLoc,qcAssertion,(==>))

prop_NbArgs1 (P p) esTA = length esTA > 1 ==> let es = uns esTA in
    all (\name -> Left (InvalidNbOfArgs p name 1 0)           == applyFunc E.fs p name ([] :: [ExpToken])
               && Left (InvalidNbOfArgs p name 1 (length es)) == applyFunc E.fs p name es)
      ["show","multi","mean","descriptive"]

prop_NbArgs2 (P p) esTA = length esTA > 2 ==> let es = uns esTA in
    all (\name -> Left (InvalidNbOfArgs p name 2 0)           == applyFunc E.fs p name ([] :: [ExpToken])
               && Left (InvalidNbOfArgs p name 2 1)           == applyFunc E.fs p name (take 1 es)
               && Left (InvalidNbOfArgs p name 2 (length es)) == applyFunc E.fs p name es)
      ["table","nTimes","take","sort","col"]

prop_NbArgs3 (P p) esTA = length esTA > 3 ==> let es = uns esTA in
  all (\name -> Left (InvalidNbOfArgs p name 3 0)           == applyFunc E.fs p name ([] :: [ExpToken])
             && Left (InvalidNbOfArgs p name 3 1)           == applyFunc E.fs p name (take 1 es)
             && Left (InvalidNbOfArgs p name 3 2)           == applyFunc E.fs p name (take 2 es)
             && Left (InvalidNbOfArgs p name 3 (length es)) == applyFunc E.fs p name es)
    ["plot"]

prop_TypeMismatchShow (P p) (ExpOA g1r) w1as (ExpTS w1') =
  let (fs,g1) = addFunc "tableOrPlot" g1r; (w1s,w1) = mk' w1as in (isTable g1r || isPlot g1r) && not (null w1s) && not (isArray w1') ==>
    withFuncs fs (arrayOf $ table <|> plot) w1  == applyFunc fs p "show" [w1]  &&
    withFuncs fs (arrayOf $ table <|> plot) w1' == applyFunc fs p "show" [w1'] &&
    success "show"                              == applyFunc fs p "show" [toArray p g1]

prop_TypeMismatchMulti (P p) g1ras w1as (ExpTS w1') =
  let (fs,g1,w1s,w1) = oneArrayOfNum g1ras w1as in not (null g1ras) && any (not.isNum) w1s && not (isArray w1') ==>
    withFuncs fs (arrayOf num) w1  == applyFunc fs p "multi" [w1]  &&
    withFuncs fs (arrayOf num) w1' == applyFunc fs p "multi" [w1'] &&
    success "multi"                == applyFunc fs p "multi" [g1]

prop_TypeMismatchMean (P p) g1ras w1as (ExpTS w1') =
  let (fs,g1,w1s,w1) = oneArrayOfNum g1ras w1as in not (null g1ras) && any (not.isNum) w1s && not (isArray w1') ==>
    withFuncs fs (arrayOf num) w1  == applyFunc fs p "mean" [w1]  &&
    withFuncs fs (arrayOf num) w1' == applyFunc fs p "mean" [w1'] &&
    success "mean"                 == applyFunc fs p "mean" [g1]

prop_TypeMismatchDesc (P p) g1ras w1as (ExpTS w1') =
  let (fs,g1,w1s,w1) = oneArrayOfNum g1ras w1as in not (null g1ras) && any (not.isNum) w1s && not (isArray w1') ==>
    withFuncs fs (arrayOf num) w1        == applyFunc fs p "descriptive" [w1]  &&
    withFuncs fs (arrayOf num) w1'       == applyFunc fs p "descriptive" [w1'] &&
    success "descriptive"                == applyFunc fs p "descriptive" [g1]

prop_TypeMismatchTable (P p) g1as g2as w1as (ExpTS w1') w2as (ExpTS w2') =
  let (g1s,g1) = mk' g1as; (_,g2) = mkObj g2as; (w1s,w1) = mk' w1as; (w2s,w2) = mkObj w2as in
  any (not.emptyArray) g1s && any (not.isArray) w1s && not (isArray w1') && any (not.isStr) w2s && not (isObj w2') ==>
    withFuncs E.fs (arrayOf array) w1        == applyFunc E.fs p "table" [w1 ,g2]  &&
    withFuncs E.fs (arrayOf array) w1'       == applyFunc E.fs p "table" [w1',w2]  &&
    withFuncs E.fs (objOf $ arrayOf str) w2  == applyFunc E.fs p "table" [g1 ,w2]  &&
    withFuncs E.fs (objOf $ arrayOf str) w2' == applyFunc E.fs p "table" [g1 ,w2'] &&
    success "table"                          == applyFunc E.fs p "table" [g1 ,g2]

prop_TypeMismatchNTimes (P p) (NumTA _ g1) (NumTA _ g2) (ExpTS w1) (ExpTS w2) = not (isNum w1) && not (isNum w2) ==>
  withFuncs E.fs num w1           == applyFunc E.fs p "nTimes" [w1,g2] &&
  withFuncs E.fs num w1           == applyFunc E.fs p "nTimes" [w1,w2] &&
  withFuncs E.fs num w2           == applyFunc E.fs p "nTimes" [g1,w2] &&
  success "nTimes"                == applyFunc E.fs p "nTimes" [g1,g2]

prop_TypeMismatchTake (P p) (NumTA _ g1) (ArrayTS g2) (TableOA g2r) (ExpTS w1) (ExpTS w2) = not (isNum w1) && not (isArray w2) ==>
  let (fs,g2') = addFunc "table" g2r in
  withFuncs fs  num              w1 == applyFunc fs p "take" [w1,g2]  &&
  withFuncs fs  num              w1 == applyFunc fs p "take" [w1,g2'] &&
  withFuncs fs  num              w1 == applyFunc fs p "take" [w1,w2]  &&
  withFuncs fs (table <|> array) w2 == applyFunc fs p "take" [g1,w2]  &&
  success "take"                    == applyFunc fs p "take" [g1,g2]  &&
  success "take"                    == applyFunc fs p "take" [g1,g2']

prop_TypeMismatchSort (P p) (NumTA _ g1) g2as (TableOA g2r) (ExpTS w1) w2as (ExpTS w2') =
  let (g2ss,g2) = mk' g2as; (w2s,w2) = mk' w2as; (fs,g2') = addFunc "table" g2r in not (isNum w1) && any (not.emptyArray) g2ss && any (not.isArray) w2s && not (isArray w2') ==>
    withFuncs fs  num                 w1  == applyFunc fs p "sort" [w1,g2]  &&
    withFuncs fs  num                 w1  == applyFunc fs p "sort" [w1,g2'] &&
    withFuncs fs  num                 w1  == applyFunc fs p "sort" [w1,w2]  &&
    withFuncs fs  num                 w1  == applyFunc fs p "sort" [w1,w2'] &&
    withFuncs fs (table <|> tableArg) w2  == applyFunc fs p "sort" [g1,w2]  &&
    withFuncs fs (table <|> tableArg) w2' == applyFunc fs p "sort" [g1,w2'] &&
    success "sort"                        == applyFunc fs p "sort" [g1,g2]  &&
    success "sort"                        == applyFunc fs p "sort" [g1,g2']

prop_TypeMismatchCol (P p) (NumTA _ g1) g2ass (TableOA g2r) (ExpTS w1) w2as (ExpTS w2') =
  let (g2ss,g2) = mk' g2ass; (w2s,w2) = mk' w2as; (fs,g2') = addFunc "table" g2r in not (isNum w1) && any (not.emptyArray) g2ss && any (not.isArray) w2s && not (isArray w2') ==>
    withFuncs fs  num                 w1  == applyFunc fs p "col" [w1,g2]  &&
    withFuncs fs  num                 w1  == applyFunc fs p "col" [w1,g2'] &&
    withFuncs fs  num                 w1  == applyFunc fs p "col" [w1,w2]  &&
    withFuncs fs  num                 w1  == applyFunc fs p "col" [w1,w2'] &&
    withFuncs fs (table <|> tableArg) w2  == applyFunc fs p "col" [g1,w2]  &&
    withFuncs fs (table <|> tableArg) w2' == applyFunc fs p "col" [g1,w2'] &&
    success "col"                         == applyFunc fs p "col" [g1,g2]  &&
    success "col"                         == applyFunc fs p "col" [g1,g2']

tableArg = nonEmpty $ arrayOf $ nonEmpty array

prop_TypeMismatchPlotLine (P p) g1as g2as g3as w1as (ExpTS w1') w2as (ExpTS w2') w3as (ExpTS w3') =
  let (_,g1) = mk' g1as; (_,g2) = mk' g2as; (_,g3) = mkObj' g3as; (w1s,w1) = mk' w1as; (w2s,w2) = mk' w2as; (w3s,w3) = mkObj' w3as in
  any (not.isNum) w1s && not (isArray w1') && any (not.isNum) w2s && not (isArray w2') && any (not.isStr) w3s && not (isObj w3') ==>
    all (\xs -> withFuncs E.fs (arrayOf num) w1  == applyFunc E.fs p "plot" xs) [[w1 ,x  ,y] | x <- [g2,w2,w2'], y <- [g3,w3,w3']] &&
    all (\xs -> withFuncs E.fs (arrayOf num) w1' == applyFunc E.fs p "plot" xs) [[w1',x  ,y] | x <- [g2,w2,w2'], y <- [g3,w3,w3']] &&
    all (\xs -> withFuncs E.fs (arrayOf num) w2  == applyFunc E.fs p "plot" xs) [[g1 ,w2 ,x] | x <- [g3,w3,w3']] &&
    all (\xs -> withFuncs E.fs (arrayOf num) w2' == applyFunc E.fs p "plot" xs) [[g1 ,w2',x] | x <- [g3,w3,w3']] &&
    withFuncs E.fs (objOf str) w3    == applyFunc E.fs p "plot" [g1 ,g2, w3]  &&
    withFuncs E.fs (objOf str) w3'   == applyFunc E.fs p "plot" [g1 ,g2, w3'] &&
    success "plot"               == applyFunc E.fs p "plot" [g1 ,g2 ,g3]

prop_EmptyArgMulti (P pf) (P pa) g1as = not (null g1as) ==>
  let (_,g1) = mk' g1as; (_,w1) = mk pa ([] :: [NumTA])
  in  Left (IllegalEmpty pa)                     == applyFunc E.fs pf "multi" [w1] &&
      withFuncs E.fs (nonEmpty $ arrayOf num) w1 == applyFunc E.fs pf "multi" [w1] &&
      success "multi"                            == applyFunc E.fs pf "multi" [g1]

prop_EmptyArgMean (P pf) (P pa) g1as = not (null g1as) ==>
  let (_,g1) = mk' g1as; (_,w1) = mk pa ([] :: [NumTA])
  in  Left (IllegalEmpty pa) == applyFunc E.fs pf "mean" [w1] &&
      success "mean"         == applyFunc E.fs pf "mean" [g1]

prop_EmptyArgDesc (P pf) (P pa) g1as = not (null g1as) ==>
  let (_,g1) = mk' g1as; (_,w1) = mk pa ([] :: [NumTA])
  in  Left (IllegalEmpty pa) == applyFunc E.fs pf "descriptive" [w1] &&
      success "descriptive"  == applyFunc E.fs pf "descriptive" [g1]

prop_EmptyArgTable (P pf) (P pa) g1ass g2as w1'ass =
  let (g1ss,g1) = mk' g1ass; (_,o) = mkObj g2as; (_,w1) = mk pa ([] :: [ExpTS]); (_,w1') = mk' (w1'ass ++ [to w1]); in any (not.emptyArray) g1ss ==>
    Left (IllegalEmpty pa) == applyFunc E.fs pf "table" [w1 , o] &&
    Left (IllegalEmpty pa) == applyFunc E.fs pf "table" [w1', o] &&
    success "table"        == applyFunc E.fs pf "table" [g1 , o]

prop_EmptyArgSort (P pf) (P pa) (NumTA _ n) = emptySortColCase "sort" pa pf n
prop_EmptyArgCol  (P pf) (P pa) (NumTA _ n) = emptySortColCase "col"  pa pf n

prop_TableColumnLengthMismatch   w1aps = tableColumnLengthCase        (map (un *** uns) w1aps)
prop_TableHeaderLengthMismatch p g1as  = tableHeaderLengthCase (un p) g1as                    .uns

prop_IndexOutOfBoundsSortTable (P pf) (NumTA _ a1@(NumT pn _ _ v)) (TableOA a2tr@(TableO _ cols header)) = (v < 0 || floor v > length cols) && any (not.null) cols ==>
  let (n,a2t,fs,expected) = mkOutOfBoundsTable pn v a2tr cols in
   expected == applyFunc fs    pf "sort" [a1,a2t] &&
   expected == evalStateT (sortTF pf pn n cols header) []
prop_IndexOutOfBoundsSortTable _ x y = error $ "EngineTest::prop_IndexOutOfBoundsSortTable [Unexpected pattern ["++show x++"] and ["++show y++"]]"

prop_IndexOutOfBoundsSortArray (P pf) (NumTA _ a1@(NumT pn _ _ v)) a2as = (v < 0 || floor v > length a2as) && any (not.emptyArray.un) a2as ==>
  let (n,aOfArrays,mArrays,expected) = mkOutOfBoundsArray pn v a2as in
   expected == applyFunc funcs pf "sort" [a1,aOfArrays] &&
   expected == evalStateT (sortAF pf pn n mArrays) []
prop_IndexOutOfBoundsSortArray _ x _ = error $ "EngineTest::prop_IndexOutOfBoundsSortArray [Unexpected pattern ["++show x++"]]"

prop_IndexOutOfBoundsColTable (P pf) (NumTA _ a1@(NumT pn _ _ v)) (TableOA a2tr@(TableO _ cols _)) = (v < 0 || floor v > length cols) && any (not.null) cols ==>
  let (n,a2t,fs,expected) = mkOutOfBoundsTable pn v a2tr cols in
   expected == applyFunc fs    pf "col" [a1,a2t] &&
   expected == evalStateT (colTF pf pn n cols) []
prop_IndexOutOfBoundsColTable _ x y = error $ "EngineTest::prop_IndexOutOfBoundsColTable [Unexpected pattern ["++show x++"] and ["++show y++"]]"

prop_IndexOutOfBoundsColArray (P pf) (NumTA _ a1@(NumT pn _ _ v)) a2as = (v < 0 || floor v > length a2as) && any (not.emptyArray.un) a2as ==>
  let (n,aOfArrays,mArrays,expected) = mkOutOfBoundsArray pn v a2as in
   expected == applyFunc funcs pf "col" [a1,aOfArrays] &&
   expected == evalStateT (colAF pf pn n mArrays) []
prop_IndexOutOfBoundsColArray _ x _ = error $ "EngineTest::prop_IndexOutOfBoundsColArray [Unexpected pattern ["++show x++"]]"

{-| Mandatory type signatures -}
prop_NbArgs1 :: P -> [ExpTA] ->  Property
prop_NbArgs2 :: P -> [ExpTA] ->  Property
prop_NbArgs3 :: P -> [ExpTA] ->  Property

prop_TypeMismatchShow     :: P ->  ExpOA    -> [ExpTS]            ->  ExpTS                                                                                     -> Property
prop_TypeMismatchMulti    :: P -> [NumOA]   -> [ExpTS]            ->  ExpTS                                                                                     -> Property
prop_TypeMismatchMean     :: P -> [NumOA]   -> [ExpTS]            ->  ExpTS                                                                                     -> Property
prop_TypeMismatchDesc     :: P -> [NumOA]   -> [ExpTS]            ->  ExpTS                                                                                     -> Property
prop_TypeMismatchTable    :: P -> [ArrayTS] -> [(String,[StrTA])] -> [ExpTS]           ->  ExpTS    -> [(String,[ExpTS])] ->  ExpTS                             -> Property
prop_TypeMismatchNTimes   :: P ->  NumTA    ->  NumTA             ->  ExpTS            ->  ExpTS                                                                -> Property
prop_TypeMismatchTake     :: P ->  NumTA    ->  ArrayTS           ->  TableOA          ->  ExpTS    ->  ExpTS                                                   -> Property
prop_TypeMismatchSort     :: P ->  NumTA    -> [ArrayTS]          ->  TableOA          ->  ExpTS    -> [ExpTS] ->  ExpTS                                        -> Property
prop_TypeMismatchCol      :: P ->  NumTA    -> [ArrayTS]          ->  TableOA          ->  ExpTS    -> [ExpTS] ->  ExpTS                                        -> Property
prop_TypeMismatchPlotLine :: P -> [NumTA]   -> [NumTA]            ->  [(String,StrTA)] -> [ExpTS]   ->  ExpTS  -> [ExpTS] -> ExpTS -> [(String,ExpTS)] -> ExpTS -> Property

prop_EmptyArgMulti :: P -> P -> [NumTA]                                      -> Property
prop_EmptyArgMean  :: P -> P -> [NumTA]                                      -> Property
prop_EmptyArgDesc  :: P -> P -> [NumTA]                                      -> Property
prop_EmptyArgTable :: P -> P -> [ArrayTS] -> [(String,[StrTA])]-> [ArrayTS]  -> Property
prop_EmptyArgSort  :: P -> P -> NumTA -> [ArrayTS] -> [ArrayTS]              -> Property
prop_EmptyArgCol   :: P -> P -> NumTA -> [ArrayTS] -> [ArrayTS]              -> Property

prop_TableColumnLengthMismatch :: [(P,[ExpTS])]  -> [(String,[StrTA])] -> Property
prop_TableHeaderLengthMismatch :: P -> [ArrayTS] -> [StrTA]            -> Property

prop_IndexOutOfBoundsSortTable :: P -> NumTA ->  TableOA  -> Property
prop_IndexOutOfBoundsSortArray :: P -> NumTA -> [ArrayTS] -> Property
prop_IndexOutOfBoundsColTable  :: P -> NumTA ->  TableOA  -> Property
prop_IndexOutOfBoundsColArray  :: P -> NumTA -> [ArrayTS] -> Property
