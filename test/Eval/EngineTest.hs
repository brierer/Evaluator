{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Eval.EngineTest where

import qualified Eval.EngineTestUtils as E (fs)

import Control.Monad.State                 (evalStateT)
import Data.Eval                           (EvalError(..),ExpObj(..))
import Data.List                           (genericLength)
import Data.Token                          (ExpToken(..))
import Eval.Engine                         (funcs,showF,multiF,meanF)
import Eval.EngineTestUtils                (addFunc,addFunc',mk,mkO,oneArrayOfNum,success,toArray,tablesAndPlots,ws2)
import Eval.Function                       (arrayOf,table,plot,array,obj,num,(<|>),withFuncs)
import Eval.FunctionEvalTestUtils          (Is(..),ExpOA(..),TableOA(..),NumOA(..),ExpTS(..),ArrayTS(..),ObjTS(..),applyFunc)
import Parser.MonolithicParserTestUtils    (P(..),ExpTA(..),NumTA(..),uns,NumType(..))
import Test.Framework                      (TestSuite,NonEmptyList(..),Property,makeTestSuite,makeQuickCheckTest,makeLoc,qcAssertion,(==>),makeUnitTest,assertEqual_,makeLoc)

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
    ["plotLine"]

prop_TypeMismatchShow (P p) (ExpOA g1r) w1as (ExpTS w1') =
  let (fs,g1) = addFunc "tableOrPlot" g1r; (w1s,w1) = mk w1as in (isTable g1r || isPlot g1r) && not (null w1s) && not (isArray w1') ==>
    withFuncs fs (arrayOf $ table <|> plot) w1  == applyFunc fs p "show" [w1]  &&
    withFuncs fs (arrayOf $ table <|> plot) w1' == applyFunc fs p "show" [w1'] &&
    success "show"                              == applyFunc fs p "show" [toArray p g1]

prop_TypeMismatchMulti (P p) g1ras w1as (ExpTS w1') =
  let (fs,g1,w1s,w1) = oneArrayOfNum g1ras w1as in any (not.isNum) w1s && not (isArray w1') ==>
    withFuncs fs (arrayOf num) w1  == applyFunc fs p "multi" [w1]  &&
    withFuncs fs (arrayOf num) w1' == applyFunc fs p "multi" [w1'] &&
    success "multi"                == applyFunc fs p "multi" [g1]

prop_TypeMismatchMean (P p) g1ras w1as (ExpTS w1') =
  let (fs,g1,w1s,w1) = oneArrayOfNum g1ras w1as in any (not.isNum) w1s && not (isArray w1') ==>
    withFuncs fs (arrayOf num) w1  == applyFunc fs p "mean" [w1]  &&
    withFuncs fs (arrayOf num) w1' == applyFunc fs p "mean" [w1'] &&
    success "mean"                 == applyFunc fs p "mean" [g1]

prop_TypeMismatchDesc (P p) g1ras w1as (ExpTS w1') =
  let (fs,g1,w1s,w1) = oneArrayOfNum g1ras w1as in any (not.isNum) w1s && not (isArray w1') ==>
    withFuncs fs (arrayOf num) w1        == applyFunc fs p "descriptive" [w1]  &&
    withFuncs fs (arrayOf num) w1'       == applyFunc fs p "descriptive" [w1'] &&
    success "descriptive"                == applyFunc fs p "descriptive" [g1]

prop_TypeMismatchTable (P p) g1ass (ObjTS g2) w1ass (ExpTS w1') (ExpTS w2) =
  let (_,g1) = mk g1ass; (w1ss,w1) = mk w1ass; in any (not.isArray) w1ss && not (isArray w1') && not (isObj w2) ==>
    withFuncs E.fs (arrayOf array) w1  == applyFunc E.fs p "table" [w1 ,g2] &&
    withFuncs E.fs (arrayOf array) w1' == applyFunc E.fs p "table" [w1',w2] &&
    withFuncs E.fs  obj            w2  == applyFunc E.fs p "table" [g1 ,w2] &&
    success "table"                    == applyFunc E.fs p "table" [g1 ,g2]

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
  let (_,g2) = mk g2as; (w2s,w2) = mk w2as; (fs,g2') = addFunc "table" g2r in not (isNum w1) && any (not.isArray) w2s && not (isArray w2') ==>
    withFuncs fs  num                      w1  == applyFunc fs p "sort" [w1,g2]  &&
    withFuncs fs  num                      w1  == applyFunc fs p "sort" [w1,g2'] &&
    withFuncs fs  num                      w1  == applyFunc fs p "sort" [w1,w2]  &&
    withFuncs fs  num                      w1  == applyFunc fs p "sort" [w1,w2'] &&
    withFuncs fs (table <|> arrayOf array) w2  == applyFunc fs p "sort" [g1,w2]  &&
    withFuncs fs (table <|> arrayOf array) w2' == applyFunc fs p "sort" [g1,w2'] &&
    success "sort"                             == applyFunc fs p "sort" [g1,g2]  &&
    success "sort"                             == applyFunc fs p "sort" [g1,g2']

prop_TypeMismatchCol (P p) (NumTA _ g1) g2ass (TableOA g2r) (ExpTS w1) w2as (ExpTS w2') =
  let (_,g2) = mk g2ass; (w2s,w2) = mk w2as; (fs,g2') = addFunc "table" g2r in not (isNum w1) && any (not.isArray) w2s && not (isArray w2') ==>
    withFuncs fs  num                      w1  == applyFunc fs p "col" [w1,g2]  &&
    withFuncs fs  num                      w1  == applyFunc fs p "col" [w1,g2'] &&
    withFuncs fs  num                      w1  == applyFunc fs p "col" [w1,w2]  &&
    withFuncs fs  num                      w1  == applyFunc fs p "col" [w1,w2'] &&
    withFuncs fs (table <|> arrayOf array) w2  == applyFunc fs p "col" [g1,w2]  &&
    withFuncs fs (table <|> arrayOf array) w2' == applyFunc fs p "col" [g1,w2'] &&
    success "col"                              == applyFunc fs p "col" [g1,g2]  &&
    success "col"                              == applyFunc fs p "col" [g1,g2']

prop_TypeMismatchPlotLine (P p) g1as g2as (ObjTS g3) w1as (ExpTS w1') w2as (ExpTS w2') (ExpTS w3) =
  let (_,g1) = mk g1as; (_,g2) = mk g2as; (w1s,w1) = mk w1as; (w2s,w2) = mk w2as
  in any (not.isNum) w1s && not (isArray w1') && any (not.isNum) w2s && not (isArray w2') && not (isObj w3) ==>
    withFuncs E.fs (arrayOf num) w1     == applyFunc E.fs p "plotLine" [w1 ,g2 ,g3] &&
    withFuncs E.fs (arrayOf num) w1     == applyFunc E.fs p "plotLine" [w1 ,w2 ,g3] &&
    withFuncs E.fs (arrayOf num) w1     == applyFunc E.fs p "plotLine" [w1 ,w2',g3] &&
    withFuncs E.fs (arrayOf num) w1     == applyFunc E.fs p "plotLine" [w1 ,g2 ,w3] &&
    withFuncs E.fs (arrayOf num) w1'    == applyFunc E.fs p "plotLine" [w1',g2 ,g3] &&
    withFuncs E.fs (arrayOf num) w1'    == applyFunc E.fs p "plotLine" [w1',w2 ,g3] &&
    withFuncs E.fs (arrayOf num) w1'    == applyFunc E.fs p "plotLine" [w1',w2',g3] &&
    withFuncs E.fs (arrayOf num) w1'    == applyFunc E.fs p "plotLine" [w1',g2 ,w3] &&
    withFuncs E.fs (arrayOf num) w2     == applyFunc E.fs p "plotLine" [g1 ,w2 ,g3] &&
    withFuncs E.fs (arrayOf num) w2     == applyFunc E.fs p "plotLine" [g1 ,w2 ,w3] &&
    withFuncs E.fs (arrayOf num) w2'    == applyFunc E.fs p "plotLine" [g1 ,w2',g3] &&
    withFuncs E.fs (arrayOf num) w2'    == applyFunc E.fs p "plotLine" [g1 ,w2',w3] &&
    withFuncs E.fs  obj          w3     == applyFunc E.fs p "plotLine" [g1 ,g2 ,w3] &&
    success "plotLine"                  == applyFunc E.fs p "plotLine" [g1 ,g2 ,g3]

prop_ReturnValueShow (P p) a1ras' = let (fs,a1) = addFunc' "tablesAndPlots" a1r; (_,a1r) = mkO a1rs; a1rs = tablesAndPlots a1ras'; expected = Right (ObjO p [("result",a1r)])
                                    in  expected == applyFunc fs p "show" [a1] && expected == evalStateT (showF p [a1r]) []

prop_ReturnValueMulti (P p) a1as = let a1s = uns a1as; a1rs = map (\(NumT q _ _ x) -> NumO q x) a1s
                                       a1 = ArrayT p ("","") a1s; a1r = ArrayO p a1rs
                                       expected = Right $ NumO p $ product $ map (\(NumO _ x)->x) a1rs
                                   in  expected == applyFunc funcs p "multi" [a1] && expected == evalStateT (multiF p [a1r]) []

prop_ReturnValueMean (P pn) (P pa) (NonEmpty a1as) = 
  let a1s = uns a1as; a1rs = map (\(NumT q _ _ x) -> NumO q x) a1s; 
      a1 = ArrayT pa ws2 a1s; a1r = ArrayO pa a1rs
      w1 = ArrayT pa ws2 [];  w1r = ArrayO pa []; 
      expected = Right $ NumO pn $ product (map (\(NumO _ x)->x) a1rs) / genericLength a1rs
      expectedEmpty = Left (IllegalEmptyArray pa "mean")
  in  expected      == applyFunc funcs pn "mean" [a1]  && 
      expected      == evalStateT (meanF pn [a1r]) [] &&
      expectedEmpty == applyFunc funcs pn "mean" [w1]  && 
      expectedEmpty == evalStateT (meanF pa [w1r]) []

-- prop_ReturnValueDesc

prop_ReturnValueTable = True

{-| Mandatory type signatures -}
prop_NbArgs1 :: P -> [ExpTA] ->  Property
prop_NbArgs2 :: P -> [ExpTA] ->  Property
prop_NbArgs3 :: P -> [ExpTA] ->  Property

prop_TypeMismatchShow     :: P ->  ExpOA    -> [ExpTS]   ->  ExpTS                                                        -> Property
prop_TypeMismatchMulti    :: P -> [NumOA]   -> [ExpTS]   ->  ExpTS                                                        -> Property
prop_TypeMismatchMean     :: P -> [NumOA]   -> [ExpTS]   ->  ExpTS                                                        -> Property
prop_TypeMismatchDesc     :: P -> [NumOA]   -> [ExpTS]   ->  ExpTS                                                        -> Property
prop_TypeMismatchTable    :: P -> [ArrayTS] ->  ObjTS    -> [ExpTS]  ->  ExpTS    ->  ExpTS                               -> Property
prop_TypeMismatchNTimes   :: P ->  NumTA    ->  NumTA    ->  ExpTS   ->  ExpTS                                            -> Property
prop_TypeMismatchTake     :: P ->  NumTA    ->  ArrayTS  ->  TableOA ->  ExpTS    ->  ExpTS                               -> Property
prop_TypeMismatchSort     :: P ->  NumTA    -> [ArrayTS] ->  TableOA ->  ExpTS    -> [ExpTS] ->  ExpTS                    -> Property
prop_TypeMismatchCol      :: P ->  NumTA    -> [ArrayTS] ->  TableOA ->  ExpTS    -> [ExpTS] ->  ExpTS                    -> Property
prop_TypeMismatchPlotLine :: P -> [NumTA]   -> [NumTA]   ->  ObjTS   -> [ExpTS]   ->  ExpTS  -> [ExpTS] -> ExpTS -> ExpTS -> Property

prop_ReturnValueShow  :: P      -> [ExpOA]             -> Bool
prop_ReturnValueMulti :: P      -> [NumTA]             -> Bool
prop_ReturnValueMean  :: P -> P ->  NonEmptyList NumTA -> Bool
--prop_ReturnValueDesc  :: P -> P ->  NonEmptyList NumTA -> Bool
