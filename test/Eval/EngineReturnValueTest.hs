{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Eval.EngineReturnValueTest where

import Control.Monad.State                 (evalStateT)
import Data.Eval                           (ExpObj(..))
import Data.List                           (genericLength)
import Data.Token                          (ExpToken(..))
import Eval.Engine                         (funcs,showF,multiF,meanF,descF,tableF,nTimesF,takeTF,takeAF,sortTF,sortAF,colTF,colAF)
import Eval.EngineTestUtils                (TableValidArgs(..),addFunc',mk',mkO',tablesAndPlots,emptyArray,mkMultiMeanReturn,unprecise,
                                            mkTableValidArgs,unsafeMarshallP,unsafeMarshall,sortTOn,sortAOn,keepInRange,mkSortColArray)
import Eval.FunctionEvalTestUtils1         (ExpOA(..),TableOA(..),ExpTS(..),ArrayTS(..),applyFunc,p0)
import Parser.MonolithicParserTestUtils    (P(..),NumTA(..),un)
import Test.Framework                      (TestSuite,Property,makeTestSuite,makeQuickCheckTest,makeLoc,qcAssertion,(==>))
                                           
import Data.Vector                         (fromList)
import Statistics.Sample                   (mean,variance,skewness,kurtosis)

prop_ReturnValueShow (P p) a1ras' = let (fs,a1) = addFunc' "tablesAndPlots" a1r; (_,a1r) = mkO' a1rs; a1rs = tablesAndPlots a1ras'; expected = Right (ObjO p [("result",a1r)])
                                    in  True ==> expected == applyFunc fs p "show" [a1] && expected == evalStateT (showF p a1r) []

prop_ReturnValueMulti (P pf) (P pa) a1as = not (null a1as) ==>
  let (a1,a1rs) = mkMultiMeanReturn a1as pa; expected = Right $ NumO pf $ product $ map (\(NumO _ x)->x) a1rs in  not (null a1as) ==>
      expected == applyFunc funcs pf "multi" [a1]  &&
      expected == evalStateT (multiF pf a1rs) []

prop_ReturnValueMean  (P pf) (P pa) a1as = not (null a1as) ==>
  let (a1,a1rs) = mkMultiMeanReturn a1as pa; expected = Right $ NumO pf $ sum (map (\(NumO _ x)->x) a1rs) / genericLength a1rs in not (null a1as) ==>
      unprecise expected == unprecise (applyFunc funcs pf "mean" [a1])  &&
      unprecise expected == unprecise (evalStateT (meanF pf a1rs) [])

prop_ReturnValueDesc (P pf) (P pa) a1as = length a1as >= 2 ==>
  let (a1,a1rs) = mkMultiMeanReturn a1as pa
      ns  = map (\(NumO _ x)->x) a1rs
      ns' = fromList ns
      expected = Right $ TableO pf [map (StrO pf) ["count",                 "sum",  "mean",  "variance",   "skewness",   "kurtosis"],
                                    map (NumO pf) [fromIntegral $ length ns, sum ns, mean ns',variance ns', skewness ns', kurtosis ns']] [] in
      expected == applyFunc funcs pf "descriptive" [a1] &&
      expected == evalStateT (descF pf a1rs) []

prop_ReturnValueTable (P pf) (TableValidArgs g1ss g2s) useHeader = any (not.null) g1ss ==>
  let (g1@(ArrayT _ _ es),g2@(ObjT _ _ ps),expected) = mkTableValidArgs pf g1ss g2s useHeader in
   expected == applyFunc funcs pf "table" [g1, g2] &&
   expected == evalStateT (tableF pf (map unsafeMarshall es) (map unsafeMarshallP ps)) []

prop_ReturnValueNTimes (P pf) a1@(NumTA _ (NumT p1 _ _ v1)) a2@(NumTA _ (NumT _ _ _ v2)) =
  let expected = Right $ ArrayO pf $ replicate (floor v2) (NumO p1 v1) in
      expected == applyFunc funcs pf "nTimes" [un a1, un a2] &&
      expected == evalStateT (nTimesF pf (NumO p1 v1) v2) []
prop_ReturnValueNTimes _ x y = error $ "EngineTest::prop_ReturnValueNTimes [Unexpected pattern ["++show x++"] and ["++show y++"]]"

prop_ReturnValueTakeTable (P pf) (NumTA _ a1@(NumT _ _ _ v)) (TableOA a2tr@(TableO _ cols header)) = any (not.null) cols ==>
  let (fs,a2t) = addFunc' "mkTable" a2tr; n = floor v; expected = Right $ TableO pf (map (take n) cols) header in
   expected == applyFunc fs  pf "take" [a1,a2t] && 
   expected == evalStateT (takeTF pf n cols header) []
prop_ReturnValueTakeTable _ x y = error $ "EngineTest::prop_ReturnValueTake [Unexpected pattern ["++show x++"] and ["++show y++"]]"

prop_ReturnValueTakeArray (P pf) (NumTA _ a1@(NumT _ _ _ v)) a2as = not (null a2as) ==>
  let n = floor v; (a2s,a2a) = mk' a2as; a2ar = map unsafeMarshall a2s; expected = Right $ ArrayO pf $ take n a2ar in
   expected == applyFunc funcs pf "take" [a1,a2a] && 
   expected == evalStateT (takeAF pf n a2ar) []
prop_ReturnValueTakeArray _ x y = error $ "EngineTest::prop_ReturnValueTake [Unexpected pattern ["++show x++"] and ["++show y++"]]"

prop_ReturnValueSortTable (P pf) (NumTA _ a1') (TableOA a2tr@(TableO _ cols header)) = any (not.null) cols ==>
  let (a1,n) = keepInRange a1' (length cols); (fs,a2t) = addFunc' "mkTable" a2tr; expected = Right $ TableO pf (sortTOn n cols) header in
   expected == applyFunc fs    pf "sort" [a1,a2t] && 
   expected == evalStateT (sortTF pf p0 n cols header) []
prop_ReturnValueSortTable _ x y = error $ "EngineTest::prop_ReturnValueTake [Unexpected pattern ["++show x++"] and ["++show y++"]]"

prop_ReturnValueSortArray (P pf) (NumTA _ a1') a2as = any (not.emptyArray.un) a2as ==>
  let (n, a1,aOfArrays,arrays,mArrays) = mkSortColArray a1' a2as; expected = Right $ ArrayO pf (sortAOn n $ map unsafeMarshall arrays)in
   expected == applyFunc funcs pf "sort" [a1,aOfArrays] && 
   expected == evalStateT (sortAF pf p0 n mArrays) []

prop_ReturnValueColTable (P pf) (NumTA _ a1') (TableOA a2tr@(TableO _ cols _)) = any (not.null) cols ==>
  let (a1,n) = keepInRange a1' (length cols); (fs,a2t) = addFunc' "mkTable" a2tr; expected = Right $ ArrayO pf (cols !! n) in
   expected == applyFunc fs    pf "col" [a1,a2t] && 
   expected == evalStateT (colTF pf p0 n cols) []
prop_ReturnValueColTable _ x y = error $ "EngineTest::prop_ReturnValueTake [Unexpected pattern ["++show x++"] and ["++show y++"]]"

prop_ReturnValueColArray (P pf) (NumTA _ a1') a2as = any (not.emptyArray.un) a2as ==>
  let (n, a1,aOfArrays,arrays,mArrays) = mkSortColArray a1' a2as; expected = Right $ let ArrayO _ es = unsafeMarshall $ arrays !! n in ArrayO pf es in
   expected == applyFunc funcs pf "col" [a1,aOfArrays] && 
   expected == evalStateT (colAF pf p0 n mArrays) []

{-| Mandatory type signatures -}
prop_ReturnValueShow      :: P      -> [ExpOA]           -> Property
prop_ReturnValueMulti     :: P -> P -> [NumTA]           -> Property
prop_ReturnValueMean      :: P -> P -> [NumTA]           -> Property
prop_ReturnValueDesc      :: P -> P -> [NumTA]           -> Property
prop_ReturnValueTable     :: P -> TableValidArgs -> Bool -> Property
prop_ReturnValueTakeTable :: P -> NumTA -> TableOA       -> Property
prop_ReturnValueTakeArray :: P -> NumTA -> [ExpTS]       -> Property
prop_ReturnValueSortTable :: P -> NumTA ->  TableOA      -> Property
prop_ReturnValueSortArray :: P -> NumTA -> [ArrayTS]     -> Property
prop_ReturnValueColTable :: P -> NumTA ->  TableOA      -> Property
prop_ReturnValueColArray :: P -> NumTA -> [ArrayTS]     -> Property
