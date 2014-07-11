{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Eval.Engine.EngineReturnValueTest where

import Prelude hiding (null)

import qualified Data.Vector as V
import qualified Prelude as P

import Control.Monad.State
import Data.Eval
import Data.ExpObj
import Data.ExpToken
import Data.List
import Eval.Engine
import Eval.Engine.EngineTestUtils

import Eval.Function.FunctionEvalTestUtils1
import Eval.Function.FunctionEvalTestUtils2
import Parser.MonolithicParserTestUtils
import Test.Framework

import Statistics.Sample

prop_Show (P p) a1ras' = let (funs,a1) = addFunc' "tablesAndPlots" a1r; (_,a1r) = mkO' a1rs; a1rs = tablesAndPlots a1ras'; expected = Right (ObjO p [("result",a1r)])
                         in  True ==> expected == applyFunc funs p "show" [a1] && expected == evalStateT (showF p a1r) []

prop_Multi (P pf) (P pa) a1as = not (P.null a1as) ==>
  let (a1,a1rs) = mkMultiMeanReturn a1as pa; expected = Right $ NumO pf $ product $ map (\(NumO _ x)->x) $ filter isNum a1rs in  not (null a1as) ==>
      expected == applyFunc funcs pf "multi" [a1]  &&
      expected == evalStateT (multiF pf a1rs) []

prop_Mean  (P pf) (P pa) a1as = not (null a1as) ==>
  let (a1,a1rs) = mkMultiMeanReturn a1as pa; expected = Right $ NumO pf $ sum (map (\(NumO _ x)->x) $ filter isNum a1rs) / genericLength a1rs in not (null a1as) ==>
      unprecise expected == unprecise (applyFunc funcs pf "mean" [a1])  &&
      unprecise expected == unprecise (evalStateT (meanF pf a1rs) [])

prop_Desc (P pf) (P pa) a1as = length a1as >= 2 ==>
  let (a1,a1rs) = mkMultiMeanReturn a1as pa
      ns  = map (\(NumO _ x)->x) $ filter isNum a1rs
      ns' = V.fromList ns
      sExpected = show expected
      expected :: Eval ExpObj
      expected = Right $ TableO pf [map (StrO pf) ["count",                 "sum",  "mean",  "variance",   "skewness",   "kurtosis"],
                                    map (NumO pf) [fromIntegral $ length ns, sum ns, mean ns',variance ns', skewness ns', kurtosis ns']] [] in
      (expected == applyFunc funcs pf "descriptive" [a1] &&
       expected == evalStateT (descF pf a1rs) []) ||
      (sExpected == show (applyFunc funcs pf "descriptive" [a1]) &&
       sExpected == show (evalStateT (descF pf a1rs) []))

prop_Table (P pf) (TableValidArgs g1ss g2s) useHeader = any (not.null) g1ss ==>
  let (g1@(ArrT _ _ es),g2@(ObjT _ _ ps),expected) = mkTableValidArgs pf g1ss g2s useHeader in
   expected == applyFunc funcs pf "table" [g1, g2] &&
   expected == evalStateT (tableF pf (map unsafeMarshall es) (map unsafeMarshallP ps)) []

prop_NTimes (P pf) a1@(NumTA _ (NumT p1 _ _ v1)) a2@(NumTA _ (NumT _ _ _ v2)) =
  let expected = Right $ ArrayO pf $ replicate (floor v2) (NumO p1 v1) in
      expected == applyFunc funcs pf "nTimes" [un a1, un a2] &&
      expected == evalStateT (nTimesF pf (NumO p1 v1) v2) []
prop_NTimes _ x y = error $ "EngineTest::prop_NTimes [Unexpected pattern ["++show x++"] and ["++show y++"]]"

prop_TakeTable (P pf) (NumTA _ a1@(NumT _ _ _ v)) (TableOA a2tr@(TableO _ cols header)) = any (not.null) cols ==>
  let (fs,a2t) = addFunc' "mkTable" a2tr; n = floor v; expected = Right $ TableO pf (map (take n) cols) header in
   expected == applyFunc fs  pf "take" [a1,a2t] &&
   expected == evalStateT (takeTF pf n cols header) []
prop_TakeTable _ x y = error $ "EngineTest::prop_Take [Unexpected pattern ["++show x++"] and ["++show y++"]]"

prop_TakeArray (P pf) (NumTA _ a1@(NumT _ _ _ v)) a2as = not (null a2as) ==>
  let n = floor v; (a2s,a2a) = mk' a2as; a2ar = map unsafeMarshall a2s; expected = Right $ ArrayO pf $ take n a2ar in
   expected == applyFunc funcs pf "take" [a1,a2a] &&
   expected == evalStateT (takeAF pf n a2ar) []
prop_TakeArray _ x y = error $ "EngineTest::prop_Take [Unexpected pattern ["++show x++"] and ["++show y++"]]"

prop_SortTable (P pf) (NumTA _ a1') (TableOA a2tr@(TableO _ cols header)) = any (not.null) cols ==>
  let (a1,n) = keepInRange a1' (length cols); (fs,a2t) = addFunc' "mkTable" a2tr; expected = Right $ TableO pf (sortTOn n cols) header in
   expected == applyFunc fs    pf "sort" [a1,a2t] &&
   expected == evalStateT (sortTF pf p0 n cols header) []
prop_SortTable _ x y = error $ "EngineTest::prop_Take [Unexpected pattern ["++show x++"] and ["++show y++"]]"

prop_SortArray (P pf) (NumTA _ a1') (TableValidArgs g2ss _) = any (not.null) g2ss ==>
  let (n, a1,aOfArrays,arrays,mArrays) = mkSortColArray a1' g2ss; expected = Right $ ArrayO pf (sortAOn n $ map unsafeMarshall arrays)in
   expected == applyFunc funcs pf "sort" [a1,aOfArrays] &&
   expected == evalStateT (sortAF pf p0 n mArrays) []

prop_ColTable (P pf) (NumTA _ a1') (TableOA a2tr@(TableO _ cols _)) = any (not.null) cols ==>
  let (a1,n) = keepInRange a1' (length cols); (fs,a2t) = addFunc' "mkTable" a2tr; expected = Right $ ArrayO pf (cols !! n) in
   expected == applyFunc fs    pf "col" [a1,a2t] &&
   expected == evalStateT (colTF pf p0 n cols) []
prop_ColTable _ x y = error $ "EngineTest::prop_Take [Unexpected pattern ["++show x++"] and ["++show y++"]]"

prop_ColArray (P pf) (NumTA _ a1') (TableValidArgs g2ss _) = any (not.null) g2ss ==>
  let (n, a1,aOfArrays,arrays,mArrays) = mkSortColArray a1' g2ss; expected = Right $ let ArrayO _ es = unsafeMarshall $ arrays !! n in ArrayO pf es in
   expected == applyFunc funcs pf "col" [a1,aOfArrays] &&
   expected == evalStateT (colAF pf p0 n mArrays) []

{-| Mandatory type signatures -}
prop_Show      :: P      -> [ExpOA]            -> Property
prop_Multi     :: P -> P -> [AtomTA]           -> Property
prop_Mean      :: P -> P -> [AtomTA]           -> Property
prop_Desc      :: P -> P -> [AtomTA]           -> Property
prop_Table     :: P -> TableValidArgs -> Bool  -> Property
prop_TakeTable :: P -> NumTA -> TableOA        -> Property
prop_TakeArray :: P -> NumTA -> [ExpTS]        -> Property
prop_SortTable :: P -> NumTA ->  TableOA       -> Property
prop_SortArray :: P -> NumTA -> TableValidArgs -> Property
prop_ColTable :: P -> NumTA ->  TableOA        -> Property
prop_ColArray :: P -> NumTA -> TableValidArgs  -> Property
