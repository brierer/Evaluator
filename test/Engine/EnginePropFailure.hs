--{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Engine.EnginePropFailure where
--
--import qualified Eval.EngineTestUtils as E (fs)
--
--import Control.Arrow                       ((***))
--import Control.Monad.State                 (evalStateT)
--import Data.EvalError                      (EvalError(..))
--import Data.ExpObj                         (ExpObj(..))
--import Data.ExpToken                       (PairToken(..),IdToken(..),ExpToken(..))
--import Eval.Engine                         (funcs,sortTF,sortAF,colTF,colAF)
--import Eval.EngineTestUtils                (TableValidArgs(..),addFunc,mk,mk',mkObj,mkObj',oneArrayOf,success,toArray,emptySortColCase,
--                                            tableColumnLengthCase,tableHeaderLengthCase,mkOutOfBoundsTable,mkOutOfBoundsArray,typeMismatchSortColCase)
--import Eval.Function                       (table,plot,array,str,num,atom,arrayOf,objOf,nonEmpty,(<|>),withFuncs)
--import Eval.FunctionEvalTestUtils1         (ExpOA(..),TableOA(..),AtomTA(..),ExpTS(..),ArrayTS(..),applyFunc,p0,ws2)
--import Eval.FunctionEvalTestUtils2         (Is(..))
--import Parser.MonolithicParserTestUtils    (Unto(..),P(..),ExpTA(..),StrTA(..),NumTA(..),un,uns)
--import Test.Framework                      (TestSuite,Property,makeTestSuite,makeQuickCheckTest,makeLoc,qcAssertion,(==>))
--
--prop_NbArgs1 (P p) esTA = length esTA > 1 ==> let es = uns esTA in
--    all (\name -> Left (InvalidNbOfArgs p name 1 0)           == applyFunc E.fs p name ([] :: [ExpToken])
--               && Left (InvalidNbOfArgs p name 1 (length es)) == applyFunc E.fs p name es)
--      ["show","multi","mean","descriptive"]
--
--prop_NbArgs2 (P p) esTA = length esTA > 2 ==> let es = uns esTA in
--    all (\name -> Left (InvalidNbOfArgs p name 2 0)           == applyFunc E.fs p name ([] :: [ExpToken])
--               && Left (InvalidNbOfArgs p name 2 1)           == applyFunc E.fs p name (take 1 es)
--               && Left (InvalidNbOfArgs p name 2 (length es)) == applyFunc E.fs p name es)
--      ["table","nTimes","take","sort","col"]
--
--prop_NbArgs3 (P p) esTA = length esTA > 3 ==> let es = uns esTA in
--  all (\name -> Left (InvalidNbOfArgs p name 3 0)           == applyFunc E.fs p name ([] :: [ExpToken])
--             && Left (InvalidNbOfArgs p name 3 1)           == applyFunc E.fs p name (take 1 es)
--             && Left (InvalidNbOfArgs p name 3 2)           == applyFunc E.fs p name (take 2 es)
--             && Left (InvalidNbOfArgs p name 3 (length es)) == applyFunc E.fs p name es)
--    ["plot"]
--
--prop_TypeMismatchShow (P p) (ExpOA g1r) w1as (ExpTS w1') =
--  let (fs,g1) = addFunc "tableOrPlot" g1r; (w1s,w1) = mk' w1as in (isTable g1r || isPlot g1r) && not (null w1s) && not (isArray w1') ==>
--    withFuncs fs (arrayOf $ table <|> plot) w1  == applyFunc fs p "show" [w1]  &&
--    withFuncs fs (arrayOf $ table <|> plot) w1' == applyFunc fs p "show" [w1'] &&
--    success "show"                              == applyFunc fs p "show" [toArray p g1]
--
--prop_TypeMismatchMulti (P p) g1ras w1as (ExpTS w1') =
--  let (fs,g1,w1s,w1) = oneArrayOf g1ras w1as in not (null g1ras) && any (not.isAtom) w1s && not (isArray w1') ==>
--    withFuncs fs (arrayOf atom) w1  == applyFunc fs p "multi" [w1]  &&
--    withFuncs fs (arrayOf atom) w1' == applyFunc fs p "multi" [w1'] &&
--    success "multi"                 == applyFunc fs p "multi" [g1]
--
--prop_TypeMismatchMean (P p) g1ras w1as (ExpTS w1') =
--  let (fs,g1,w1s,w1) = oneArrayOf g1ras w1as in not (null g1ras) && any (not.isAtom) w1s && not (isArray w1') ==>
--    withFuncs fs (arrayOf atom) w1  == applyFunc fs p "mean" [w1]  &&
--    withFuncs fs (arrayOf atom) w1' == applyFunc fs p "mean" [w1'] &&
--    success "mean"                  == applyFunc fs p "mean" [g1]
--
--prop_TypeMismatchDesc (P p) g1ras w1as (ExpTS w1') =
--  let (fs,g1,w1s,w1) = oneArrayOf g1ras w1as in not (null g1ras) && any (not.isAtom) w1s && not (isArray w1') ==>
--    withFuncs fs (arrayOf atom) w1        == applyFunc fs p "descriptive" [w1]  &&
--    withFuncs fs (arrayOf atom) w1'       == applyFunc fs p "descriptive" [w1'] &&
--    success "descriptive"                == applyFunc fs p "descriptive" [g1]
--
--prop_TypeMismatchTable (P p) (TableValidArgs g1ss g2s) w1as (ExpTS w1') w2as (ExpTS w2') =
--  let g1 = ArrayT p0 ws2 $ map (ArrayT p0 ws2) g1ss
--      g2 = ObjT p0 ws2 [PairT (IdT p0 ws2 "col") $ ArrayT p0 ws2 g2s]
--      (w1s,w1) = mk' w1as; (w2s,w2) = mkObj w2as in
--    any (not.null) g1ss && not (null g2s) && any (not.isArray) w1s && not (isArray w1') && any (not.isStr) w2s && not (isObj w2') ==>
--    withFuncs E.fs (arrayOf $ arrayOf atom) w1  == applyFunc E.fs p "table" [w1 ,g2]  &&
--    withFuncs E.fs (arrayOf $ arrayOf atom) w1' == applyFunc E.fs p "table" [w1',w2]  &&
--    withFuncs E.fs (objOf $ arrayOf str) w2     == applyFunc E.fs p "table" [g1 ,w2]  &&
--    withFuncs E.fs (objOf $ arrayOf str) w2'    == applyFunc E.fs p "table" [g1 ,w2'] &&
--    success "table"                             == applyFunc E.fs p "table" [g1 ,g2]
--
--prop_TypeMismatchNTimes (P p) (NumTA _ g1) (NumTA _ g2) (ExpTS w1) (ExpTS w2) = not (isNum w1) && not (isNum w2) ==>
--  withFuncs E.fs num w1           == applyFunc E.fs p "nTimes" [w1,g2] &&
--  withFuncs E.fs num w1           == applyFunc E.fs p "nTimes" [w1,w2] &&
--  withFuncs E.fs num w2           == applyFunc E.fs p "nTimes" [g1,w2] &&
--  success "nTimes"                == applyFunc E.fs p "nTimes" [g1,g2]
--
--prop_TypeMismatchTake (P p) (NumTA _ g1) (ArrayTS g2) (TableOA g2r) (ExpTS w1) (ExpTS w2) = not (isNum w1) && not (isArray w2) ==>
--  let (fs,g2') = addFunc "table" g2r in
--  withFuncs fs  num              w1 == applyFunc fs p "take" [w1,g2]  &&
--  withFuncs fs  num              w1 == applyFunc fs p "take" [w1,g2'] &&
--  withFuncs fs  num              w1 == applyFunc fs p "take" [w1,w2]  &&
--  withFuncs fs (table <|> array) w2 == applyFunc fs p "take" [g1,w2]  &&
--  success "take"                    == applyFunc fs p "take" [g1,g2]  &&
--  success "take"                    == applyFunc fs p "take" [g1,g2']
--
--prop_TypeMismatchSort = typeMismatchSortColCase "sort"
--prop_TypeMismatchCol  = typeMismatchSortColCase "col"
--
--prop_TypeMismatchPlotLine (P p) g1as g2as g3as w1as (ExpTS w1') w2as (ExpTS w2') w3as (ExpTS w3') =
--  let (_,g1) = mk' g1as; (_,g2) = mk' g2as; (_,g3) = mkObj' g3as; (w1s,w1) = mk' w1as; (w2s,w2) = mk' w2as; (w3s,w3) = mkObj' w3as in
--  any (not.isNum) w1s && not (isArray w1') && any (not.isNum) w2s && not (isArray w2') && any (not.isStr) w3s && not (isObj w3') ==>
--    all (\xs -> withFuncs E.fs (arrayOf num) w1  == applyFunc E.fs p "plot" xs) [[w1 ,x  ,y] | x <- [g2,w2,w2'], y <- [g3,w3,w3']] &&
--    all (\xs -> withFuncs E.fs (arrayOf num) w1' == applyFunc E.fs p "plot" xs) [[w1',x  ,y] | x <- [g2,w2,w2'], y <- [g3,w3,w3']] &&
--    all (\xs -> withFuncs E.fs (arrayOf num) w2  == applyFunc E.fs p "plot" xs) [[g1 ,w2 ,x] | x <- [g3,w3,w3']] &&
--    all (\xs -> withFuncs E.fs (arrayOf num) w2' == applyFunc E.fs p "plot" xs) [[g1 ,w2',x] | x <- [g3,w3,w3']] &&
--    withFuncs E.fs (objOf str) w3    == applyFunc E.fs p "plot" [g1 ,g2, w3]  &&
--    withFuncs E.fs (objOf str) w3'   == applyFunc E.fs p "plot" [g1 ,g2, w3'] &&
--    success "plot"               == applyFunc E.fs p "plot" [g1 ,g2 ,g3]
--
--prop_EmptyArgMulti (P pf) (P pa) g1as = not (null g1as) ==>
--  let (_,g1) = mk' g1as; (_,w1) = mk pa ([] :: [NumTA])
--  in  Left (IllegalEmpty pa)                     == applyFunc E.fs pf "multi" [w1] &&
--      withFuncs E.fs (nonEmpty $ arrayOf num) w1 == applyFunc E.fs pf "multi" [w1] &&
--      success "multi"                            == applyFunc E.fs pf "multi" [g1]
--
--prop_EmptyArgMean (P pf) (P pa) g1as = not (null g1as) ==>
--  let (_,g1) = mk' g1as; (_,w1) = mk pa ([] :: [NumTA])
--  in  Left (IllegalEmpty pa) == applyFunc E.fs pf "mean" [w1] &&
--      success "mean"         == applyFunc E.fs pf "mean" [g1]
--
--prop_EmptyArgDesc (P pf) (P pa) g1as = not (null g1as) ==>
--  let (_,g1) = mk' g1as; (_,w1) = mk pa ([] :: [NumTA])
--  in  Left (IllegalEmpty pa) == applyFunc E.fs pf "descriptive" [w1] &&
--      success "descriptive"  == applyFunc E.fs pf "descriptive" [g1]
--
--prop_EmptyArgTable (P pf) (P pa) (TableValidArgs g1ss _) g2as  =
--  let g1  = ArrayT p0 ws2 $ map (ArrayT p0 ws2) g1ss
--      w1' = ArrayT p0 ws2 $ map (ArrayT p0 ws2) g1ss ++ [w1]
--      (_,o) = mkObj g2as; (_,w1) = mk pa ([] :: [ExpTS]) in any (not.null) g1ss ==>
--    Left (IllegalEmpty pa) == applyFunc E.fs pf "table" [w1 , o] &&
--    Left (IllegalEmpty pa) == applyFunc E.fs pf "table" [w1', o] &&
--    success "table"        == applyFunc E.fs pf "table" [g1 , o]
--
--prop_EmptyArgSort (P pf) (P pa) (NumTA _ n) = emptySortColCase "sort" pa pf n
--prop_EmptyArgCol  (P pf) (P pa) (NumTA _ n) = emptySortColCase "col"  pa pf n
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
--{-| Mandatory type signatures -}
--prop_NbArgs1 :: P -> [ExpTA] ->  Property
--prop_NbArgs2 :: P -> [ExpTA] ->  Property
--prop_NbArgs3 :: P -> [ExpTA] ->  Property
--
--prop_TypeMismatchShow     :: P ->   ExpOA        -> [ExpTS]            ->  ExpTS                                                                                       -> Property
--prop_TypeMismatchMulti    :: P ->  [AtomTA]      -> [ExpTS]            ->  ExpTS                                                                                       -> Property
--prop_TypeMismatchMean     :: P ->  [AtomTA]      -> [ExpTS]            ->  ExpTS                                                                                       -> Property
--prop_TypeMismatchDesc     :: P ->  [AtomTA]      -> [ExpTS]            ->  ExpTS                                                                                       -> Property
--prop_TypeMismatchTable    :: P -> TableValidArgs -> [ExpTS]            ->  ExpTS            -> [(String,[ExpTS])] ->  ExpTS                                            -> Property
--prop_TypeMismatchNTimes   :: P ->   NumTA        ->  NumTA             ->  ExpTS            ->  ExpTS                                                                  -> Property
--prop_TypeMismatchTake     :: P ->   NumTA        ->  ArrayTS           ->  TableOA          ->  ExpTS    ->  ExpTS                                                     -> Property
--prop_TypeMismatchSort     :: P ->   NumTA        -> TableValidArgs     ->  TableOA          ->  ExpTS    -> [[ExpTS]] -> [ExpTS] ->  ExpTS                             -> Property
--prop_TypeMismatchCol      :: P ->   NumTA        -> TableValidArgs     ->  TableOA          ->  ExpTS    -> [[ExpTS]] -> [ExpTS] ->  ExpTS                             -> Property
--prop_TypeMismatchPlotLine :: P ->  [NumTA]       -> [NumTA]            ->  [(String,StrTA)] -> [ExpTS]   ->   ExpTS   -> [ExpTS] -> ExpTS -> [(String,ExpTS)] -> ExpTS -> Property
--
--prop_EmptyArgMulti :: P -> P -> [NumTA]                              -> Property
--prop_EmptyArgMean  :: P -> P -> [NumTA]                              -> Property
--prop_EmptyArgDesc  :: P -> P -> [NumTA]                              -> Property
--prop_EmptyArgTable :: P -> P -> TableValidArgs -> [(String,[StrTA])] -> Property
--prop_EmptyArgSort  :: P -> P -> NumTA -> TableValidArgs              -> Property
--prop_EmptyArgCol   :: P -> P -> NumTA -> TableValidArgs              -> Property
--
--prop_TableColumnLengthMismatch :: [(P,[AtomTA])]  -> [(String,[StrTA])] -> Property
--prop_TableHeaderLengthMismatch :: P -> TableValidArgs -> TableValidArgs -> Property
--
--prop_IndexOutOfBoundsSortTable :: P -> NumTA ->  TableOA       -> Property
--prop_IndexOutOfBoundsSortArray :: P -> NumTA -> TableValidArgs -> Property
--prop_IndexOutOfBoundsColTable  :: P -> NumTA ->  TableOA       -> Property
--prop_IndexOutOfBoundsColArray  :: P -> NumTA -> TableValidArgs -> Property
