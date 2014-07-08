{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Eval.EngineTest where

import qualified Eval.EngineTestUtils as E (fs)

import Control.Arrow                       ((***))
import Control.Monad.State                 (evalStateT)
import Data.Eval                           (EvalError(..),ExpObj(..))
import Data.List                           (genericLength)
import Data.Token                          (ExpToken(..))
import Eval.Engine                         (funcs,showF,multiF,meanF,descF,tableF)
import Eval.EngineTestUtils                (TableValidArgs(..),addFunc,addFunc',mk,mk',mkO',mkObj,mkObj',oneArrayOfNum,success,toArray,tablesAndPlots,emptyArray,emptySortColCase,
                                            tableColumnLengthCase,tableHeaderLengthCase,mkMultiMeanReturn,unprecise,mkTableValidArgs,unsafeMarshall)
import Eval.Function                       (table,plot,array,str,num,arrayOf,objOf,nonEmpty,(<|>),withFuncs)
import Eval.FunctionEvalTestUtils1         (ExpOA(..),TableOA(..),NumOA(..),ExpTS(..),ArrayTS(..),applyFunc)
import Eval.FunctionEvalTestUtils2         (Is(..))
import Parser.MonolithicParserTestUtils    (P(..),ExpTA(..),StrTA(..),NumTA(..),to,un,uns)
import Test.Framework                      (TestSuite,Property,makeTestSuite,makeQuickCheckTest,makeLoc,qcAssertion,(==>))

import Data.Vector (fromList)
import Statistics.Sample

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
    withFuncs E.fs (arrayOf num) w1  == applyFunc E.fs p "plotLine" [w1 ,g2 ,g3]  &&
    withFuncs E.fs (arrayOf num) w1  == applyFunc E.fs p "plotLine" [w1 ,w2 ,g3]  &&
    withFuncs E.fs (arrayOf num) w1  == applyFunc E.fs p "plotLine" [w1 ,w2',g3]  &&
    withFuncs E.fs (arrayOf num) w1  == applyFunc E.fs p "plotLine" [w1 ,g2 ,w3]  &&
    withFuncs E.fs (arrayOf num) w1  == applyFunc E.fs p "plotLine" [w1 ,g2 ,w3'] &&
    withFuncs E.fs (arrayOf num) w1  == applyFunc E.fs p "plotLine" [w1 ,w2 ,w3]  &&
    withFuncs E.fs (arrayOf num) w1  == applyFunc E.fs p "plotLine" [w1 ,w2',w3]  &&
    withFuncs E.fs (arrayOf num) w1  == applyFunc E.fs p "plotLine" [w1 ,w2 ,w3'] &&
    withFuncs E.fs (arrayOf num) w1  == applyFunc E.fs p "plotLine" [w1 ,w2',w3'] &&
                                     
    withFuncs E.fs (arrayOf num) w1' == applyFunc E.fs p "plotLine" [w1',g2 ,g3]  &&
    withFuncs E.fs (arrayOf num) w1' == applyFunc E.fs p "plotLine" [w1',w2 ,g3]  &&
    withFuncs E.fs (arrayOf num) w1' == applyFunc E.fs p "plotLine" [w1',w2',g3]  &&
    withFuncs E.fs (arrayOf num) w1' == applyFunc E.fs p "plotLine" [w1',g2 ,w3]  &&
    withFuncs E.fs (arrayOf num) w1' == applyFunc E.fs p "plotLine" [w1',g2 ,w3'] &&
    withFuncs E.fs (arrayOf num) w1' == applyFunc E.fs p "plotLine" [w1',w2 ,w3]  &&
    withFuncs E.fs (arrayOf num) w1' == applyFunc E.fs p "plotLine" [w1',w2',w3]  &&
    withFuncs E.fs (arrayOf num) w1' == applyFunc E.fs p "plotLine" [w1',w2 ,w3'] &&
    withFuncs E.fs (arrayOf num) w1' == applyFunc E.fs p "plotLine" [w1',w2',w3'] &&
                                     
    withFuncs E.fs (arrayOf num) w2  == applyFunc E.fs p "plotLine" [g1 ,w2 ,g3]  &&
    withFuncs E.fs (arrayOf num) w2  == applyFunc E.fs p "plotLine" [g1 ,w2 ,w3]  &&
    withFuncs E.fs (arrayOf num) w2  == applyFunc E.fs p "plotLine" [g1 ,w2 ,w3'] &&
    
    withFuncs E.fs (arrayOf num) w2' == applyFunc E.fs p "plotLine" [g1 ,w2',g3]  &&
    withFuncs E.fs (arrayOf num) w2' == applyFunc E.fs p "plotLine" [g1 ,w2',w3]  &&
    withFuncs E.fs (arrayOf num) w2' == applyFunc E.fs p "plotLine" [g1 ,w2',w3'] &&
    
    withFuncs E.fs (objOf str) w3    == applyFunc E.fs p "plotLine" [g1 ,g2, w3]  &&
    withFuncs E.fs (objOf str) w3'   == applyFunc E.fs p "plotLine" [g1 ,g2, w3'] &&
                                     
    success "plotLine"               == applyFunc E.fs p "plotLine" [g1 ,g2 ,g3]

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

prop_ReturnValueShow (P p) a1ras' = let (fs,a1) = addFunc' "tablesAndPlots" a1r; (_,a1r) = mkO' a1rs; a1rs = tablesAndPlots a1ras'; expected = Right (ObjO p [("result",a1r)])
                                    in  True ==> expected == applyFunc fs p "show" [a1] && expected == evalStateT (showF p [a1r]) []

prop_ReturnValueMulti (P pf) (P pa) a1as = not (null a1as) ==>
  let (a1,a1rs,a1r) = mkMultiMeanReturn a1as pa; expected = Right $ NumO pf $ product $ map (\(NumO _ x)->x) a1rs in  not (null a1as) ==>
      expected == applyFunc funcs pf "multi" [a1]  &&
      expected == evalStateT (multiF pf [a1r]) []

prop_ReturnValueMean  (P pf) (P pa) a1as = not (null a1as) ==>
  let (a1,a1rs,a1r) = mkMultiMeanReturn a1as pa; expected = Right $ NumO pf $ sum (map (\(NumO _ x)->x) a1rs) / genericLength a1rs in  not (null a1as) ==>
      unprecise expected == unprecise (applyFunc funcs pf "mean" [a1])  &&
      unprecise expected == unprecise (evalStateT (meanF pf [a1r]) [])

prop_ReturnValueDesc (P pf) (P pa) a1as = length a1as >= 2 ==>
  let (a1,a1rs,a1r) = mkMultiMeanReturn a1as pa
      ns  = map (\(NumO _ x)->x) a1rs
      ns' = fromList ns
      expected = Right $ TableO pf [map (StrO pf) ["count",                 "sum",  "mean",  "variance",   "skewness",   "kurtosis"],
                                    map (NumO pf) [fromIntegral $ length ns, sum ns, mean ns',variance ns', skewness ns', kurtosis ns']] [] in
      expected == applyFunc funcs pf "descriptive" [a1] &&
      expected == evalStateT (descF pf [a1r]) []
                            
prop_ReturnValueTable (P pf) (TableValidArgs g1ss g2s) useHeader = any (not.null) g1ss ==>
  let (g1,g2,expected) = mkTableValidArgs pf g1ss g2s useHeader in
   expected == applyFunc funcs pf "table" [g1, g2] &&
   expected == evalStateT (tableF pf [unsafeMarshall g1,unsafeMarshall g2]) []


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

prop_ReturnValueShow  :: P      -> [ExpOA]           -> Property
prop_ReturnValueMulti :: P -> P -> [NumTA]           -> Property
prop_ReturnValueMean  :: P -> P -> [NumTA]           -> Property
prop_ReturnValueDesc  :: P -> P -> [NumTA]           -> Property 
prop_ReturnValueTable :: P -> TableValidArgs -> Bool -> Property

{-|
show = show([tservice,tsalaire,trente])
tservice = table([[service]],{col:["service"]})
tsalaire = table([salaires],{col:[salaire]})
trente = table([rente],{col:[rente]})
rente = multi([0.02,moyensalaire,service])
moyensalaire = mean(salaires)
salaires = [55000,60000,45000]
service = 35

show = show([tablegoal,table(get,{col:["Depense","Septembre","Octobre"]})])
tablegoal = table(goal,{col:["Depense","objectif"]})
goal = [["Loyer","Epicerie","Resto","Alcool","Electricite","Ecole"],[510,200,100,50,40,166]]
get = [["Loyer","Epicerie","Resto","Alcool","Electricite","Ecole"],
[510,75,105,7,0,0],
[510,117,75,0,47,6]]

show = show([plotLine(x,y,{title:"MesNotes",color:"pink"}),table([x,y],{col:["X","Note"]})])
moyenne = [[mean(y)]]
x = [1,2,3,4,5,6,7]
y = [0.25,0.72,0.82,0.53,0.75,0.8,0.86]

show = show([x])
x = table(desc,{})
data = nTimes(1,1000000)
desc = descriptive(data)

show = show([table([country,gdp],{col:["pays","GDP"]}),[[mean(gdp)]],dsfdsf])
gdp = [-2,2.4,7.7,"","",-7.9,"","",0.7,1,2.4,2.9,"","","","","",2.6]
country = ["US","UK","Sweden","Spain","Portugal",
"NewZealand",
"Netherlands","Norway","Japan","Italy",
"Ireland","Greece","Germany","France","Finland",
"Denmark","Canada","Belgium","Austria","Australia"]
dsfdsf = table([[2,2],[2,2]],{})


show = show([table(means,{col:["Name"]}),table(noteTop,{col:["Nom","Note"]})])
means = [[mean(col(1,noteTop)),"z"]]
noteTop = take(10,noteSorted)
noteSorted = sortTable(1,note)
note = [name,noteExam]
name = ["Lili","Nicole","Steve","George","Bob","Leonardo","Raphael","Carey","Naomi","Catherine","Julia",
"Carolina","Madonna","Sherron","Diana"]
noteExam = [99,41,55,22,37,75,19,74,73,85,63,60,82,54,14]

-}