{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Eval.EngineTest where

import qualified Eval.EngineTestUtils as E (fs)

import Control.Monad.State                 (evalStateT)
import Data.Eval                           (EvalError(..),ExpObj(..))
import Data.List                           (genericLength)
import Data.Token                          (ExpToken(..))
import Eval.Engine                         (funcs,showF,multiF,meanF)
import Eval.EngineTestUtils                (addFunc,addFunc',mk,mk',mkO',oneArrayOfNum,success,toArray,tablesAndPlots,emptyArray,emptySortColCase,mkMultiMean,unprecise)
import Eval.Function                       (table,plot,array,obj,num,arrayOf,nonEmpty,(<|>),withFuncs)
import Eval.FunctionEvalTestUtils          (Is(..),ExpOA(..),TableOA(..),NumOA(..),ExpTS(..),ArrayTS(..),ObjTS(..),applyFunc)
import Parser.MonolithicParserTestUtils    (P(..),ExpTA(..),NumTA(..),to,uns)
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

prop_TypeMismatchTable (P p) g1ass (ObjTS g2) w1ass (ExpTS w1') (ExpTS w2) =
  let (g1ss,g1) = mk' g1ass; (w1ss,w1) = mk' w1ass; in any (not.emptyArray) g1ss && any (not.isArray) w1ss && not (isArray w1') && not (isObj w2) ==>
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

prop_TypeMismatchPlotLine (P p) g1as g2as (ObjTS g3) w1as (ExpTS w1') w2as (ExpTS w2') (ExpTS w3) =
  let (_,g1) = mk' g1as; (_,g2) = mk' g2as; (w1s,w1) = mk' w1as; (w2s,w2) = mk' w2as
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

prop_EmptyArgMulti (P pn) (P pa) g1as = not (null g1as) ==>
  let (_,g1) = mk' g1as; (_,w1) = mk pa ([] :: [NumTA])
  in  Left (IllegalEmpty pa)                     == applyFunc E.fs pn "multi" [w1] &&
      withFuncs E.fs (nonEmpty $ arrayOf num) w1 == applyFunc E.fs pn "multi" [w1] &&
      success "multi"                            == applyFunc E.fs pn "multi" [g1]

prop_EmptyArgMean (P pn) (P pa) g1as = not (null g1as) ==>
  let (_,g1) = mk' g1as; (_,w1) = mk pa ([] :: [NumTA])
  in  Left (IllegalEmpty pa) == applyFunc E.fs pn "mean" [w1] &&
      success "mean"         == applyFunc E.fs pn "mean" [g1]

prop_EmptyArgDesc (P pn) (P pa) g1as = not (null g1as) ==>
  let (_,g1) = mk' g1as; (_,w1) = mk pa ([] :: [NumTA])
  in  Left (IllegalEmpty pa) == applyFunc E.fs pn "descriptive" [w1] &&
      success "descriptive"  == applyFunc E.fs pn "descriptive" [g1]

prop_EmptyArgTable (P pt) (P pa) g1ass w1'ass (ObjTS o) =
  let (g1ss,g1) = mk' g1ass; (_,w1) = mk pa ([] :: [ExpTS]); (_,w1') = mk' (w1'ass ++ [to w1]); in any (not.emptyArray) g1ss ==>
    Left (IllegalEmpty pa) == applyFunc E.fs pt "table" [w1 , o] &&
    Left (IllegalEmpty pa) == applyFunc E.fs pt "table" [w1', o] &&
    success "table"        == applyFunc E.fs pt "table" [g1 , o]

prop_EmptyArgSort (P pt) (P pa) (NumTA _ n) = emptySortColCase "sort" pa pt n
prop_EmptyArgCol  (P pt) (P pa) (NumTA _ n) = emptySortColCase "col"  pa pt n

prop_ReturnValueShow (P p) a1ras' = let (fs,a1) = addFunc' "tablesAndPlots" a1r; (_,a1r) = mkO' a1rs; a1rs = tablesAndPlots a1ras'; expected = Right (ObjO p [("result",a1r)])
                                    in  True ==> expected == applyFunc fs p "show" [a1] && expected == evalStateT (showF p [a1r]) []

prop_ReturnValueMulti (P pn) (P pa) a1as = not (null a1as) ==>
  let (a1,a1rs,a1r) = mkMultiMean a1as pa; expected = Right $ NumO pn $ product $ map (\(NumO _ x)->x) a1rs in  not (null a1as) ==>
      expected == applyFunc funcs pn "multi" [a1]  &&
      expected == evalStateT (multiF pn [a1r]) []

prop_ReturnValueMean  (P pn) (P pa) a1as = not (null a1as) ==>
  let (a1,a1rs,a1r) = mkMultiMean a1as pa; expected = Right $ NumO pn $ sum (map (\(NumO _ x)->x) a1rs) / genericLength a1rs in  not (null a1as) ==>
      unprecise expected == unprecise (applyFunc funcs pn "mean" [a1])  &&
      unprecise expected == unprecise (evalStateT (meanF pn [a1r]) [])

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

prop_EmptyArgMulti :: P -> P -> [NumTA]                         -> Property
prop_EmptyArgMean  :: P -> P -> [NumTA]                         -> Property
prop_EmptyArgDesc  :: P -> P -> [NumTA]                         -> Property
prop_EmptyArgTable :: P -> P -> [ArrayTS] -> [ArrayTS] -> ObjTS -> Property
prop_EmptyArgSort  :: P -> P -> NumTA -> [ArrayTS] -> [ArrayTS] -> Property
prop_EmptyArgCol   :: P -> P -> NumTA -> [ArrayTS] -> [ArrayTS] -> Property

prop_ReturnValueShow  :: P      -> [ExpOA] -> Property
prop_ReturnValueMulti :: P -> P -> [NumTA] -> Property
prop_ReturnValueMean  :: P -> P -> [NumTA] -> Property

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