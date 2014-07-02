{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Eval.EngineTest where

import Test.Framework hiding               (forAll)
import qualified Eval.EngineTestUtils as E (fs)

import Data.Eval                           (EvalError(..),ExpObj(..),valFunc)
import Data.Token                          (ExpToken(..))
import Eval.EngineTestUtils                (addFunc,mk,oneArrayOfNum,success)
import Eval.Function                       (Marshallable,arrayOf,table,plot,array,obj,num,(<|>),applyFunc,withFuncs)
import Eval.FunctionEvalTestUtils          (Is(..),ExpOA(..),TableOA(..),ExpTS(..),ArrayTS(..),ObjTS(..),applyFunc',mkFunc)
import Parser.MonolithicParserTestUtils    (P(..),ExpTA(..),NumTA(..),uns)

prop_NbArgs1 (P p) esTA = length esTA > 1 ==> let es = uns esTA in
    all (\name -> Left (InvalidNbOfArgs p name 1 0)           == applyFunc' E.fs p name ([] :: [ExpToken])
               && Left (InvalidNbOfArgs p name 1 (length es)) == applyFunc' E.fs p name es)
      ["show","multi","mean","descriptive"]

prop_NbArgs2 (P p) esTA = length esTA > 2 ==> let es = uns esTA in
    all (\name -> Left (InvalidNbOfArgs p name 2 0)           == applyFunc' E.fs p name ([] :: [ExpToken])
               && Left (InvalidNbOfArgs p name 2 1)           == applyFunc' E.fs p name (take 1 es)
               && Left (InvalidNbOfArgs p name 2 (length es)) == applyFunc' E.fs p name es)
      ["table","nTimes","take","sort","col"]

prop_NbArgs3 (P p) esTA = length esTA > 3 ==> let es = uns esTA in
  all (\name -> Left (InvalidNbOfArgs p name 3 0)           == applyFunc' E.fs p name ([] :: [ExpToken])
             && Left (InvalidNbOfArgs p name 3 1)           == applyFunc' E.fs p name (take 1 es)
             && Left (InvalidNbOfArgs p name 3 2)           == applyFunc' E.fs p name (take 2 es)
             && Left (InvalidNbOfArgs p name 3 (length es)) == applyFunc' E.fs p name es)
    ["plotLine"]

prop_TypeErrorShow  (P p) (ExpOA g1r) w1as (ExpTS w1') =
  let (fs,g1) = addFunc "tableOrPlot" g1r; (w1s,w1) = mk w1as in (isTable g1r || isPlot g1r) && not (null w1s) && not (isArray w1') ==>
    withFuncs fs (arrayOf $ table <|> plot) w1  == withFuncs fs applyFunc (mkFunc p "show" [w1]) &&
    withFuncs fs (arrayOf $ table <|> plot) w1' == withFuncs fs applyFunc (mkFunc p "show" [w1']) 
--    &&
--    success "show"                     == applyFunc fs (mkFunc p "show" [g1])

prop_TypeErrorMulti (P p) g1ras w1as (ExpTS w1') =
  let (fs,g1,w1s,w1) = oneArrayOfNum g1ras w1as in any (not.isNum) w1s && not (isArray w1') ==>
    withFuncs fs (arrayOf num) w1  == withFuncs fs applyFunc (mkFunc p "multi" [w1]) &&
    withFuncs fs (arrayOf num) w1' == withFuncs fs applyFunc (mkFunc p "multi" [w1']) 
--    &&
--    success "multi" == applyFunc fs (mkFunc p "multi" [g1])

prop_TypeErrorMean (P p) g1ras w1as (ExpTS w1') =
  let (fs,g1,w1s,w1) = oneArrayOfNum g1ras w1as in any (not.isNum) w1s && not (isArray w1') ==>
    withFuncs fs (arrayOf num) w1  == withFuncs fs applyFunc (mkFunc p "mean" [w1]) &&
    withFuncs fs (arrayOf num) w1' == withFuncs fs applyFunc (mkFunc p "mean" [w1']) 
--    &&
--    success "mean"  == applyFunc fs (mkFunc p "mean" [g1])

prop_TypeErrorDesc (P p) g1ras w1as (ExpTS w1') =
  let (fs,g1,w1s,w1) = oneArrayOfNum g1ras w1as in any (not.isNum) w1s && not (isArray w1') ==>
    withFuncs fs (arrayOf num) w1        == withFuncs fs applyFunc (mkFunc p "descriptive" [w1]) &&
    withFuncs fs (arrayOf num) w1'       == withFuncs fs applyFunc (mkFunc p "descriptive" [w1'])
--     &&
--    success "descriptive" == applyFunc fs (mkFunc p "descriptive" [g1])

prop_TypeErrorTable (P p) g1ass (ObjTS g2) w1ass (ExpTS w1') (ExpTS w2) =
  let (_,g1) = mk g1ass; (w1ss,w1) = mk w1ass; in any (not.isArray) w1ss && not (isArray w1') && not (isObj w2) ==>
    withFuncs E.fs (arrayOf array) w1  == withFuncs E.fs applyFunc (mkFunc p "table" [w1 ,g2]) &&
    withFuncs E.fs (arrayOf array) w1' == withFuncs E.fs applyFunc (mkFunc p "table" [w1',w2]) &&
    withFuncs E.fs  obj            w2  == withFuncs E.fs applyFunc (mkFunc p "table" [g1 ,w2]) 
--    &&
--    success "table"          == applyFunc E.fs (mkFunc p "table" [g1 ,g2])

prop_TypeErrorNTimes (P p) (NumTA _ g1) (NumTA _ g2) (ExpTS w1) (ExpTS w2) = not (isNum w1) && not (isNum w2) ==>
  withFuncs E.fs num w1           == withFuncs E.fs applyFunc (mkFunc p "nTimes" [w1,g2]) &&
  withFuncs E.fs num w1           == withFuncs E.fs applyFunc (mkFunc p "nTimes" [w1,w2]) &&
  withFuncs E.fs num w2           == withFuncs E.fs applyFunc (mkFunc p "nTimes" [g1,w2]) 
--  &&
--  success "nTimes" == applyFunc E.fs (mkFunc p "nTimes" [g1,g2])

prop_TypeErrorTake (P p) (NumTA _ g1) (ArrayTS g2) (TableOA g2r) (ExpTS w1) (ExpTS w2) = not (isNum w1) && not (isArray w2) ==>
  let (fs,g2') = addFunc "table" g2r in
  withFuncs fs  num              w1 == withFuncs fs applyFunc (mkFunc p "take" [w1,g2])  &&
  withFuncs fs  num              w1 == withFuncs fs applyFunc (mkFunc p "take" [w1,g2']) &&
  withFuncs fs  num              w1 == withFuncs fs applyFunc (mkFunc p "take" [w1,w2])  &&
  withFuncs fs (table <|> array) w2 == withFuncs fs applyFunc (mkFunc p "take" [g1,w2])  
--  &&
--   success "take"                   == applyFunc fs (mkFunc p "take" [g1,g2])  &&
--   success "take"            == applyFunc fs (mkFunc p "take" [g1,g2'])

prop_TypeErrorSort (P p) (NumTA _ g1) g2as (TableOA g2r) (ExpTS w1) w2as (ExpTS w2') =
  let (_,g2) = mk g2as; (w2s,w2) = mk w2as; (fs,g2') = addFunc "table" g2r in not (isNum w1) && any (not.isArray) w2s && not (isArray w2') ==>
    withFuncs fs  num                      w1  == withFuncs fs applyFunc (mkFunc p "sort" [w1,g2])  &&
    withFuncs fs  num                      w1  == withFuncs fs applyFunc (mkFunc p "sort" [w1,g2']) &&
    withFuncs fs  num                      w1  == withFuncs fs applyFunc (mkFunc p "sort" [w1,w2])  &&
    withFuncs fs  num                      w1  == withFuncs fs applyFunc (mkFunc p "sort" [w1,w2']) &&
    withFuncs fs (table <|> arrayOf array) w2  == withFuncs fs applyFunc (mkFunc p "sort" [g1,w2])  &&
    withFuncs fs (table <|> arrayOf array) w2' == withFuncs fs applyFunc (mkFunc p "sort" [g1,w2']) 
--    &&
--     success "sort"                       == applyFunc fs (mkFunc p "sort" [g1,g2]) &&
--     success "sort"                       == applyFunc fs (mkFunc p "sort" [g1,g2'])

prop_TypeErrorCol (P p) (NumTA _ g1) g2ass (TableOA g2r) (ExpTS w1) w2as (ExpTS w2') =
  let (_,g2) = mk g2ass; (w2s,w2) = mk w2as; (fs,g2') = addFunc "table" g2r in not (isNum w1) && any (not.isArray) w2s && not (isArray w2') ==>
    withFuncs fs  num                      w1  == withFuncs fs applyFunc (mkFunc p "col" [w1,g2])  &&
    withFuncs fs  num                      w1  == withFuncs fs applyFunc (mkFunc p "col" [w1,g2']) &&
    withFuncs fs  num                      w1  == withFuncs fs applyFunc (mkFunc p "col" [w1,w2])  &&
    withFuncs fs  num                      w1  == withFuncs fs applyFunc (mkFunc p "col" [w1,w2']) &&
    withFuncs fs (table <|> arrayOf array) w2  == withFuncs fs applyFunc (mkFunc p "col" [g1,w2])  &&
    withFuncs fs (table <|> arrayOf array) w2' == withFuncs fs applyFunc (mkFunc p "col" [g1,w2']) 
--    &&
--     success "col"                        == applyFunc fs (mkFunc p "col" [g1,g2]) &&
--     success "col"                        == applyFunc fs (mkFunc p "col" [g1,g2'])

prop_TypeErrorPlotLine (P p) g1as g2as (ObjTS g3) w1as (ExpTS w1') w2as (ExpTS w2') (ExpTS w3) =
  let (_,g1) = mk g1as; (_,g2) = mk g2as; (w1s,w1) = mk w1as; (w2s,w2) = mk w2as
  in any (not.isNum) w1s && not (isArray w1') && any (not.isNum) w2s && not (isArray w2') && not (isObj w3) ==>
    withFuncs E.fs (arrayOf num) w1     == withFuncs E.fs applyFunc (mkFunc p "plotLine" [w1 ,g2 ,g3]) &&
    withFuncs E.fs (arrayOf num) w1     == withFuncs E.fs applyFunc (mkFunc p "plotLine" [w1 ,w2 ,g3]) &&
    withFuncs E.fs (arrayOf num) w1     == withFuncs E.fs applyFunc (mkFunc p "plotLine" [w1 ,w2',g3]) &&
    withFuncs E.fs (arrayOf num) w1     == withFuncs E.fs applyFunc (mkFunc p "plotLine" [w1 ,g2 ,w3]) &&
    withFuncs E.fs (arrayOf num) w1'    == withFuncs E.fs applyFunc (mkFunc p "plotLine" [w1',g2 ,g3]) &&
    withFuncs E.fs (arrayOf num) w1'    == withFuncs E.fs applyFunc (mkFunc p "plotLine" [w1',w2 ,g3]) &&
    withFuncs E.fs (arrayOf num) w1'    == withFuncs E.fs applyFunc (mkFunc p "plotLine" [w1',w2',g3]) &&
    withFuncs E.fs (arrayOf num) w1'    == withFuncs E.fs applyFunc (mkFunc p "plotLine" [w1',g2 ,w3]) &&
    withFuncs E.fs (arrayOf num) w2     == withFuncs E.fs applyFunc (mkFunc p "plotLine" [g1 ,w2 ,g3]) &&
    withFuncs E.fs (arrayOf num) w2     == withFuncs E.fs applyFunc (mkFunc p "plotLine" [g1 ,w2 ,w3]) &&
    withFuncs E.fs (arrayOf num) w2'    == withFuncs E.fs applyFunc (mkFunc p "plotLine" [g1 ,w2',g3]) &&
    withFuncs E.fs (arrayOf num) w2'    == withFuncs E.fs applyFunc (mkFunc p "plotLine" [g1 ,w2',w3]) &&
    withFuncs E.fs  obj          w3     == withFuncs E.fs applyFunc (mkFunc p "plotLine" [g1 ,g2 ,w3]) 
--    &&
--    success "plotLine" == applyFunc E.fs (mkFunc p "plotLine" [g1 ,g2 ,g3])

{-| Mandatory type signatures -}
prop_NbArgs1 :: P -> [ExpTA] ->  Property
prop_NbArgs2 :: P -> [ExpTA] ->  Property
prop_NbArgs3 :: P -> [ExpTA] ->  Property

prop_TypeErrorShow  :: P ->  ExpOA  -> [ExpTS] -> ExpTS -> Property
prop_TypeErrorMulti :: P -> [ExpOA] -> [ExpTS] -> ExpTS  -> Property
prop_TypeErrorMean  :: P -> [ExpOA] -> [ExpTS] -> ExpTS  -> Property
prop_TypeErrorDesc  :: P -> [ExpOA] -> [ExpTS] -> ExpTS  -> Property

prop_TypeErrorTable  :: P -> [ArrayTS] ->   ObjTS   -> [ExpTS] ->  ExpTS  ->  ExpTS           -> Property
prop_TypeErrorNTimes :: P ->  NumTA    ->   NumTA   ->  ExpTS  ->  ExpTS                      -> Property
prop_TypeErrorTake   :: P ->  NumTA    ->  ArrayTS  -> TableOA ->  ExpTS  ->  ExpTS           -> Property
prop_TypeErrorSort   :: P ->  NumTA    -> [ArrayTS] -> TableOA ->  ExpTS  -> [ExpTS] -> ExpTS -> Property
prop_TypeErrorCol    :: P ->  NumTA    -> [ArrayTS] -> TableOA ->  ExpTS  -> [ExpTS] -> ExpTS -> Property

prop_TypeErrorPlotLine :: P -> [NumTA] -> [NumTA]   -> ObjTS   -> [ExpTS] -> ExpTS -> [ExpTS] -> ExpTS -> ExpTS -> Property

{-|
unlines $  [ "show = show([tservice,tsalaire,trente])" , "tservice = table([[service]],{col:[\"service\"]})" , "tsalaire = table([salaires],{col:[\"salaire\"]})" , "trente = table([rente],{col:[\"rente\"]})" , "rente = multi([0.02,moyensalaire,service])" , "moyensalaire = mean(salaires)" , "salaires = [55000,60000,45000]" , "service = 35" ]


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