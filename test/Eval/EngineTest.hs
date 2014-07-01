{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Eval.EngineTest where

import Test.Framework hiding            (forAll)
                                        
import Data.Eval                        (EvalError(..),FuncEntry)
import Data.Token                       (ExpToken(..))
import Eval.Engine                      (funcs)
import Eval.Function                    (Marshallable,arrayOf,table,plot,array,obj,num,(<|>),applyFunc)
import Eval.FunctionEvalTestUtils       (Is(..),ExpTS(..),ArrayTS(..),ObjTS(..),p0,applyFunc',mkFunc)
import Parser.MonolithicParserTestUtils (P(..),ExpTA(..),NumTA(..),uns)

fs :: Marshallable a => [FuncEntry a]
fs = map noCall funcs where
  noCall (n,(typeValidators,_)) = (n,(typeValidators,\_ -> error $ "Function ["++n++"] was called erroneously"))

prop_NbArgs1 (P p) esTA = length esTA > 1 ==> let es = uns esTA in
    all (\name -> Left (InvalidNbOfArgs p name 1 0)           == applyFunc' fs p name ([] :: [ExpToken])
               && Left (InvalidNbOfArgs p name 1 (length es)) == applyFunc' fs p name es)
      ["show","multi","mean","descriptive"]

prop_NbArgs2 (P p) esTA = length esTA > 2 ==> let es = uns esTA in
    all (\name -> Left (InvalidNbOfArgs p name 2 0)           == applyFunc' fs p name ([] :: [ExpToken])
               && Left (InvalidNbOfArgs p name 2 1)           == applyFunc' fs p name (take 1 es)
               && Left (InvalidNbOfArgs p name 2 (length es)) == applyFunc' fs p name es)
      ["table","nTimes","take","sort","col"]

prop_NbArgs3 (P p) esTA = length esTA > 3 ==> let es = uns esTA in
  all (\name -> Left (InvalidNbOfArgs p name 3 0)           == applyFunc' fs p name ([] :: [ExpToken])
             && Left (InvalidNbOfArgs p name 3 1)           == applyFunc' fs p name (take 1 es)
             && Left (InvalidNbOfArgs p name 3 2)           == applyFunc' fs p name (take 2 es)
             && Left (InvalidNbOfArgs p name 3 (length es)) == applyFunc' fs p name es)
    ["plotLine"]

mk ts = let es = uns ts; arg = ArrayT p0 ("","") es in (es,arg)

prop_ErrorTypeShow  (P p) w1as (ExpTS w1') = let (w1s,w1) = mk w1as in not (null w1s) && not (isArray w1') ==>  
  arrayOf (table fs <|> plot fs) w1  == applyFunc fs (mkFunc p "show" [w1]) && 
  arrayOf (table fs <|> plot fs) w1' == applyFunc fs (mkFunc p "show" [w1'])
  
prop_ErrorTypeMulti (P p) w1as (ExpTS w1') = let (w1s,w1) = mk w1as in any (not.isNum) w1s && not (isArray w1') ==>  
  arrayOf num w1  == applyFunc fs (mkFunc p "multi" [w1]) && 
  arrayOf num w1' == applyFunc fs (mkFunc p "multi" [w1'])
  
prop_ErrorTypeMean (P p) w1as (ExpTS w1') = let (w1s,w1) = mk w1as in any (not.isNum) w1s && not (isArray w1') ==>  
  arrayOf num w1  == applyFunc fs (mkFunc p "mean" [w1]) && 
  arrayOf num w1' == applyFunc fs (mkFunc p "mean" [w1'])
  
prop_ErrorTypeDesc (P p) w1as (ExpTS w1') = let (w1s,w1) = mk w1as in any (not.isNum) w1s && not (isArray w1') ==>  
  arrayOf num w1  == applyFunc fs (mkFunc p "descriptive" [w1]) && 
  arrayOf num w1' == applyFunc fs (mkFunc p "descriptive" [w1'])

prop_ErrorTypeTable (P p) g1ass (ObjTS g2) w1ass (ExpTS w1') (ExpTS w2) = 
  let (_,g1) = mk g1ass; (w1ss,w1) = mk w1ass; in any (not.isArray) w1ss && not (isArray w1') && not (isObj w2) ==>
    arrayOf (array fs) w1  == applyFunc fs (mkFunc p "table" [w1 ,g2]) &&
    arrayOf (array fs) w1' == applyFunc fs (mkFunc p "table" [w1',w2]) &&
    obj fs w2              == applyFunc fs (mkFunc p "table" [g1 ,w2])

prop_ErrorTypeNTimes (P p) (NumTA _ g1) (NumTA _ g2) (ExpTS w1) (ExpTS w2) = not (isNum w1) && not (isNum w2) ==>
  num w1 == applyFunc fs (mkFunc p "nTimes" [w1,g2]) &&
  num w1 == applyFunc fs (mkFunc p "nTimes" [w1,w2]) &&
  num w2 == applyFunc fs (mkFunc p "nTimes" [g1,w2]) 

prop_ErrorTypeTake (P p) (NumTA _ g1) (ArrayTS g2) (ExpTS w1) (ExpTS w2) = not (isNum w1) && not (isArray w2) ==>
   num w1                    == applyFunc fs (mkFunc p "take" [w1,g2]) &&
   num w1                    == applyFunc fs (mkFunc p "take" [w1,w2]) &&
  (table fs <|> array fs) w2 == applyFunc fs (mkFunc p "take" [g1,w2])

prop_ErrorTypeSort (P p) (NumTA _ g1) g2as (ExpTS w1) w2as (ExpTS w2') = 
  let (_,g2) = mk g2as; (w2s,w2) = mk w2as in not (isNum w1) && any (not.isArray) w2s && not (isArray w2') ==>
     num w1                               == applyFunc fs (mkFunc p "sort" [w1,g2]) &&
     num w1                               == applyFunc fs (mkFunc p "sort" [w1,w2]) &&
     num w1                               == applyFunc fs (mkFunc p "sort" [w1,w2']) &&
    (table fs <|> arrayOf (array fs)) w2  == applyFunc fs (mkFunc p "sort" [g1,w2]) &&
    (table fs <|> arrayOf (array fs)) w2' == applyFunc fs (mkFunc p "sort" [g1,w2'])

prop_ErrorTypeCol (P p) (NumTA _ g1) g2ass (ExpTS w1) w2as (ExpTS w2') = 
  let (_,g2) = mk g2ass; (w2s,w2) = mk w2as in not (isNum w1) && any (not.isArray) w2s && not (isArray w2') ==>
     num w1                               == applyFunc fs (mkFunc p "col" [w1,g2]) &&
     num w1                               == applyFunc fs (mkFunc p "col" [w1,w2]) &&
     num w1                               == applyFunc fs (mkFunc p "col" [w1,w2']) &&
    (table fs <|> arrayOf (array fs)) w2  == applyFunc fs (mkFunc p "col" [g1,w2]) &&
    (table fs <|> arrayOf (array fs)) w2' == applyFunc fs (mkFunc p "col" [g1,w2'])

prop_ErrorTypePlotLine (P p) g1as g2as (ObjTS g3) w1as (ExpTS w1') w2as (ExpTS w2') (ExpTS w3) = 
  let (_,g1) = mk g1as; (_,g2) = mk g2as; (w1s,w1) = mk w1as; (w2s,w2) = mk w2as 
  in any (not.isNum) w1s && not (isArray w1') && any (not.isNum) w2s && not (isArray w2') && not (isObj w3) ==>
    arrayOf num w1  == applyFunc fs (mkFunc p "plotLine" [w1 ,g2 ,g3]) &&
    arrayOf num w1  == applyFunc fs (mkFunc p "plotLine" [w1 ,w2 ,g3]) &&
    arrayOf num w1  == applyFunc fs (mkFunc p "plotLine" [w1 ,w2',g3]) &&
    arrayOf num w1  == applyFunc fs (mkFunc p "plotLine" [w1 ,g2 ,w3]) &&
    arrayOf num w1' == applyFunc fs (mkFunc p "plotLine" [w1',g2 ,g3]) &&
    arrayOf num w1' == applyFunc fs (mkFunc p "plotLine" [w1',w2 ,g3]) &&
    arrayOf num w1' == applyFunc fs (mkFunc p "plotLine" [w1',w2',g3]) &&
    arrayOf num w1' == applyFunc fs (mkFunc p "plotLine" [w1',g2 ,w3]) &&
    arrayOf num w2  == applyFunc fs (mkFunc p "plotLine" [g1 ,w2 ,g3]) &&
    arrayOf num w2  == applyFunc fs (mkFunc p "plotLine" [g1 ,w2 ,w3]) &&
    arrayOf num w2' == applyFunc fs (mkFunc p "plotLine" [g1 ,w2',g3]) &&
    arrayOf num w2' == applyFunc fs (mkFunc p "plotLine" [g1 ,w2',w3]) &&
    obj fs w3       == applyFunc fs (mkFunc p "plotLine" [g1 ,g2 ,w3])

{-| Mandatory type signatures -}
type NbArgTest = P -> [ExpTA] ->  Property
prop_NbArgs1 :: NbArgTest 
prop_NbArgs2 :: NbArgTest 
prop_NbArgs3 :: NbArgTest 

prop_ErrorTypeShow  :: P -> [ExpTS] -> ExpTS -> Property
prop_ErrorTypeMulti :: P -> [ExpTS] -> ExpTS -> Property
prop_ErrorTypeMean  :: P -> [ExpTS] -> ExpTS -> Property
prop_ErrorTypeDesc  :: P -> [ExpTS] -> ExpTS -> Property

prop_ErrorTypeTable  :: P -> [ArrayTS] ->   ObjTS   -> [ExpTS] ->  ExpTS  -> ExpTS -> Property
prop_ErrorTypeNTimes :: P ->  NumTA    ->   NumTA   ->  ExpTS  ->  ExpTS           -> Property
prop_ErrorTypeTake   :: P ->  NumTA    ->   ArrayTS ->  ExpTS  ->  ExpTS           -> Property
prop_ErrorTypeSort   :: P ->  NumTA    -> [ArrayTS] ->  ExpTS  -> [ExpTS] -> ExpTS -> Property
prop_ErrorTypeCol    :: P ->  NumTA    -> [ArrayTS] ->  ExpTS  -> [ExpTS] -> ExpTS -> Property

prop_ErrorTypePlotLine :: P -> [NumTA] -> [NumTA]   -> ObjTS   -> [ExpTS] -> ExpTS -> [ExpTS] -> ExpTS -> ExpTS -> Property

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