{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Eval.EngineTest where

import Test.Framework hiding            (forAll)
                                        
import Data.Eval                        (EvalError(..))
import Data.Token                       (IdToken(..),ExpToken(..))
import Eval.Engine                      (funcs)
import Eval.Function                    (arrayOf,table,plot,(<|>),applyFunc)
import Eval.FunctionEvalTestUtils       (ExpTS(..),applyFunc')
import Parser.MonolithicParserTestUtils (W(..),P(..),ExpTA(..),un,uns)

prop_NbArgs1 (P p) esTA = length esTA > 1 ==> let es = uns esTA in
  all (\name -> Left (InvalidNbOfArgs p name 1 0)           == applyFunc' funcs p name []
             && Left (InvalidNbOfArgs p name 1 (length es)) == applyFunc' funcs p name es)
    ["show","multi","mean","descriptive"]

prop_NbArgs2 (P p) esTA = length esTA > 2 ==> let es = uns esTA in
  all (\name -> Left (InvalidNbOfArgs p name 2 0)           == applyFunc' funcs p name []
             && Left (InvalidNbOfArgs p name 2 1)           == applyFunc' funcs p name (take 1 es)
             && Left (InvalidNbOfArgs p name 2 (length es)) == applyFunc' funcs p name es)
    ["table","nTimes","take","sortTable"]

prop_NbArgs3 (P p) esTA = length esTA > 3 ==> let es = uns esTA in
  all (\name -> Left (InvalidNbOfArgs p name 3 0)           == applyFunc' funcs p name []
             && Left (InvalidNbOfArgs p name 3 1)           == applyFunc' funcs p name (take 1 es)
             && Left (InvalidNbOfArgs p name 3 2)           == applyFunc' funcs p name (take 2 es)
             && Left (InvalidNbOfArgs p name 3 (length es)) == applyFunc' funcs p name es)
    ["plotLine"]

type Test = P -> [ExpTA] -> Property
prop_NbArgs1 :: Test
prop_NbArgs2 :: Test
prop_NbArgs3 :: Test

--prop_ErrorTypeShowLit :: P -> P -> P -> W -> (W,W) -> (W,W) -> [ExpTS] -> Property
--prop_ErrorTypeShowLit pf pi pa wf wi wa es = not (null es) ==> 
--  let arg = ArrayT (un pa) (un wa) (uns es)
--      expected = arrayOf (table funcs <|> plot funcs) arg
--      actual = applyFunc funcs (FuncT (un pf) (un wf) (IdT (un pi) (un wi) "show") [arg])
--  in  expected == actual
    
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