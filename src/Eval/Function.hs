module Eval.Function where

import Eval.MultiPass (Eval,EvalError(System))

data Obj = Unimplemented
           deriving (Show)

type OneValidArg = Obj -> Eval ()
type OneArg      = Obj -> Eval Obj

type TwoValidArgs = Obj -> Obj -> Eval ()
type TwoArgs      = Obj -> Obj -> Eval Obj

type ThreeValidArgs = Obj -> Obj -> Obj -> Eval ()
type ThreeArgs      = Obj -> Obj -> Obj -> Eval Obj

oneArgFuncs :: [(String,       OneValidArg,   OneArg)]
oneArgFuncs = [ ("show",       isShowableList,showF)
              , ("multi",      isNumList,     multiF)
              , ("mean",       isNumList,     meanF)
              , ("descriptive",isNumList,     descF)
              ]

twoArgsFuncs :: [(String,     TwoValidArgs,  TwoArgs)]
twoArgsFuncs = [ ("table",    isTableArgs,   tableF)
               , ("nTimes",   isTwoNums,     nTimesF)
               , ("take",     isNumAndTable, takeF)
               , ("sortTable",isNumAndTable, sortTableF)
               ]
threeArgsFuncs :: [(String,     ThreeValidArgs, ThreeArgs)]
threeArgsFuncs = [ ("plotLine", isPlotLineArgs, plotLineF)
                 ]

{-| Validation funcs -}
isShowableList :: OneValidArg
isNumList      :: OneValidArg

isShowableList _ = Left $ System "Eval.Function::isShowableList [Not Implemented]"
isNumList _      = Left $ System "Eval.Function::isNumList      [Not Implemented]"

isTableArgs   :: TwoValidArgs
isTwoNums     :: TwoValidArgs
isNumAndTable :: TwoValidArgs

isTableArgs _ _   = Left $ System "Eval.Function::isTableArgs   [Not Implemented]"
isTwoNums _ _     = Left $ System "Eval.Function::isTwoNums     [Not Implemented]"
isNumAndTable _ _ = Left $ System "Eval.Function::isNumAndTable [Not Implemented]"

isPlotLineArgs :: ThreeValidArgs
isPlotLineArgs _ _ _ = Left $ System "Eval.Function::isPlotLineArgs [Not Implemented]"

{-| Funcs -}
showF  :: OneArg
multiF :: OneArg
meanF  :: OneArg
descF  :: OneArg

showF _  = Left $ System "Eval.Function::showF  [Not Implemented]"
multiF _ = Left $ System "Eval.Function::multiF [Not Implemented]"
meanF _  = Left $ System "Eval.Function::meanF  [Not Implemented]"
descF _  = Left $ System "Eval.Function::descF  [Not Implemented]"

tableF     :: TwoArgs
nTimesF    :: TwoArgs
takeF      :: TwoArgs
sortTableF :: TwoArgs

tableF _ _     = Left $ System "Eval.Function::tableF     [Not Implemented]"
nTimesF _ _    = Left $ System "Eval.Function::ntimesF    [Not Implemented]"
takeF _ _      = Left $ System "Eval.Function::takeF      [Not Implemented]"
sortTableF _ _ = Left $ System "Eval.Function::sortTableF [Not Implemented]"

plotLineF :: ThreeArgs
plotLineF _ _ _ = Left $ System "Eval.Function::plotLineF [Not Implemented]"

{-|

show = show([tservice,tsalaire,trente])
tservice = table([[service]],{col:["service"]})
tsalaire = table([salaires],{col:["salaire"]})
trente = table([rente],{col:["rente"]})
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