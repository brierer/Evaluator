module Eval.Function where

import Data.Eval
import Data.Token

funcs :: [(String,       ([TypeValidator],      Func))]
funcs =   -- 1 arg functions
        [ ("show",       ([list showable], showF))
        , ("multi",      ([list num],      multiF))
        , ("mean",       ([list num],      meanF))
        , ("descriptive",([list num],      descF))
          -- 2 arg functions
        , ("table",      ([matrix,obj],    tableF))
        , ("nTimes",     ([num,num],       nTimesF))
        , ("take",       ([num,matrix],    takeF))
        , ("sortTable",  ([num,matrix],    sortTableF))
          -- 3 arg functions
        , ("plotLine",   ([array,array,obj],   plotLineF))
        ]

{-| Validation combinators -}
list :: TypeValidator -> TypeValidator
list _ _ = error "Eval.Function::list   [Not Implemented]"

or :: TypeValidator -> TypeValidator -> TypeValidator
or _ _ _ = error "Eval.Function::or   [Not Implemented]"

{-| Validation functions -}
showable :: TypeValidator
table    :: TypeValidator
plot     :: TypeValidator
matrix   :: TypeValidator
array    :: TypeValidator
obj      :: TypeValidator
str      :: TypeValidator
num      :: TypeValidator
bool     :: TypeValidator
null     :: TypeValidator

showable _ = error "Eval.Function::showable [Not Implemented]"
table _    = error "Eval.Function::table [Not Implemented]"
plot _     = error "Eval.Function::plot [Not Implemented]"
matrix _   = error "Eval.Function::matrix [Not Implemented]"
array _    = error "Eval.Function::array [Not Implemented]"
obj _      = error "Eval.Function::obj [Not Implemented]"
str _      = error "Eval.Function::str [Not Implemented]"
num _      = error "Eval.Function::num [Not Implemented]"
bool _     = error "Eval.Function::bool [Not Implemented]"

null (NullT _ _) = return NullObj
null e           = typeMismatch e "Null"

typeMismatch :: ExpToken -> String -> Eval Obj
typeMismatch e = Left . TypeMismatch (getP e) (getT e)

{-| Funcs -}
showF      :: Func
multiF     :: Func
meanF      :: Func
descF      :: Func
tableF     :: Func
nTimesF    :: Func
takeF      :: Func
sortTableF :: Func
plotLineF  :: Func

showF _      = error "Eval.Function::showF  [Not Implemented]"
multiF _     = error "Eval.Function::multiF [Not Implemented]"
meanF _      = error "Eval.Function::meanF  [Not Implemented]"
descF _      = error "Eval.Function::descF  [Not Implemented]"
tableF _     = error "Eval.Function::tableF     [Not Implemented]"
nTimesF _    = error "Eval.Function::ntimesF    [Not Implemented]"
takeF _      = error "Eval.Function::takeF      [Not Implemented]"
sortTableF _ = error "Eval.Function::sortTableF [Not Implemented]"
plotLineF _  = error "Eval.Function::plotLineF [Not Implemented]"


getP :: ExpToken -> Pos
getP (ArrayT p _ _) = p
getP (ObjT p _ _)   = p
getP (StrT p _ _)   = p
getP (NumT p _ _ _) = p
getP (BoolT p _ _)  = p
getP (NullT p _)    = p
getP e              = error $ "Eval.Function::getP [Failed pattern match ["++show e++"]]"

getT :: ExpToken -> String
getT (ArrayT{}) = head types
getT (ObjT{})   = types !! 1
getT (StrT{})   = types !! 2
getT (NumT{})   = types !! 3
getT (BoolT{})  = types !! 4
getT (NullT{})  = types !! 5
getT e          = error $ "Eval.Function::getT [Failed pattern match ["++show e++"]]"

types :: [String]
types = ["Array","Object","String","Number","Boolean","Null"]


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