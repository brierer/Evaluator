module Eval.Function where

import Prelude hiding (exp)

import Control.Monad  (liftM2)
import Data.Eval      (ExpObj(..),EvalError(TypeMismatch),Eval,TypeValidator,Func)
import Data.Token     (PairToken(..),IdToken(..),ExpToken(..),Pos)

funcs :: Marshallable a => 
        [(String,       ([TypeValidator a],Func))]
funcs =   -- 1 arg functions
        [ ("show",       ([array], showF))
        , ("multi",      ([array], multiF))
        , ("mean",       ([array], meanF))
        , ("descriptive",([array], descF))
          -- 2 arg functions
        , ("table",      ([matrix,obj], tableF))
        , ("nTimes",     ([num,num],    nTimesF))
        , ("take",       ([num,matrix], takeF))
        , ("sortTable",  ([num,matrix], sortTableF))
          -- 3 arg functions
        , ("plotLine",   ([array,array,obj],plotLineF))
        ]

{-| Validation functions and combinators -}
class Marshallable a where
  exp      :: TypeValidator a
  showable :: TypeValidator a
  table    :: TypeValidator a
  plot     :: TypeValidator a
  matrix   :: TypeValidator a
  array    :: TypeValidator a
  obj      :: TypeValidator a
  str      :: TypeValidator a
  num      :: TypeValidator a
  bool     :: TypeValidator a
  null     :: TypeValidator a
  
  getP :: a -> Pos
  getT :: a -> String

instance Marshallable ExpToken where
  exp _      = error "Eval.Function::exp<ExpToken> [Not Implemented]"
  showable _ = error "Eval.Function::showable<ExpToken> [Not Implemented]"
  table _    = error "Eval.Function::table<ExpToken> [Not Implemented]"
  plot _     = error "Eval.Function::plot<ExpToken> [Not Implemented]"
  matrix _   = error "Eval.Function::matrix<ExpToken> [Not Implemented]"
  array _    = error "Eval.Function::array<ExpToken> [Not Implemented]"
  obj _      = error "Eval.Function::obj<ExpToken> [Not Implemented]"

--  array (ArrayT p _ es) = liftM (ArrayO p) $ mapM exp es;   array e = typeMismatch (head types) e
--  obj  (ObjT p _ ps)    = liftM (ObjO p) $ mapM toTuple ps; obj e   = typeMismatch (types !! 1) e
  str  (StrT p _ s)     = return $ StrO p s;                str e   = typeMismatch (types !! 2) e
  num  (NumT p _ _ n)   = return $ NumO p n;                num e   = typeMismatch (types !! 3) e
  bool (BoolT p _ b)    = return $ BoolO p b;               bool e  = typeMismatch (types !! 4) e
  null (NullT p _)      = return $ NullO p;                 null e  = typeMismatch (types !! 5) e
  
  getP (ArrayT p _ _) = p
  getP (ObjT p _ _)   = p
  getP (StrT p _ _)   = p
  getP (NumT p _ _ _) = p
  getP (BoolT p _ _)  = p
  getP (NullT p _)    = p
  getP e              = error $ "Eval.Function::getP<ExpToken> [Failed pattern match ["++show e++"]]"
  
  getT (ArrayT{}) = head types
  getT (ObjT{})   = types !! 1
  getT (StrT{})   = types !! 2
  getT (NumT{})   = types !! 3
  getT (BoolT{})  = types !! 4
  getT (NullT{})  = types !! 5
  getT e          = error $ "Eval.Function::getT<ExpToken> [Failed pattern match ["++show e++"]]"

instance Marshallable ExpObj where
  exp _      = error "Eval.Function::exp<Obj> [Not Implemented]"
  showable _ = error "Eval.Function::showable<Obj> [Not Implemented]"
  table _    = error "Eval.Function::table<Obj> [Not Implemented]"
  plot _     = error "Eval.Function::plot<Obj> [Not Implemented]"
  matrix _   = error "Eval.Function::matrix<Obj> [Not Implemented]"

  array x@(ArrayO{}) = return x; array e = typeMismatch (head types) e
  obj   x@(ObjO{})   = return x; obj e   = typeMismatch (types !! 1) e  
  str   x@(StrO{})   = return x; str e   = typeMismatch (types !! 2) e
  num   x@(NumO{})   = return x; num e   = typeMismatch (types !! 3) e
  bool  x@(BoolO{})  = return x; bool e  = typeMismatch (types !! 4) e
  null  x@(NullO{})  = return x; null e  = typeMismatch (types !! 5) e
  
  getP (ArrayO p _) = p
  getP (ObjO p _)   = p
  getP (StrO p _)   = p
  getP (NumO p _)   = p
  getP (BoolO p _)  = p
  getP (NullO p)    = p
--  getP e              = error $ "Eval.Function::getP<Obj> [Failed pattern match ["++show e++"]]"
  
  getT (ArrayO{}) = head types
  getT (ObjO{})   = types !! 1
  getT (StrO{})   = types !! 2
  getT (NumO{})   = types !! 3
  getT (BoolO{})  = types !! 4
  getT (NullO{})  = types !! 5
--  getT e          = error $ "Eval.Function::getT<Obj> [Failed pattern match ["++show e++"]]"


typeMismatch :: Marshallable a => String -> a -> Eval ExpObj
typeMismatch expected e = Left $ TypeMismatch (getP e) expected (getT e)

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

types :: [String]
types = ["Array","Object","String","Number","Boolean","Null"]

toTuple :: PairToken -> Eval (String,ExpObj)
toTuple (PairT _ (IdT _ _ x) y) = liftM2 (,) (return x) (exp y)

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