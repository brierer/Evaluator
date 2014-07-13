{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Prop.Parser.MonolithicParserTest where

import Parser.Monolithic
import Test.Framework

import Prop.Parser.MonolithicParserUtils

{-# ANN module "HLint: ignore Use camelCase" #-}

-- Program
prop_Prog (ProgTA prog)    =                prog .= testCase progT prog
prop_Form (FormTA form)    =                form .= testCase formT form
prop_Pair (PairTA pair)    =                pair .= testCase pairT pair
prop_Id   (IdTA i)         =                i    .= testCase idT i
prop_Exp  (ExpTA e)        =                e    .= testCase expT e
                                            
-- Composite expressions                    
prop_Func  (FuncTA func)   =                func .= testCase funcT func
prop_Array (ArrTA  arr)    =                arr  .= testCase arrT  arr
prop_Obj   (ObjTA  obj)    =                obj  .= testCase objT  obj
                                            
-- Atomic expressions                       
prop_Var    (VarTA var)    =                var .= testCase varT var
prop_String (StrTA str)    =                str .= testCase strT str

prop_Num_int (NumTA t int) = (t == Int) ==> int .= testCase numT int
prop_Num_flt (NumTA t flt) = (t == Flt) ==> flt .= testCase numT flt
prop_Num_exp (NumTA t flt) = (t == Exp) ==> flt .= testCase numT flt

prop_Bool (BoolTA b)       =                b   .= testCase boolT b
prop_Null (NullTA n)       =                n   .= testCase nullT n