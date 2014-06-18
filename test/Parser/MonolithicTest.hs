{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Parser.MonolithicTest where

import Parser.Monolithic      (progT,formT,pairT,expT,funcT,arrayT,objT,varT,strT,numT,boolT,nullT)
import Parser.ParserTestUtils ( ProgTA(..),FormTA(..),PairTA(..),ExpTA(..),FuncTA(..),ArrayTA(..),ObjTA(..)
                              , VarTA(..),StrTA(..),NumTA(..),BoolTA(..),NullTA(..),NumType(..),testCase)
import Test.Framework         (TestSuite,makeTestSuite,makeQuickCheckTest,makeLoc,qcAssertion,(==>))

{-# ANN module "HLint: ignore Use camelCase" #-}

-- Program
prop_Prog (ProgTA prog)    = prog == testCase progT prog
prop_Form (FormTA form)    = form == testCase formT form
prop_Pair (PairTA pair)    = pair == testCase pairT pair
prop_Exp  (ExpTA e)        = e    == testCase expT e

-- Composite expressions
prop_Func (FuncTA func)    = func  == testCase funcT func
prop_Array (ArrayTA array) = array == testCase arrayT array
prop_Obj (ObjTA obj)       = obj   == testCase objT obj

-- Atomic expressions
prop_Var    (VarTA var)    = var == testCase varT var
prop_String (StrTA str)    = str == testCase strT str

prop_Num_int (NumTA t int) = (t == Int) ==> int == testCase numT int
prop_Num_flt (NumTA t flt) = (t == Flt) ==> flt == testCase numT flt
prop_Num_exp (NumTA t flt) = (t == Exp) ==> flt == testCase numT flt

prop_Bool (BoolTA b)       = b == testCase boolT b
prop_Null (NullTA n)       = n == testCase nullT n


