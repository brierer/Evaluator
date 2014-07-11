{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Parser.MonolithicParserTest where

import Parser.Monolithic
import Parser.MonolithicParserTestUtils
import Test.Framework

{-# ANN module "HLint: ignore Use camelCase" #-}

{-| Units -}
-- Program
test_Prog = do
  assertEqual (Right $ mkProg [])                        $ simpleParse progT ""
  assertEqual (Right $ mkProg [mkForm "x" mkNull]) $ simpleParse progT "x=null"

test_Form = do
  assertEqual (Right $ mkForm "a" $ mkVar "b")     $ simpleParse formT "a=b"
  assertEqual (Right $ mkForm "b" $ mkStr "hello") $ simpleParse formT "b=\"hello\""

test_Pair = do
  assertEqual (Right $ mkPair "c" $ mkNum "1" 1)   $ simpleParse pairT "c:1"
  assertEqual (Right $ mkPair "d" $ mkNum "2.0" 2) $ simpleParse pairT "d:2.0"

test_Id = do
  assertEqual (Right $ mkId "a123")  $ simpleParse idT "a123"
  assertEqual (Right $ mkId "hello") $ simpleParse idT "hello"

test_Exp = do
  assertEqual (Right $ mkFunc "f" [])   $ simpleParse expT "f()"
  assertEqual (Right $ mkArray [])      $ simpleParse expT "[]"
  assertEqual (Right $ mkObj   [])      $ simpleParse expT "{}"
  assertEqual (Right $ mkVar "x")       $ simpleParse expT "x"
  assertEqual (Right $ mkStr "")        $ simpleParse expT "\"\""
  assertEqual (Right $ mkNum "-1" (-1)) $ simpleParse expT "-1"
  assertEqual (Right $ mkBool True)     $ simpleParse expT "true"
  assertEqual (Right mkNull)            $ simpleParse expT "null"
  
-- Composite expressions
test_Func = do
  assertEqual (Right $ mkFunc "f1" [mkNull])              $ simpleParse funcT "f1(null)"
  assertEqual (Right $ mkFunc "f2" [mkNull,mkBool False]) $ simpleParse funcT "f2(null,false)"

test_Array = do
  assertEqual (Right $ mkArray [mkNum "0" 0])                           $ simpleParse arrayT "[0]"
  assertEqual (Right $ mkArray [mkNum "-1.0" (-1),mkNum "-2e2" (-200)]) $ simpleParse arrayT "[-1.0,-2e2]"

test_Obj = do
  assertEqual (Right $ mkObj [mkPair "x" $ mkStr "hello"])                     $ simpleParse objT "{x:\"hello\"}"
  assertEqual (Right $ mkObj [mkPair "x" $ mkVar "y",mkPair "y" $ mkArray []]) $ simpleParse objT "{x:y,y:[]}"
  
-- Atomic expressions
test_Var = do
  assertEqual (Right $ mkVar "X")      $ simpleParse varT "X"
  assertEqual (Right $ mkVar "vaR123") $ simpleParse varT "vaR123"
  
test_Str = assertEqual (Right $ mkStr $ [' '..'!'] ++ ['#'..'~']) $ simpleParse strT $ "\"" ++ [' '..'!'] ++ ['#'..'~'] ++ "\""

test_Num = do
  assertEqual (Right $ mkNum "10" 10)      $ simpleParse numT "10"
  assertEqual (Right $ mkNum "11.5" 11.5)  $ simpleParse numT "11.5"
  assertEqual (Right $ mkNum "1.23E2" 123) $ simpleParse numT "1.23E2"
  
test_Bool = do
  assertEqual (Right $ mkBool True)  $ simpleParse boolT "true"
  assertEqual (Right $ mkBool False) $ simpleParse boolT "false"
  
test_Null = assertEqual (Right mkNull) $ simpleParse nullT "null"

{-| Props -}
-- Program
prop_Prog (ProgTA prog)    = prog == testCase progT prog
prop_Form (FormTA form)    = form == testCase formT form
prop_Pair (PairTA pair)    = pair == testCase pairT pair
prop_Id   (IdTA i)         = i    == testCase idT i
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


