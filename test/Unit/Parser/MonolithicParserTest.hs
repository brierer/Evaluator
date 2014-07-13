{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Unit.Parser.MonolithicParserTest where

import Parser.Monolithic
import Test.Framework

import Common.Parser.MonolithicParserUtils
import Unit.Parser.MonolithicParserUtils

{-# ANN module "HLint: ignore Use camelCase" #-}

-- Program
test_Prog = do assertEqual (mkProg  (1,1) [])                                 $ unsafeParse progT ""
               assertEqual (mkProg (1,1) [mkForm' (1,1) "x" $ mkNull' (1,3)]) $ unsafeParse progT "x=null"
test_Form = do assertEqual (mkForm' (1,1) "a" $ mkVar' (1,3) "b")             $ unsafeParse formT "a=b"
               assertEqual (mkForm' (1,1) "b" $ mkStr' (1,3) "hello")         $ unsafeParse formT "b=\"hello\""
test_Pair = do assertEqual (mkPair' (1,1) "c" $ mkNum' (1,3) "1" 1)           $ unsafeParse pairT "c:1"
               assertEqual (mkPair' (1,1) "d" $ mkNum' (1,3) "2.0" 2)         $ unsafeParse pairT "d:2.0"
test_Id =   do assertEqual (mkId' (1,1) "a123")                               $ unsafeParse idT  "a123"
               assertEqual (mkId' (1,1) "hello")                              $ unsafeParse idT  "hello"
test_Exp =  do assertEqual (mkFunc' (1,1) "f" [])                             $ unsafeParse expT "f()"
               assertEqual (mkArr' (1,1) [])                                  $ unsafeParse expT "[]"
               assertEqual (mkObj' (1,1) [])                                  $ unsafeParse expT "{}"
               assertEqual (mkVar' (1,1) "x")                                 $ unsafeParse expT "x"
               assertEqual (mkStr' (1,1) "")                                  $ unsafeParse expT "\"\""
               assertEqual (mkNum' (1,1) "-1" (-1))                           $ unsafeParse expT "-1"
               assertEqual (mkBool' (1,1) True)                               $ unsafeParse expT "true"
               assertEqual (mkNull' (1,1))                                    $ unsafeParse expT "null"

-- Composite expressions
test_Func = do assertEqual (mkFunc' (1,1) "f1" [mkNull' (1,4)])                                                    $ unsafeParse funcT "f1(null)"
               assertEqual (mkFunc' (1,1) "f2" [mkNull' (1,4),mkBool' (1,9) False])                                $ unsafeParse funcT "f2(null,false)"
test_Arr =  do assertEqual (mkArr' (1,1) [mkNum' (1,2) "0" 0])                                                     $ unsafeParse arrT  "[0]"
               assertEqual (mkArr' (1,1) [mkNum' (1,2) "-1.0" (-1),mkNum' (1,7) "-2e2" (-200)])                    $ unsafeParse arrT  "[-1.0,-2e2]"
test_Obj =  do assertEqual (mkObj' (1,1) [mkPair' (1,2) "x" $ mkStr' (1,4) "hello"])                               $ unsafeParse objT  "{x:\"hello\"}"
               assertEqual (mkObj' (1,1) [mkPair' (1,2) "x" $ mkVar' (1,4) "y",mkPair' (1,6) "y" $ mkNull' (1,8)]) $ unsafeParse objT  "{x:y,y:null}"

-- Atomic expressions
test_Var =  do assertEqual (mkVar' (1,1) "X")          $ unsafeParse varT  "X"
               assertEqual (mkVar' (1,1) "vaR123")     $ unsafeParse varT  "vaR123"
test_Str =     assertEqual (mkStr' (1,1) ascii)        $ unsafeParse strT  $ "\"" ++ ascii ++ "\""
test_Num =  do assertEqual (mkNum' (1,1) "10" 10)      $ unsafeParse numT  "10"
               assertEqual (mkNum' (1,1) "11.5" 11.5)  $ unsafeParse numT  "11.5"
               assertEqual (mkNum' (1,1) "1.23E2" 123) $ unsafeParse numT  "1.23E2"
test_Bool = do assertEqual (mkBool' (1,1) True)        $ unsafeParse boolT "true"
               assertEqual (mkBool' (1,1) False)       $ unsafeParse boolT "false"
test_Null =    assertEqual (mkNull' (1,1))             $ unsafeParse nullT "null"

{-| Unparse -}
-- Program
test_UnparseProg = do assertEqual ""            $ unparse $ mkProg (1,1) []
                      assertEqual "x=null"      $ unparse $ mkProg (1,1) [mkForm' (1,1) "x" $ mkNull' (1,3)]
test_UnparseForm = do assertEqual "a=b"         $ unparse $ mkForm' (1,1) "a" $ mkVar' (1,3) "b"
                      assertEqual "b=\"hello\"" $ unparse $ mkForm' (1,1) "b" $ mkStr' (1,3) "hello"
test_UnparsePair = do assertEqual "c:1"         $ unparse $ mkPair' (1,1) "c" $ mkNum' (1,3) "1" 1
                      assertEqual "d:2.0"       $ unparse $ mkPair' (1,1) "d" $ mkNum' (1,3) "2.0" 2
test_UnparseId =   do assertEqual "a123"        $ unparse $ mkId' (1,1) "a123"
                      assertEqual "hello"       $ unparse $ mkId' (1,1) "hello"
test_UnparseExp =  do assertEqual "f()"         $ unparse $ mkFunc' (1,1) "f" []
                      assertEqual "[]"          $ unparse $ mkArr' (1,1) []
                      assertEqual "{}"          $ unparse $ mkObj' (1,1) []
                      assertEqual "x"           $ unparse $ mkVar' (1,1) "x"
                      assertEqual "\"\""        $ unparse $ mkStr' (1,1) ""
                      assertEqual "-1"          $ unparse $ mkNum' (1,1) "-1" (-1)
                      assertEqual "true"        $ unparse $ mkBool' (1,1) True
                      assertEqual "null"        $ unparse $ mkNull' (1,1)

-- Composite expressions
test_UnparseFunc = do assertEqual "f1(null)"        $ unparse $ mkFunc' (1,1) "f1" [mkNull' (1,4)]
                      assertEqual "f2(null,false)"  $ unparse $ mkFunc' (1,1) "f2" [mkNull' (1,4),mkBool' (1,9) False]
test_UnparseArr =  do assertEqual "[0]"             $ unparse $ mkArr' (1,1) [mkNum' (1,2) "0" 0]
                      assertEqual "[-1.0,-2e2]"     $ unparse $ mkArr' (1,1) [mkNum' (1,2) "-1.0" (-1),mkNum' (1,7) "-2e2" (-200)]
test_UnparseObj =  do assertEqual "{x:\"hello\"}"   $ unparse $ mkObj' (1,1) [mkPair' (1,2) "x" $ mkStr' (1,4) "hello"]
                      assertEqual "{x:y,y:null}"    $ unparse $ mkObj' (1,1) [mkPair' (1,2) "x" $ mkVar' (1,4) "y",mkPair' (1,6) "y" $ mkNull' (1,8)]

-- Atomic expressions
test_UnparseVar =  do assertEqual "X"                     $ unparse $ mkVar' (1,1) "X"
                      assertEqual "vaR123"                $ unparse $ mkVar' (1,1) "vaR123"
test_UnparseStr =     assertEqual ("\"" ++ ascii ++ "\"") $ unparse $ mkStr' (1,1) ascii
test_UnparseNum =  do assertEqual "10"                    $ unparse $ mkNum' (1,1) "10" 10
                      assertEqual "11.5"                  $ unparse $ mkNum' (1,1) "11.5" 11.5
                      assertEqual "1.23E2"                $ unparse $ mkNum' (1,1) "1.23E2" 123
test_UnparseBool = do assertEqual "true"                  $ unparse $ mkBool' (1,1) True
                      assertEqual "false"                 $ unparse $ mkBool' (1,1) False
test_UnparseNull =    assertEqual "null"                  $ unparse $ mkNull' (1,1)
