{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns #-}
module Eval.FunctionEvalTest where

import Test.Framework hiding            (forAll)
import Prelude        hiding            (null)
                                        
import Data.Eval                        (EvalError(..))
import Eval.Function                    (Marshallable(..),funcs,types)
import Eval.FunctionEvalTestUtils       (Is(..),TestFuncs(..),ExpOA(..),ArrayOA(..),ObjOA(..),StrOA(..),NumOA(..),BoolOA(..),NullOA(..),testFunc,applyFunc',forAll,removeEntry,funcNames,testF)
import Parser.MonolithicParserTestUtils (P(..),ExpTA(..),StrTA(..),NumTA(..),BoolTA(..),NullTA(..),unExpTA)

prop_NbArgs1 (P p) esTA = length esTA > 1 ==> let es = map unExpTA esTA in
  all (\name -> Left (InvalidNbOfArgs p name 1 0)           == applyFunc' funcs p name []
             && Left (InvalidNbOfArgs p name 1 (length es)) == applyFunc' funcs p name es)
    ["show","multi","mean","descriptive"]

prop_NbArgs2 (P p) esTA = length esTA > 2 ==> let es = map unExpTA esTA in
  all (\name -> Left (InvalidNbOfArgs p name 2 0)           == applyFunc' funcs p name []
             && Left (InvalidNbOfArgs p name 2 1)           == applyFunc' funcs p name (take 1 es)
             && Left (InvalidNbOfArgs p name 2 (length es)) == applyFunc' funcs p name es)
    ["table","nTimes","take","sortTable"]

prop_NbArgs3 (P p) esTA = length esTA > 3 ==> let es = map unExpTA esTA in
  all (\name -> Left (InvalidNbOfArgs p name 3 0)           == applyFunc' funcs p name []
             && Left (InvalidNbOfArgs p name 3 1)           == applyFunc' funcs p name (take 1 es)
             && Left (InvalidNbOfArgs p name 3 2)           == applyFunc' funcs p name (take 2 es)
             && Left (InvalidNbOfArgs p name 3 (length es)) == applyFunc' funcs p name es)
    ["plotLine"]

{-| Validators including literals -}
-- Array
--prop_ErrorMarshallArrayLit  (ExpTA e) = not (isArray e || isVar e || isFunc e) ==> Left (TypeMismatch (getP e) "Array" (getT e)) == array e
prop_ErrorMarshallArrayObj  (ExpOA e) = not (isArray e)                        ==> Left (TypeMismatch (getP e) "Array" (getT e)) == array e
prop_ErrorMarshallArrayFunc (TF es)   = 
  forAll (removeEntry "arrayTestF" $ zip3 funcNames types es) $ \(name,t,e) ->     Left (TypeMismatch (getP e) "Array" t)        == array (testFunc es (getP e) name) 

--prop_MarshallArrayLit  (ArrayTA a)           = testF a == array a
prop_MarshallArrayObj  (ArrayOA a)           = Right a == array a
prop_MarshallArrayFunc (TF es@[a,_,_,_,_,_]) = testF a == array (testFunc es (getP a) "arrayTestF")

-- Obj
--prop_ErrorMarshallObjLit  (ExpTA e) = not (isObj e || isVar e || isFunc e) ==> Left (TypeMismatch (getP e) "Object" (getT e)) == obj e
prop_ErrorMarshallObjObj  (ExpOA e) = not (isObj e)                        ==> Left (TypeMismatch (getP e) "Object" (getT e)) == obj e
prop_ErrorMarshallObjFunc (TF es)   = 
  forAll (removeEntry "objTestF" $ zip3 funcNames types es) $ \(name,t,e) ->   Left (TypeMismatch (getP e) "Object" t)        == obj (testFunc es (getP e) name) 

--prop_MarshallObjLit  (ObjTA o)             = testF o == obj o
prop_MarshallObjObj  (ObjOA o)             = Right o == obj o
prop_MarshallObjFunc (TF es@[_,o,_,_,_,_]) = testF o == obj (testFunc es (getP o) "objTestF")

-- Str
prop_ErrorMarshallStrLit  (ExpTA e) = not (isStr e || isVar e || isFunc e) ==> Left (TypeMismatch (getP e) "String" (getT e)) == str e
prop_ErrorMarshallStrObj  (ExpOA e) = not (isStr e)                        ==> Left (TypeMismatch (getP e) "String" (getT e)) == str e
prop_ErrorMarshallStrFunc (TF es)   = 
  forAll (removeEntry "strTestF" $ zip3 funcNames types es) $ \(name,t,e) ->   Left (TypeMismatch (getP e) "String" t)        == str (testFunc es (getP e) name) 

prop_MarshallStrLit  (StrTA s)             = testF s == str s
prop_MarshallStrObj  (StrOA s)             = Right s == str s
prop_MarshallStrFunc (TF es@[_,_,s,_,_,_]) = testF s == str (testFunc es (getP s) "strTestF")

-- Num
prop_ErrorMarshallNumLit  (ExpTA e) = not (isNum e || isVar e || isFunc e) ==> Left (TypeMismatch (getP e) "Number" (getT e)) == num e
prop_ErrorMarshallNumObj  (ExpOA e) = not (isNum e)                        ==> Left (TypeMismatch (getP e) "Number" (getT e)) == num e
prop_ErrorMarshallNumFunc (TF es)   = 
  forAll (removeEntry "numTestF" $ zip3 funcNames types es) $ \(name,t,e) ->   Left (TypeMismatch (getP e) "Number" t)        == num (testFunc es (getP e) name) 

prop_MarshallNumLit  (NumTA _ n)           = testF n == num n
prop_MarshallNumObj  (NumOA   n)           = Right n == num n
prop_MarshallNumFunc (TF es@[_,_,_,n,_,_]) = testF n == num (testFunc es (getP n) "numTestF")

-- Bool
prop_ErrorMarshallBoolLit  (ExpTA e) = not (isBool e || isVar e || isFunc e) ==> Left (TypeMismatch (getP e) "Boolean" (getT e)) == bool e
prop_ErrorMarshallBoolObj  (ExpOA e) = not (isBool e)                        ==> Left (TypeMismatch (getP e) "Boolean" (getT e)) == bool e
prop_ErrorMarshallBoolFunc (TF es)   = 
  forAll (removeEntry "boolTestF" $ zip3 funcNames types es) $ \(name,t,e) ->    Left (TypeMismatch (getP e) "Boolean" t)        == bool (testFunc es (getP e) name) 

prop_MarshallBoolLit  (BoolTA b)            = testF b == bool b
prop_MarshallBoolObj  (BoolOA b)            = Right b == bool b
prop_MarshallBoolFunc (TF es@[_,_,_,_,b,_]) = testF b == bool (testFunc es (getP b) "boolTestF")

-- Null
prop_ErrorMarshallNullLit  (ExpTA e) = not (isNull e || isVar e || isFunc e) ==> Left (TypeMismatch (getP e) "Null" (getT e)) == null e
prop_ErrorMarshallNullObj  (ExpOA e) = not (isNull e)                        ==> Left (TypeMismatch (getP e) "Null" (getT e)) == null e
prop_ErrorMarshallNullFunc (TF es)   = 
  forAll (removeEntry "nullTestF" $ zip3 funcNames types es) $ \(name,t,e) ->    Left (TypeMismatch (getP e) "Null" t)        == null (testFunc es (getP e) name) 

prop_MarshallNullLit  (NullTA n)            = testF n == null n
prop_MarshallNullObj  (NullOA n)            = Right n == null n
prop_MarshallNullFunc (TF es@[_,_,_,_,_,n]) = testF n == null (testFunc es (getP n) "nullTestF")











