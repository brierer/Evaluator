{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns #-}
module Eval.FunctionEvalTest where

import Test.Framework hiding            (forAll)
import Prelude        hiding            (null)
                                        
import Data.Eval                        (EvalError(..),ExpObj(..))
import Data.Token                       (IdToken(..),ExpToken(NullT))
import Eval.Function                    (Marshallable(..),types,applyFunc)
import Eval.FunctionEvalTestUtils       (Is(..),TestFuncs(..),ExpOA(..),ArrayOA(..),ObjOA(..),StrOA(..),NumOA(..),BoolOA(..),NullOA(..),ExpTS(..),ArrayTS(..),ObjTS(..),
                                         testFunc,forAll,removeEntry,funcNames,testF,mkFunc)
import Parser.MonolithicParserTestUtils (IdTA(..),ExpTA(..),StrTA(..),NumTA(..),BoolTA(..),NullTA(..))

{-| Number of args validation -}
prop_NbArgs (NonNegative n) (NonNegative m) (IdTA (IdT p w name)) = n /= m ==>
  let (nbParams,nbArgs) = (n `mod` 1000,m `mod` 1000) 
      fs = [(name,(replicate nbParams null,const $ return $ NullO p))]
  in  Left (InvalidNbOfArgs p name nbParams nbArgs) == applyFunc fs (mkFunc p name $ replicate nbArgs $ NullT p w) &&
      Right (NullO p)                               == applyFunc fs (mkFunc p name $ replicate nbParams $ NullT p w)
  
{-| Type validations -}
-- Array
prop_ErrorMarshallArrayLit  (ExpTS e) = not (isArray e || isVar e || isFunc e) ==> Left (TypeMismatch (getP e) "Array" (getT e)) == array [] e
prop_ErrorMarshallArrayObj  (ExpOA e) = not (isArray e)                        ==> Left (TypeMismatch (getP e) "Array" (getT e)) == array [] e
prop_ErrorMarshallArrayFunc (TF es)      = 
  forAll (removeEntry "arrayTestF" $ zip3 funcNames types es) $ \(name,t,e) ->     Left (TypeMismatch (getP e) "Array" t)        == array [] (testFunc es (getP e) name) 

prop_MarshallArrayLit  (ArrayTS a)           = testF a == array [] a
prop_MarshallArrayObj  (ArrayOA a)           = Right a == array [] a
prop_MarshallArrayFunc (TF es@[a,_,_,_,_,_]) = testF a == array [] (testFunc es (getP a) "arrayTestF")

-- Obj
prop_ErrorMarshallObjLit  (ExpTS e) = not (isObj e || isVar e || isFunc e) ==> Left (TypeMismatch (getP e) "Object" (getT e)) == obj [] e
prop_ErrorMarshallObjObj  (ExpOA e) = not (isObj e)                        ==> Left (TypeMismatch (getP e) "Object" (getT e)) == obj [] e
prop_ErrorMarshallObjFunc (TF es)   = 
  forAll (removeEntry "objTestF" $ zip3 funcNames types es) $ \(name,t,e) ->   Left (TypeMismatch (getP e) "Object" t)        == obj [] (testFunc es (getP e) name) 

prop_MarshallObjLit  (ObjTS o)             = testF o == obj [] o
prop_MarshallObjObj  (ObjOA o)             = Right o == obj [] o
prop_MarshallObjFunc (TF es@[_,o,_,_,_,_]) = testF o == obj [] (testFunc es (getP o) "objTestF")

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











