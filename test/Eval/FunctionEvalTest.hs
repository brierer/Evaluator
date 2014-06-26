{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns #-}
module Eval.FunctionEvalTest where

import Test.Framework hiding            (forAll)
import Prelude        hiding            (null)
                                        
import Data.Eval                        (EvalError(..),ExpObj(..),Type(..))
import Data.Token                       (IdToken(..),ExpToken(NullT))
import Eval.Function                    (Marshallable(..),applyFunc)
import Eval.FunctionEvalTestUtils       (Is(..),TestFuncs(..),TestFuncs'(..),ExpOA(..),TableOA(..),PlotOA(..),ArrayOA(..),
                                         ObjOA(..),StrOA(..),NumOA(..),BoolOA(..),NullOA(..),ExpTS(..),ArrayTS(..),ObjTS(..),
                                         testFunc,forAll,mkEntries,testF,mkFunc)
import Parser.MonolithicParserTestUtils (IdTA(..),ExpTA(..),StrTA(..),NumTA(..),BoolTA(..),NullTA(..))

{-| Number of args validation -}
prop_NbArgs (NonNegative n) (NonNegative m) (IdTA (IdT p w name)) = let (nbParams,nbArgs) = (n `mod` 1000,m `mod` 1000)  in nbParams /= nbArgs ==>
  let fs = [(name,(replicate nbParams null,const $ return $ NullO p))]
  in  Left (InvalidNbOfArgs p name nbParams nbArgs) == applyFunc fs (mkFunc p name $ replicate nbArgs $ NullT p w) &&
      Right (NullO p)                               == applyFunc fs (mkFunc p name $ replicate nbParams $ NullT p w)
  
{-| Type validations -}
-- Table
prop_ErrorMarshallTableObj  (ExpOA e)        = not (isTable e) ==> Left (TypeMismatch (getP e) Table (getT e)) == table [] e
prop_ErrorMarshallTableFunc (TF' os) (TF es) = 
  forAll (mkEntries "tableTestF" es os) $ \(name,t,e) ->           Left (TypeMismatch (getP e) Table t)        == table [] (testFunc os es (getP e) name) 

prop_MarshallTableObj  (TableOA t)            = Right t == table [] t
prop_MarshallTableFunc (TF' os@[t,_]) (TF es) = Right t == table [] (testFunc os es (getP t) "tableTestF")

-- Plot
prop_ErrorMarshallPlotObj  (ExpOA e)        = not (isPlot e) ==> Left (TypeMismatch (getP e) Plot (getT e)) == plot [] e
prop_ErrorMarshallPlotFunc (TF' os) (TF es) = 
  forAll (mkEntries "plotTestF" es os) $ \(name,t,e) ->          Left (TypeMismatch (getP e) Plot t)        == plot [] (testFunc os es (getP e) name) 

prop_MarshallPlotObj  (PlotOA p)             = Right p == plot [] p
prop_MarshallPlotFunc (TF' os@[_,p]) (TF es) = Right p == plot [] (testFunc os es (getP p) "plotTestF")

{-| Types with literals -}
-- Array
prop_ErrorMarshallArrayLit  (ExpTS e)        = not (isArray e || isVar e || isFunc e) ==> Left (TypeMismatch (getP e) Array (getT e)) == array [] e
prop_ErrorMarshallArrayObj  (ExpOA e)        = not (isArray e)                        ==> Left (TypeMismatch (getP e) Array (getT e)) == array [] e
prop_ErrorMarshallArrayFunc (TF' os) (TF es) = 
  forAll (mkEntries "arrayTestF" es os) $ \(name,t,e) ->                                  Left (TypeMismatch (getP e) Array t)        == array [] (testFunc os es (getP e) name) 

prop_MarshallArrayLit  (ArrayTS a)                    = testF a == array [] a
prop_MarshallArrayObj  (ArrayOA a)                    = Right a == array [] a
prop_MarshallArrayFunc (TF' os) (TF es@[a,_,_,_,_,_]) = testF a == array [] (testFunc os es (getP a) "arrayTestF")

-- Obj
prop_ErrorMarshallObjLit  (ExpTS e)        = not (isObj e || isVar e || isFunc e) ==> Left (TypeMismatch (getP e) Object (getT e)) == obj [] e
prop_ErrorMarshallObjObj  (ExpOA e)        = not (isObj e)                        ==> Left (TypeMismatch (getP e) Object (getT e)) == obj [] e
prop_ErrorMarshallObjFunc (TF' os) (TF es) = 
  forAll (mkEntries "objTestF" es os) $ \(name,t,e) ->                                Left (TypeMismatch (getP e) Object t)        == obj [] (testFunc os es (getP e) name) 

prop_MarshallObjLit  (ObjTS o)                      = testF o == obj [] o
prop_MarshallObjObj  (ObjOA o)                      = Right o == obj [] o
prop_MarshallObjFunc (TF' os) (TF es@[_,o,_,_,_,_]) = testF o == obj [] (testFunc os es (getP o) "objTestF")

-- Str
prop_ErrorMarshallStrLit  (ExpTA e)        = not (isStr e || isVar e || isFunc e) ==> Left (TypeMismatch (getP e) String (getT e)) == str e
prop_ErrorMarshallStrObj  (ExpOA e)        = not (isStr e)                        ==> Left (TypeMismatch (getP e) String (getT e)) == str e
prop_ErrorMarshallStrFunc (TF' os) (TF es) = 
  forAll (mkEntries "strTestF" es os) $ \(name,t,e) ->                                Left (TypeMismatch (getP e) String t)        == str (testFunc os es (getP e) name) 

prop_MarshallStrLit  (StrTA s)                      = testF s == str s
prop_MarshallStrObj  (StrOA s)                      = Right s == str s
prop_MarshallStrFunc (TF' os) (TF es@[_,_,s,_,_,_]) = testF s == str (testFunc os es (getP s) "strTestF")

-- Num
prop_ErrorMarshallNumLit  (ExpTA e)       = not (isNum e || isVar e || isFunc e) ==> Left (TypeMismatch (getP e) Number (getT e)) == num e
prop_ErrorMarshallNumObj  (ExpOA e)       = not (isNum e)                        ==> Left (TypeMismatch (getP e) Number (getT e)) == num e
prop_ErrorMarshallNumFunc (TF' os) (TF es)= 
  forAll (mkEntries "numTestF" es os) $ \(name,t,e) ->                               Left (TypeMismatch (getP e) Number t)        == num (testFunc os es (getP e) name) 

prop_MarshallNumLit  (NumTA _ n)                    = testF n == num n
prop_MarshallNumObj  (NumOA   n)                    = Right n == num n
prop_MarshallNumFunc (TF' os) (TF es@[_,_,_,n,_,_]) = testF n == num (testFunc os es (getP n) "numTestF")

-- Bool
prop_ErrorMarshallBoolLit  (ExpTA e)        = not (isBool e || isVar e || isFunc e) ==> Left (TypeMismatch (getP e) Boolean (getT e)) == bool e
prop_ErrorMarshallBoolObj  (ExpOA e)        = not (isBool e)                        ==> Left (TypeMismatch (getP e) Boolean (getT e)) == bool e
prop_ErrorMarshallBoolFunc (TF' os) (TF es) = 
  forAll (mkEntries "boolTestF" es os) $ \(name,t,e) ->                                 Left (TypeMismatch (getP e) Boolean t)        == bool (testFunc os es (getP e) name) 

prop_MarshallBoolLit  (BoolTA b)                     = testF b == bool b
prop_MarshallBoolObj  (BoolOA b)                     = Right b == bool b
prop_MarshallBoolFunc (TF' os) (TF es@[_,_,_,_,b,_]) = testF b == bool (testFunc os es (getP b) "boolTestF")

-- Null
prop_ErrorMarshallNullLit  (ExpTA e)        = not (isNull e || isVar e || isFunc e) ==> Left (TypeMismatch (getP e) Null (getT e)) == null e
prop_ErrorMarshallNullObj  (ExpOA e)        = not (isNull e)                        ==> Left (TypeMismatch (getP e) Null (getT e)) == null e
prop_ErrorMarshallNullFunc (TF' os) (TF es) = 
  forAll (mkEntries "nullTestF" es os) $ \(name,t,e) ->                                 Left (TypeMismatch (getP e) Null t)        == null (testFunc os es (getP e) name) 

prop_MarshallNullLit  (NullTA n)                     = testF n == null n
prop_MarshallNullObj  (NullOA n)                     = Right n == null n
prop_MarshallNullFunc (TF' os) (TF es@[_,_,_,_,_,n]) = testF n == null (testFunc os es (getP n) "nullTestF")



