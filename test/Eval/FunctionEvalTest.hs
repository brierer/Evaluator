{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns #-}
module Eval.FunctionEvalTest where

import Test.Framework hiding            (forAll)
import Prelude        hiding            (any,null)
                                        
import Control.Monad                    (liftM)                                        
import Data.Eval                        (EvalError(..),ExpObj(..),Type(..))
import Data.Token                       (IdToken(..),ExpToken(NullT))
import Eval.Function                    (Marshallable(..),any,noLit,noLitType,lit,litType,applyFunc)
import Eval.FunctionEvalTestUtils       (Is(..),TestToks(..),TestObjs(..),ExpOA(..),TableOA(..),PlotOA(..),ArrayOA(..),
                                         ObjOA(..),StrOA(..),NumOA(..),BoolOA(..),NullOA(..),ExpTS(..),ArrayTS(..),ObjTS(..),ExpTF(..),ArrayTF(..),ObjTF(..),TokOrObj(..),
                                         testFunc,forAll,mkEntries,anyCase,litCase,testS,testF,mkFunc,funcNamesLit,funcNamesNoLit,constM)
import Eval.MultiPassEvalTestUtils      (usesFuncE)
import Parser.MonolithicParserTestUtils (IdTA(..),ExpTA(..),StrTA(..),NumTA(..),BoolTA(..),NullTA(..))

{-| Number of args validation -}
prop_NbArgs (NonNegative n) (NonNegative m) (IdTA (IdT p w name)) = let (nbParams,nbArgs) = (n `mod` 1000,m `mod` 1000)  in nbParams /= nbArgs ==>
  let fs = [(name,(replicate nbParams null,const $ return $ NullO p))]
  in  Left (InvalidNbOfArgs p name nbParams nbArgs) == applyFunc fs (mkFunc p name $ replicate nbArgs $ NullT p w) &&
      Right (NullO p)                               == applyFunc fs (mkFunc p name $ replicate nbParams $ NullT p w)

{-| Combinators -}

-- Any type (can't fail)
prop_MarshallAnyLit (ExpTS e)                    = testS e == any [] e
prop_MarshallAnyObj (ExpOA e)                    = Right e == any [] e
prop_MarshallAnyFunc (TestObjs os) (TestToks es) = forAll (mkEntries [] es os) (anyCase os es)

{-| Basic types without literals -}
-- Any type without literal
prop_ErrorMarshallNoLitLit (ExpTS e)                     = Left (TypeMismatch (getP e) noLitType (getT e)) == noLit [] e
prop_ErrorMarshallNoLitFunc (TestObjs os) (TestToks es)  = 
  forAll (mkEntries funcNamesNoLit es os) $ \(name,t,e) -> Left (TypeMismatch (getP e) noLitType t)        == noLit [] (testFunc os es (getP e) name) 

prop_MarshallNoLitObj  (ExpOA e)                       = isTable e || isPlot e ==> Right e == noLit [] e
prop_MarshallNoLitFunc (TestObjs os) (TestToks es)     =
  forAll (mkEntries funcNamesLit es os) $ \(name,_,e) ->                           Right e == liftM Obj (noLit [] (testFunc os es (getP e) name))

-- Table
prop_ErrorMarshallTableObj  (ExpOA e)                   = not (isTable e) ==> Left (TypeMismatch (getP e) Table (getT e)) == table [] e
prop_ErrorMarshallTableFunc (TestObjs os) (TestToks es) = 
  forAll (mkEntries ["tableTestF"] es os) $ \(name,t,e) ->                    Left (TypeMismatch (getP e) Table t)        == table [] (testFunc os es (getP e) name) 

prop_MarshallTableObj  (TableOA t)                       = Right t == table [] t
prop_MarshallTableFunc (TestObjs os@[t,_]) (TestToks es) = Right t == table [] (testFunc os es (getP t) "tableTestF")

-- Plot
prop_ErrorMarshallPlotObj  (ExpOA e)                   = not (isPlot e) ==> Left (TypeMismatch (getP e) Plot (getT e)) == plot [] e
prop_ErrorMarshallPlotFunc (TestObjs os) (TestToks es) = 
  forAll (mkEntries ["plotTestF"] es os) $ \(name,t,e) ->                   Left (TypeMismatch (getP e) Plot t)        == plot [] (testFunc os es (getP e) name) 

prop_MarshallPlotObj  (PlotOA p)                        = Right p == plot [] p
prop_MarshallPlotFunc (TestObjs os@[_,p]) (TestToks es) = Right p == plot [] (testFunc os es (getP p) "plotTestF")

{-| Basic types with literals -}
-- Any type with literal
prop_ErrorMarshallLitObj (ExpOA e)                    = isTable e || isPlot e ==> Left (TypeMismatch (getP e) litType (getT e)) == lit [] e
prop_ErrorMarshallLitFunc (TestObjs os) (TestToks es) = 
  forAll (mkEntries funcNamesLit es os) $ \(name,t,e) ->                          Left (TypeMismatch (getP e) litType t)        == lit [] (testFunc os es (getP e) name) 

prop_MarshallLitLit  (ExpTS e)                   = testS e == lit [] e
prop_MarshallLitFunc (TestObjs os) (TestToks es) = forAll (mkEntries funcNamesNoLit es os) (litCase os es)

-- Array
prop_ErrorMarshallArrayLit  (ExpTS e)                   = not (isArray e || isVar e || isFunc e) ==> Left (TypeMismatch (getP e) Array (getT e)) == array [] e
prop_ErrorMarshallArrayObj  (ExpOA e)                   = not (isArray e)                        ==> Left (TypeMismatch (getP e) Array (getT e)) == array [] e
prop_ErrorMarshallArrayFunc (TestObjs os) (TestToks es) = 
  forAll (mkEntries ["arrayTestF"] es os) $ \(name,t,e) ->                                           Left (TypeMismatch (getP e) Array t)        == array [] (testFunc os es (getP e) name) 

prop_MarshallArrayLit  (ArrayTS a)                               = testS a == array [] a
prop_MarshallArrayObj  (ArrayOA a)                               = Right a == array [] a
prop_MarshallArrayFunc (TestObjs os) (TestToks es@[a,_,_,_,_,_]) = testS a == array [] (testFunc os es (getP a) "arrayTestF")

-- Obj
prop_ErrorMarshallObjLit  (ExpTS e)                   = not (isObj e || isVar e || isFunc e) ==> Left (TypeMismatch (getP e) Object (getT e)) == obj [] e
prop_ErrorMarshallObjObj  (ExpOA e)                   = not (isObj e)                        ==> Left (TypeMismatch (getP e) Object (getT e)) == obj [] e
prop_ErrorMarshallObjFunc (TestObjs os) (TestToks es) = 
  forAll (mkEntries ["objTestF"] es os) $ \(name,t,e) ->                                         Left (TypeMismatch (getP e) Object t)        == obj [] (testFunc os es (getP e) name) 

prop_MarshallObjLit  (ObjTS o)                                 = testS o == obj [] o
prop_MarshallObjObj  (ObjOA o)                                 = Right o == obj [] o
prop_MarshallObjFunc (TestObjs os) (TestToks es@[_,o,_,_,_,_]) = testS o == obj [] (testFunc os es (getP o) "objTestF")

-- Str
prop_ErrorMarshallStrLit  (ExpTA e)                   = not (isStr e || isVar e || isFunc e) ==> Left (TypeMismatch (getP e) String (getT e)) == str e
prop_ErrorMarshallStrObj  (ExpOA e)                   = not (isStr e)                        ==> Left (TypeMismatch (getP e) String (getT e)) == str e
prop_ErrorMarshallStrFunc (TestObjs os) (TestToks es) = 
  forAll (mkEntries ["strTestF"] es os) $ \(name,t,e) ->                                         Left (TypeMismatch (getP e) String t)        == str (testFunc os es (getP e) name) 

prop_MarshallStrLit  (StrTA s)                                 = testS s == str s
prop_MarshallStrObj  (StrOA s)                                 = Right s == str s
prop_MarshallStrFunc (TestObjs os) (TestToks es@[_,_,s,_,_,_]) = testS s == str (testFunc os es (getP s) "strTestF")

-- Num
prop_ErrorMarshallNumLit  (ExpTA e)                   = not (isNum e || isVar e || isFunc e) ==> Left (TypeMismatch (getP e) Number (getT e)) == num e
prop_ErrorMarshallNumObj  (ExpOA e)                   = not (isNum e)                        ==> Left (TypeMismatch (getP e) Number (getT e)) == num e
prop_ErrorMarshallNumFunc (TestObjs os) (TestToks es) = 
  forAll (mkEntries ["numTestF"] es os) $ \(name,t,e) ->                                         Left (TypeMismatch (getP e) Number t)        == num (testFunc os es (getP e) name) 

prop_MarshallNumLit  (NumTA _ n)                               = testS n == num n
prop_MarshallNumObj  (NumOA   n)                               = Right n == num n
prop_MarshallNumFunc (TestObjs os) (TestToks es@[_,_,_,n,_,_]) = testS n == num (testFunc os es (getP n) "numTestF")

-- Bool
prop_ErrorMarshallBoolLit  (ExpTA e)                   = not (isBool e || isVar e || isFunc e) ==> Left (TypeMismatch (getP e) Boolean (getT e)) == bool e
prop_ErrorMarshallBoolObj  (ExpOA e)                   = not (isBool e)                        ==> Left (TypeMismatch (getP e) Boolean (getT e)) == bool e
prop_ErrorMarshallBoolFunc (TestObjs os) (TestToks es) = 
  forAll (mkEntries ["boolTestF"] es os) $ \(name,t,e) ->                                          Left (TypeMismatch (getP e) Boolean t)        == bool (testFunc os es (getP e) name) 

prop_MarshallBoolLit  (BoolTA b)                                = testS b == bool b
prop_MarshallBoolObj  (BoolOA b)                                = Right b == bool b
prop_MarshallBoolFunc (TestObjs os) (TestToks es@[_,_,_,_,b,_]) = testS b == bool (testFunc os es (getP b) "boolTestF")

-- Null
prop_ErrorMarshallNullLit  (ExpTA e)        = not (isNull e || isVar e || isFunc e) ==> Left (TypeMismatch (getP e) Null (getT e)) == null e
prop_ErrorMarshallNullObj  (ExpOA e)        = not (isNull e)                        ==> Left (TypeMismatch (getP e) Null (getT e)) == null e
prop_ErrorMarshallNullFunc (TestObjs os) (TestToks es) = 
  forAll (mkEntries ["nullTestF"] es os) $ \(name,t,e) ->                               Left (TypeMismatch (getP e) Null t)        == null (testFunc os es (getP e) name) 

prop_MarshallNullLit  (NullTA n)                                = testS n == null n
prop_MarshallNullObj  (NullOA n)                                = Right n == null n
prop_MarshallNullFunc (TestObjs os) (TestToks es@[_,_,_,_,_,n]) = testS n == null (testFunc os es (getP n) "nullTestF")

{-| Literals containing function calls -}
prop_MarshallFuncsLit   (ExpTF   e o (IdT _ _ i)) = usesFuncE i e ==> testF fs e == lit   fs e where fs = [(i,([],constM o))]
prop_MarshallFuncsArray (ArrayTF e o (IdT _ _ i)) = usesFuncE i e ==> testF fs e == array fs e where fs = [(i,([],constM o))]
prop_MarshallFuncsObj   (ObjTF   e o (IdT _ _ i)) = usesFuncE i e ==> testF fs e == obj   fs e where fs = [(i,([],constM o))]












