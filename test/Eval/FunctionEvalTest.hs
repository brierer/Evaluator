{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns #-}
module Eval.FunctionEvalTest where

import Test.Framework hiding            (forAll)
import Prelude        hiding            (any,null)
                                        
import Control.Monad                    (liftM)                                        
import Data.Eval                        (EvalError(..),ExpObj(..),Type(..),Func(..))
import Data.List                        ((\\))
import Data.Token                       (IdToken(..),ExpToken(..))
import Eval.Function                    (Marshallable(..),any,noLit,noLitType,lit,litType,applyFunc,withFuncs)
import Eval.FunctionEvalTestUtils       (Is(..),TestToks(..),TestObjs(..),ExpOA(..),TableOA(..),PlotOA(..),ArrayOA(..),ObjOA(..),StrOA(..),NumOA(..),BoolOA(..),NullOA(..),
                                         ExpTS(..),ArrayTS(..),ObjTS(..),ExpTF(..),ArrayTF(..),ObjTF(..),TokOrObj(..),TestIndexesT(..),TestIndexesO(..),ValA(..),
                                         testFunc,forAll,mkEntries,anyCase,litCase,testS,testF,mkFunc,funcNamesLit,funcNamesNoLit,constM,orCase,findWithPosAndType,allUniquePos,allUniquePosO)
import Eval.MultiPassEvalTestUtils      (usesFuncE)
import Parser.MonolithicParserTestUtils (IdTA(..),ExpTA(..),StrTA(..),NumTA(..),BoolTA(..),NullTA(..),P(..),W(..),un,uns)

{-| Number of args validation -}
prop_NbArgs (NonNegative n) (NonNegative m) (IdTA (IdT p w name)) = let (nbParams,nbArgs) = (n `mod` 1000,m `mod` 1000)  in nbParams /= nbArgs ==>
  let fs = [(name,(replicate nbParams null,Func $ const $ return $ NullO p))]
  in  Left (InvalidNbOfArgs p name nbParams nbArgs) == withFuncs fs applyFunc (mkFunc p name $ replicate nbArgs   $ NullT p w) &&
      Right (NullO p)                               == withFuncs fs applyFunc (mkFunc p name $ replicate nbParams $ NullT p w)

{-| Combinators -}
-- Or
prop_OrLit (TestToks es) (TestIndexesT indexes rest) = orCase es [array,obj,str,num,bool,null] indexes rest testS
prop_OrObj (TableOA t) (PlotOA p) (ArrayOA a) (ObjOA o) (StrOA s) (NumOA nb) (BoolOA b) (NullOA nu) (TestIndexesO indexes rest)
  = orCase [t,p,a,o,s,nb,b,nu] [table,plot,array,obj,str,num,bool,null] indexes rest Right

prop_ArrayOfLit :: P -> (W,W) -> [ExpTS] -> ValA ExpToken -> Bool
prop_ArrayOfLit p w ts (ValA _ v) = caseArrayOf (allUniquePos $ uns ts) where
  caseArrayOf es = let arr = ArrayT (un p) (un w) es in case withFuncs [] (arrayOf v) arr of
    r@(Right _)                        -> r == testS arr
    l@(Left (TypeMismatch pos _ actT)) -> let Just e = findWithPosAndType pos actT es; es' = es \\ [e]
                                          in  l == withFuncs [] v e && caseArrayOf es'

prop_ArrayOfObj :: P -> [ExpOA] -> ValA ExpObj -> Bool
prop_ArrayOfObj p ts (ValA _ v) = caseArrayOf (allUniquePosO $ uns ts) where
  caseArrayOf es = let arr = ArrayO (un p) es in case withFuncs [] (arrayOf v) arr of
    Right a                            -> a == arr
    l@(Left (TypeMismatch pos _ actT)) -> let Just e = findWithPosAndType pos actT es; es' = es \\ [e]
                                          in  l == withFuncs [] v e && caseArrayOf es'

-- Any type (can't fail)
prop_MarshallAnyLit (ExpTS e)                    = testS e == withFuncs [] any e
prop_MarshallAnyObj (ExpOA e)                    = Right e == withFuncs [] any e
prop_MarshallAnyFunc (TestObjs os) (TestToks es) = forAll (mkEntries [] es os) (anyCase os es)

{-| Basic types without literals -}
-- Any type without literal
prop_ErrorMarshallNoLitLit (ExpTS e)                     = Left (TypeMismatch (getP e) noLitType (getT e)) == withFuncs [] noLit e
prop_ErrorMarshallNoLitFunc (TestObjs os) (TestToks es)  = 
  forAll (mkEntries funcNamesNoLit es os) $ \(name,t,e) -> Left (TypeMismatch (getP e) noLitType t)        == withFuncs [] noLit (testFunc os es (getP e) name) 

prop_MarshallNoLitObj  (ExpOA e)                       = isTable e || isPlot e ==> Right e == withFuncs [] noLit e
prop_MarshallNoLitFunc (TestObjs os) (TestToks es)     =
  forAll (mkEntries funcNamesLit es os) $ \(name,_,e) ->                           Right e == liftM MkObj (withFuncs [] noLit (testFunc os es (getP e) name))

-- Table
prop_ErrorMarshallTableObj  (ExpOA e)                   = not (isTable e) ==> Left (TypeMismatch (getP e) Table (getT e)) == withFuncs [] table e
prop_ErrorMarshallTableFunc (TestObjs os) (TestToks es) = 
  forAll (mkEntries ["tableTestF"] es os) $ \(name,t,e) ->                    Left (TypeMismatch (getP e) Table t)        == withFuncs [] table (testFunc os es (getP e) name) 

prop_MarshallTableObj  (TableOA t)                       = Right t == withFuncs [] table t
prop_MarshallTableFunc (TestObjs os@[t,_]) (TestToks es) = Right t == withFuncs [] table (testFunc os es (getP t) "tableTestF")

-- Plot
prop_ErrorMarshallPlotObj  (ExpOA e)                   = not (isPlot e) ==> Left (TypeMismatch (getP e) Plot (getT e)) == withFuncs [] plot e
prop_ErrorMarshallPlotFunc (TestObjs os) (TestToks es) = 
  forAll (mkEntries ["plotTestF"] es os) $ \(name,t,e) ->                   Left (TypeMismatch (getP e) Plot t)        == withFuncs [] plot (testFunc os es (getP e) name) 

prop_MarshallPlotObj  (PlotOA p)                        = Right p == withFuncs [] plot p
prop_MarshallPlotFunc (TestObjs os@[_,p]) (TestToks es) = Right p == withFuncs [] plot (testFunc os es (getP p) "plotTestF")

{-| Basic types with literals -}
-- Any type with literal
prop_ErrorMarshallLitObj (ExpOA e)                    = isTable e || isPlot e ==> Left (TypeMismatch (getP e) litType (getT e)) == withFuncs [] lit e
prop_ErrorMarshallLitFunc (TestObjs os) (TestToks es) = 
  forAll (mkEntries funcNamesLit es os) $ \(name,t,e) ->                          Left (TypeMismatch (getP e) litType t)        == withFuncs [] lit (testFunc os es (getP e) name) 

prop_MarshallLitLit  (ExpTS e)                   = testS e == withFuncs [] lit e
prop_MarshallLitFunc (TestObjs os) (TestToks es) = forAll (mkEntries funcNamesNoLit es os) (litCase os es)

-- Array
prop_ErrorMarshallArrayLit  (ExpTS e)                   = not (isArray e || isVar e || isFunc e) ==> Left (TypeMismatch (getP e) Arr (getT e)) == withFuncs [] array e
prop_ErrorMarshallArrayObj  (ExpOA e)                   = not (isArray e)                        ==> Left (TypeMismatch (getP e) Arr (getT e)) == withFuncs [] array e
prop_ErrorMarshallArrayFunc (TestObjs os) (TestToks es) = 
  forAll (mkEntries ["arrayTestF"] es os) $ \(name,t,e) ->                                           Left (TypeMismatch (getP e) Arr t)        == withFuncs [] array (testFunc os es (getP e) name) 

prop_MarshallArrayLit  (ArrayTS a)                               = testS a == withFuncs [] array a
prop_MarshallArrayObj  (ArrayOA a)                               = Right a == withFuncs [] array a
prop_MarshallArrayFunc (TestObjs os) (TestToks es@[a,_,_,_,_,_]) = testS a == withFuncs [] array (testFunc os es (getP a) "arrayTestF")

-- Obj
prop_ErrorMarshallObjLit  (ExpTS e)                   = not (isObj e || isVar e || isFunc e) ==> Left (TypeMismatch (getP e) Obj (getT e)) == withFuncs [] obj e
prop_ErrorMarshallObjObj  (ExpOA e)                   = not (isObj e)                        ==> Left (TypeMismatch (getP e) Obj (getT e)) == withFuncs [] obj e
prop_ErrorMarshallObjFunc (TestObjs os) (TestToks es) = 
  forAll (mkEntries ["objTestF"] es os) $ \(name,t,e) ->                                         Left (TypeMismatch (getP e) Obj t)        == withFuncs [] obj (testFunc os es (getP e) name) 

prop_MarshallObjLit  (ObjTS o)                                 = testS o == withFuncs [] obj o
prop_MarshallObjObj  (ObjOA o)                                 = Right o == withFuncs [] obj o
prop_MarshallObjFunc (TestObjs os) (TestToks es@[_,o,_,_,_,_]) = testS o == withFuncs [] obj (testFunc os es (getP o) "objTestF")

-- Str
prop_ErrorMarshallStrLit  (ExpTA e)                   = not (isStr e || isVar e || isFunc e) ==> Left (TypeMismatch (getP e) Str (getT e)) == withFuncs [] str e
prop_ErrorMarshallStrObj  (ExpOA e)                   = not (isStr e)                        ==> Left (TypeMismatch (getP e) Str (getT e)) == withFuncs [] str e
prop_ErrorMarshallStrFunc (TestObjs os) (TestToks es) = 
  forAll (mkEntries ["strTestF"] es os) $ \(name,t,e) ->                                         Left (TypeMismatch (getP e) Str t)        == withFuncs [] str (testFunc os es (getP e) name) 

prop_MarshallStrLit  (StrTA s)                                 = testS s == withFuncs [] str s
prop_MarshallStrObj  (StrOA s)                                 = Right s == withFuncs [] str s
prop_MarshallStrFunc (TestObjs os) (TestToks es@[_,_,s,_,_,_]) = testS s == withFuncs [] str (testFunc os es (getP s) "strTestF")

-- Num
prop_ErrorMarshallNumLit  (ExpTA e)                   = not (isNum e || isVar e || isFunc e) ==> Left (TypeMismatch (getP e) Num (getT e)) == withFuncs [] num e
prop_ErrorMarshallNumObj  (ExpOA e)                   = not (isNum e)                        ==> Left (TypeMismatch (getP e) Num (getT e)) == withFuncs [] num e
prop_ErrorMarshallNumFunc (TestObjs os) (TestToks es) = 
  forAll (mkEntries ["numTestF"] es os) $ \(name,t,e) ->                                         Left (TypeMismatch (getP e) Num t)        == withFuncs [] num (testFunc os es (getP e) name) 

prop_MarshallNumLit  (NumTA _ n)                               = testS n == withFuncs [] num n
prop_MarshallNumObj  (NumOA   n)                               = Right n == withFuncs [] num n
prop_MarshallNumFunc (TestObjs os) (TestToks es@[_,_,_,n,_,_]) = testS n == withFuncs [] num (testFunc os es (getP n) "numTestF")

-- Bool
prop_ErrorMarshallBoolLit  (ExpTA e)                   = not (isBool e || isVar e || isFunc e) ==> Left (TypeMismatch (getP e) Bool (getT e)) == withFuncs [] bool e
prop_ErrorMarshallBoolObj  (ExpOA e)                   = not (isBool e)                        ==> Left (TypeMismatch (getP e) Bool (getT e)) == withFuncs [] bool e
prop_ErrorMarshallBoolFunc (TestObjs os) (TestToks es) = 
  forAll (mkEntries ["boolTestF"] es os) $ \(name,t,e) ->                                          Left (TypeMismatch (getP e) Bool t)        == withFuncs [] bool (testFunc os es (getP e) name) 

prop_MarshallBoolLit  (BoolTA b)                                = testS b == withFuncs [] bool b
prop_MarshallBoolObj  (BoolOA b)                                = Right b == withFuncs [] bool b
prop_MarshallBoolFunc (TestObjs os) (TestToks es@[_,_,_,_,b,_]) = testS b == withFuncs [] bool (testFunc os es (getP b) "boolTestF")

-- Null
prop_ErrorMarshallNullLit  (ExpTA e)        = not (isNull e || isVar e || isFunc e) ==> Left (TypeMismatch (getP e) Null (getT e)) == withFuncs [] null e
prop_ErrorMarshallNullObj  (ExpOA e)        = not (isNull e)                        ==> Left (TypeMismatch (getP e) Null (getT e)) == withFuncs [] null e
prop_ErrorMarshallNullFunc (TestObjs os) (TestToks es) = 
  forAll (mkEntries ["nullTestF"] es os) $ \(name,t,e) ->                               Left (TypeMismatch (getP e) Null t)        == withFuncs [] null (testFunc os es (getP e) name) 

prop_MarshallNullLit  (NullTA n)                                = testS n == withFuncs [] null n
prop_MarshallNullObj  (NullOA n)                                = Right n == withFuncs [] null n
prop_MarshallNullFunc (TestObjs os) (TestToks es@[_,_,_,_,_,n]) = testS n == withFuncs [] null (testFunc os es (getP n) "nullTestF")

{-| Literals containing function calls -}
prop_MarshallFuncsLit   (ExpTF   e o (IdT _ _ i)) = usesFuncE i e ==> testF fs e == withFuncs fs lit   e where fs = [(i,([],Func $ constM o))]
prop_MarshallFuncsArray (ArrayTF e o (IdT _ _ i)) = usesFuncE i e ==> testF fs e == withFuncs fs array e where fs = [(i,([],Func $ constM o))]
prop_MarshallFuncsObj   (ObjTF   e o (IdT _ _ i)) = usesFuncE i e ==> testF fs e == withFuncs fs obj   e where fs = [(i,([],Func $ constM o))]



