{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns #-}
module Eval.FunctionEvalTest where

import Prelude hiding                   (any,null)

import qualified Prelude as P           (any,null)

import Control.Arrow                    (second)
import Control.Monad                    (liftM)
import Data.Eval                        (Func(..),TypeValidator(..))
import Data.EvalError                   (EvalError(..))
import Data.ExpObj                      (Type(..),ExpObj(..))
import Data.ExpToken                    (PairToken(..),IdToken(..),ExpToken(..))
import Eval.Function                    (Marshallable(..),table,plot,array,obj,str,num,bool,null,any,noLit,noLitType,lit,litType,nonEmpty,args,withFuncs,evalError)
import Eval.FunctionEvalTestUtils1      (TestToks(..),TestObjs(..),ExpOA(..),TableOA(..),PlotOA(..),ArrayOA(..),ObjOA(..),StrOA(..),NumOA(..),BoolOA(..),NullOA(..),
                                         ExpTS(..),ArrayTS(..),ObjTS(..),ArrayTF(..),ObjTF(..),testFunc,forAll,testS,testF,mkFunc,funcNamesLit,funcNamesNoLit)
import Eval.FunctionEvalTestUtils2      (Is(..),InvalidArgsNb(..),TokOrObj(..),TestIndexesT(..),TestIndexesO(..),ValA(..),ArgErrorA(..),mkEntries,anyCase,litCase,orCase,caseArrayOf,caseObjOf)
import Eval.MultiPassEvalTestUtils      (usesFuncE,w2,p0)
import Parser.MonolithicParserTestUtils (StrTA(..),NumTA(..),BoolTA(..),NullTA(..),P(..),un,uns)
import Test.Framework                   (TestSuite,NonNegative(..),makeTestSuite,makeQuickCheckTest,makeLoc,qcAssertion,(==>))

{-| Number of args validation -}
prop_NbArgs (InvalidArgsNb nbParams nbArgs p name goodFunc badFunc fs) = Left (InvalidNbOfArgs p name nbParams nbArgs) == withFuncs fs any badFunc && Right (NullO p) == withFuncs fs any goodFunc

{-| Combinators -}
-- Or
prop_OrLit (TestIndexesT es  indexes rest) = orCase es [           array,obj,str,num,bool,null] indexes rest testS
prop_OrObj (TestIndexesO os  indexes rest) = orCase os [table,plot,array,obj,str,num,bool,null] indexes rest Right

prop_ArrayOfLit :: P -> [ExpTS] -> ValA ExpToken -> Bool
prop_ArrayOfObj :: P -> [ExpOA] -> ValA ExpObj   -> Bool
prop_ArrayOfLit p ts (ValA _ v) = caseArrayOf (un p) v (uns ts) testS $ flip ArrayT w2
prop_ArrayOfObj p ts (ValA _ v) = caseArrayOf (un p) v (uns ts) Right ArrayO

prop_ObjOfLit :: P -> [(String,ExpTS)] -> ValA ExpToken -> Bool
prop_ObjOfObj :: P -> [(String,ExpOA)] -> ValA ExpObj   -> Bool
prop_ObjOfLit p ps (ValA _ v) = caseObjOf (un p) v (map (second un) ps) testS $ \pos -> ObjT pos w2 . map (\(x,y) -> PairT (IdT p0 w2 x) y)
prop_ObjOfObj p ps (ValA _ v) = caseObjOf (un p) v (map (second un) ps) Right ObjO

prop_NonEmptyLit (TestToks [a@(ArrayT pa wa es), o@(ObjT po wo pairs), s@(StrT ps ws v), nb, b, nu]) =
  let f = withFuncs [] (nonEmpty any) in not (P.null es) && not (P.null pairs) && not (P.null v) ==>
    Left (IllegalEmpty pa) == f (ArrayT pa wa [])  &&
    Left (IllegalEmpty po) == f (ObjT   po wo [])  &&
    Left (IllegalEmpty ps) == f (StrT   ps ws [])  &&
    testS a  == f a  && testS o  == f o  && testS s  == f s &&
    testS nb == f nb && testS b  == f b  && testS nu == f nu

prop_NonEmptyObj n (TestObjs [t@(TableO pt ess oT), p@(PlotO pp pairsP oP)])
                   (ArrayOA a@(ArrayO pa es)) (ObjOA o@(ObjO po pairsO)) (StrOA s@(StrO ps v)) (NumOA nb) (BoolOA b) (NullOA nu) =
  let f = withFuncs [] (nonEmpty any) in P.any (not . P.null) ess && not (P.null pairsP) && not (P.null es) && not (P.null pairsO) && not (P.null v) ==>
    Left (IllegalEmpty pt) == f (TableO pt (replicate (n`mod`1000) []) oT) &&
    Left (IllegalEmpty pp) == f (PlotO  pp [] oP) &&
    Left (IllegalEmpty pa) == f (ArrayO pa [])    &&
    Left (IllegalEmpty po) == f (ObjO   po [])    &&
    Left (IllegalEmpty ps) == f (StrO   ps [])    &&
    Right t  == f t  && Right p  == f p  &&
    Right a  == f a  && Right o  == f o  && Right s  == f s &&
    Right nb == f nb && Right b  == f b  && Right nu == f nu

prop_NonEmptyFunc n name (P pos)
                 (TestObjs [t@(TableO _ ess oT), p@(PlotO _ pairsP oP)])
                 (ArrayOA a@(ArrayO _ es)) (ObjOA o@(ObjO _ pairsO)) (StrOA s@(StrO _ v)) (NumOA nb) (BoolOA b) (NullOA nu) =
  let f x = withFuncs [(name,([],Func $ \_ _ -> return x))] (nonEmpty any) $ mkFunc pos name []
  in  P.any (not . P.null) ess && not (P.null pairsP) && not (P.null es) && not (P.null pairsO) && not (P.null v) ==>
    Left (IllegalEmpty pos) == f (TableO pos (replicate (n`mod`1000) []) oT) &&
    Left (IllegalEmpty pos) == f (PlotO  pos [] oP) &&
    Left (IllegalEmpty pos) == f (ArrayO pos [])    &&
    Left (IllegalEmpty pos) == f (ObjO   pos [])    &&
    Left (IllegalEmpty pos) == f (StrO   pos [])    &&
    Right t  == f t  && Right p  == f p  &&
    Right a  == f a  && Right o  == f o  && Right s  == f s &&
    Right nb == f nb && Right b  == f b  && Right nu == f nu

prop_ArgError (NonNegative n') name (ArgErrorA e) = Left (ArgError n name e) == withFuncs (map args fs) any (mkFunc p0 name $ replicate (n+1) (NullT p0 w2))
  where fs = [(name,(replicate n any ++ [TypeVal $ \_ -> evalError e], f))]
        f = Func $ \_ -> error "FunctionEvalTest::prop_ArgError [Should not be called]"
        n = n' `mod` 1000

{-| Basic type validators -}
-- Any type (can't fail)
prop_MarshallAnyLit (ExpTS e)                    = testS e == withFuncs [] any e
prop_MarshallAnyObj (ExpOA e)                    = Right e == withFuncs [] any e
prop_MarshallAnyFunc (TestObjs os) (TestToks es) = forAll (mkEntries [] es os) (anyCase os es)

{-| Basic types without literals -}
-- Any type without literal
prop_ErrorMarshallNoLitLit (ExpTS e)                     = Left (TypeMismatch (getPos e) noLitType (getType e)) == withFuncs [] noLit e
prop_ErrorMarshallNoLitFunc (TestObjs os) (TestToks es)  =
  forAll (mkEntries funcNamesNoLit es os) $ \(name,t,e) -> Left (TypeMismatch (getPos e) noLitType t)           == withFuncs [] noLit (testFunc os es (getPos e) name)

prop_MarshallNoLitObj  (ExpOA e)                       = isTable e || isPlot e ==> Right e == withFuncs [] noLit e
prop_MarshallNoLitFunc (TestObjs os) (TestToks es)     =
  forAll (mkEntries funcNamesLit es os) $ \(name,_,e) ->                           Right e == liftM MkObj (withFuncs [] noLit (testFunc os es (getPos e) name))

-- Table
prop_ErrorMarshallTableObj  (ExpOA e)                   = not (isTable e) ==> Left (TypeMismatch (getPos e) Table (getType e)) == withFuncs [] table e
prop_ErrorMarshallTableFunc (TestObjs os) (TestToks es) =
  forAll (mkEntries ["tableTestF"] es os) $ \(name,t,e) ->                    Left (TypeMismatch (getPos e) Table t)           == withFuncs [] table (testFunc os es (getPos e) name)

prop_MarshallTableObj  (TableOA t)                       = Right t == withFuncs [] table t
prop_MarshallTableFunc (TestObjs os@[t,_]) (TestToks es) = Right t == withFuncs [] table (testFunc os es (getPos t) "tableTestF")

-- Plot
prop_ErrorMarshallPlotObj  (ExpOA e)                   = not (isPlot e) ==> Left (TypeMismatch (getPos e) Plot (getType e)) == withFuncs [] plot e
prop_ErrorMarshallPlotFunc (TestObjs os) (TestToks es) =
  forAll (mkEntries ["plotTestF"] es os) $ \(name,t,e) ->                   Left (TypeMismatch (getPos e) Plot t)           == withFuncs [] plot (testFunc os es (getPos e) name)

prop_MarshallPlotObj  (PlotOA p)                        = Right p == withFuncs [] plot p
prop_MarshallPlotFunc (TestObjs os@[_,p]) (TestToks es) = Right p == withFuncs [] plot (testFunc os es (getPos p) "plotTestF")

{-| Basic types with literals -}
-- Any type with literal
prop_ErrorMarshallLitObj (ExpOA e)                    = isTable e || isPlot e ==> Left (TypeMismatch (getPos e) litType (getType e)) == withFuncs [] lit e
prop_ErrorMarshallLitFunc (TestObjs os) (TestToks es) =
  forAll (mkEntries funcNamesLit es os) $ \(name,t,e) ->                          Left (TypeMismatch (getPos e) litType t)           == withFuncs [] lit (testFunc os es (getPos e) name)

prop_MarshallLitLit  (ExpTS e)                   = testS e == withFuncs [] lit e
prop_MarshallLitFunc (TestObjs os) (TestToks es) = forAll (mkEntries funcNamesNoLit es os) (litCase os es)

-- Array
prop_ErrorMarshallArrayLit  (ExpTS e)                   = not (isArray e || isVar e || isFunc e) ==> Left (TypeMismatch (getPos e) Arr (getType e)) == withFuncs [] array e
prop_ErrorMarshallArrayObj  (ExpOA e)                   = not (isArray e)                        ==> Left (TypeMismatch (getPos e) Arr (getType e)) == withFuncs [] array e
prop_ErrorMarshallArrayFunc (TestObjs os) (TestToks es) =
  forAll (mkEntries ["arrayTestF"] es os) $ \(name,t,e) ->                                           Left (TypeMismatch (getPos e) Arr t)           == withFuncs [] array (testFunc os es (getPos e) name)

prop_MarshallArrayLit  (ArrayTS a)                               = testS a == withFuncs [] array a
prop_MarshallArrayObj  (ArrayOA a)                               = Right a == withFuncs [] array a
prop_MarshallArrayFunc (TestObjs os) (TestToks es@[a,_,_,_,_,_]) = testS a == withFuncs [] array (testFunc os es (getPos a) "arrayTestF")

-- Obj
prop_ErrorMarshallObjLit  (ExpTS e)                   = not (isObj e || isVar e || isFunc e) ==> Left (TypeMismatch (getPos e) Obj (getType e)) == withFuncs [] obj e
prop_ErrorMarshallObjObj  (ExpOA e)                   = not (isObj e)                        ==> Left (TypeMismatch (getPos e) Obj (getType e)) == withFuncs [] obj e
prop_ErrorMarshallObjFunc (TestObjs os) (TestToks es) =
  forAll (mkEntries ["objTestF"] es os) $ \(name,t,e) ->                                         Left (TypeMismatch (getPos e) Obj t)           == withFuncs [] obj (testFunc os es (getPos e) name)

prop_MarshallObjLit  (ObjTS o)                                 = testS o == withFuncs [] obj o
prop_MarshallObjObj  (ObjOA o)                                 = Right o == withFuncs [] obj o
prop_MarshallObjFunc (TestObjs os) (TestToks es@[_,o,_,_,_,_]) = testS o == withFuncs [] obj (testFunc os es (getPos o) "objTestF")

-- Str
prop_ErrorMarshallStrLit  (ExpTS e)                   = not (isStr e || isVar e || isFunc e) ==> Left (TypeMismatch (getPos e) Str (getType e)) == withFuncs [] str e
prop_ErrorMarshallStrObj  (ExpOA e)                   = not (isStr e)                        ==> Left (TypeMismatch (getPos e) Str (getType e)) == withFuncs [] str e
prop_ErrorMarshallStrFunc (TestObjs os) (TestToks es) =
  forAll (mkEntries ["strTestF"] es os) $ \(name,t,e) ->                                         Left (TypeMismatch (getPos e) Str t)           == withFuncs [] str (testFunc os es (getPos e) name)

prop_MarshallStrLit  (StrTA s)                                 = testS s == withFuncs [] str s
prop_MarshallStrObj  (StrOA s)                                 = Right s == withFuncs [] str s
prop_MarshallStrFunc (TestObjs os) (TestToks es@[_,_,s,_,_,_]) = testS s == withFuncs [] str (testFunc os es (getPos s) "strTestF")

-- Num
prop_ErrorMarshallNumLit  (ExpTS e)                   = not (isNum e || isVar e || isFunc e) ==> Left (TypeMismatch (getPos e) Num (getType e)) == withFuncs [] num e
prop_ErrorMarshallNumObj  (ExpOA e)                   = not (isNum e)                        ==> Left (TypeMismatch (getPos e) Num (getType e)) == withFuncs [] num e
prop_ErrorMarshallNumFunc (TestObjs os) (TestToks es) =
  forAll (mkEntries ["numTestF"] es os) $ \(name,t,e) ->                                         Left (TypeMismatch (getPos e) Num t)           == withFuncs [] num (testFunc os es (getPos e) name)

prop_MarshallNumLit  (NumTA _ n)                               = testS n == withFuncs [] num n
prop_MarshallNumObj  (NumOA   n)                               = Right n == withFuncs [] num n
prop_MarshallNumFunc (TestObjs os) (TestToks es@[_,_,_,n,_,_]) = testS n == withFuncs [] num (testFunc os es (getPos n) "numTestF")

-- Bool
prop_ErrorMarshallBoolLit  (ExpTS e)                   = not (isBool e || isVar e || isFunc e) ==> Left (TypeMismatch (getPos e) Bool (getType e)) == withFuncs [] bool e
prop_ErrorMarshallBoolObj  (ExpOA e)                   = not (isBool e)                        ==> Left (TypeMismatch (getPos e) Bool (getType e)) == withFuncs [] bool e
prop_ErrorMarshallBoolFunc (TestObjs os) (TestToks es) =
  forAll (mkEntries ["boolTestF"] es os) $ \(name,t,e) ->                                          Left (TypeMismatch (getPos e) Bool t)           == withFuncs [] bool (testFunc os es (getPos e) name)

prop_MarshallBoolLit  (BoolTA b)                                = testS b == withFuncs [] bool b
prop_MarshallBoolObj  (BoolOA b)                                = Right b == withFuncs [] bool b
prop_MarshallBoolFunc (TestObjs os) (TestToks es@[_,_,_,_,b,_]) = testS b == withFuncs [] bool (testFunc os es (getPos b) "boolTestF")

-- Null
prop_ErrorMarshallNullLit  (ExpTS e)        = not (isNull e || isVar e || isFunc e) ==> Left (TypeMismatch (getPos e) Null (getType e)) == withFuncs [] null e
prop_ErrorMarshallNullObj  (ExpOA e)        = not (isNull e)                        ==> Left (TypeMismatch (getPos e) Null (getType e)) == withFuncs [] null e
prop_ErrorMarshallNullFunc (TestObjs os) (TestToks es) =
  forAll (mkEntries ["nullTestF"] es os) $ \(name,t,e) ->                               Left (TypeMismatch (getPos e) Null t)           == withFuncs [] null (testFunc os es (getPos e) name)

prop_MarshallNullLit  (NullTA n)                                = testS n == withFuncs [] null n
prop_MarshallNullObj  (NullOA n)                                = Right n == withFuncs [] null n
prop_MarshallNullFunc (TestObjs os) (TestToks es@[_,_,_,_,_,n]) = testS n == withFuncs [] null (testFunc os es (getPos n) "nullTestF")

{-| Literals containing function calls -}
prop_MarshallFuncsArray (ArrayTF e o (IdT _ _ i)) = usesFuncE i e ==> testF fs e == withFuncs fs array e where fs = [(i,([],Func $ \_ _ -> return o))]
prop_MarshallFuncsObj   (ObjTF   e o (IdT _ _ i)) = usesFuncE i e ==> testF fs e == withFuncs fs obj   e where fs = [(i,([],Func $ \_ _ -> return o))]

