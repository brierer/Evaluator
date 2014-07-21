{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module Prop.Eval.FunctionEvalUtils where

import Control.Arrow hiding (arr)
import Data.List     hiding (any)
import Prelude       hiding (any)

import qualified Prelude as P

import Control.Applicative
import Control.Monad.State
import Data.Eval
import Data.EvalError
import Data.HasPos
import Data.ExpToken
import Data.ExpObj
import Data.Type
import Eval.Function
import Eval.MultiPass
import Eval.Type
import Test.Framework

import Common.Parser.MonolithicParserUtils

import Prop.Eval.MultiPassEvalUtils
import Prop.Eval.TypeEvalUtils
import Prop.Parser.MonolithicParserUtils

import Unit.Eval.TypeEvalUtils
import Unit.Parser.MonolithicParserUtils

class Is a where
  isTable :: a -> Bool
  isPlot  :: a -> Bool
  isArr   :: a -> Bool
  isObj   :: a -> Bool
  isStr   :: a -> Bool
  isNum   :: a -> Bool
  isBool  :: a -> Bool
  isNull  :: a -> Bool

instance Is ExpToken where
  isTable _ = error "FunctionEvalUtils::isTable<ExpToken> [Should not be called]"
  isPlot  _ = error "FunctionEvalUtils::isPlot <ExpToken> [Should not be called]"

  isArr (ArrT{})   = True ; isArr _   = False
  isObj (ObjT{})   = True;  isObj _   = False
  isStr (StrT{})   = True;  isStr _   = False
  isNum (NumT{})   = True;  isNum _   = False
  isBool (BoolT{}) = True;  isBool _  = False
  isNull (NullT{}) = True;  isNull _  = False
  
instance Is ExpObj where
  isTable (TableO{}) = True;  isTable _ = False
  isPlot  (PlotO{})  = True;  isPlot  _ = False
  isArr   (ArrO{})   = True;  isArr   _ = False
  isObj   (ObjO{})   = True;  isObj   _ = False
  isStr   (StrO{})   = True;  isStr   _ = False
  isNum   (NumO{})   = True;  isNum   _ = False
  isBool  (BoolO{})  = True;  isBool  _ = False
  isNull  (NullO{})  = True;  isNull  _ = False

class HasElems a where
  elemsOf :: a -> [a]
  filterElems :: (a -> Bool) -> a -> a
  
instance HasElems ExpToken where 
  elemsOf (ArrT _ _ es) = es
  elemsOf (ObjT _ _ ps) = map pairVal ps
  
  filterElems pr (ArrT p w es) = ArrT p w $ filter pr es
  filterElems pr (ObjT p w ps) = ObjT p w $ filter (pr.pairVal) ps
  
instance HasElems ExpObj where 
  elemsOf (ArrO _ es) = es
  elemsOf (ObjO _ ps) = map snd ps
  
  filterElems pr (ArrO p es) = ArrO p $ filter pr es
  filterElems pr (ObjO p ps) = ObjO p $ filter (pr.snd) ps

data NbArgs = NbArgs Pos String Int Int deriving (Show)
instance Arbitrary NbArgs where
  arbitrary               = mNbArgs  arbitrary  arbitrary   arbitrary   arbitrary
  shrink (NbArgs p s n m) = mNbArgs (tShrink p) (shrink s) (tShrink n) (tShrink m)
mNbArgs = liftMF4 NbArgs un id un un

data ArrLitFailure = ArrLitFailure [ExpToken] ExpObj Int deriving (Show)
instance Arbitrary   ArrLitFailure where arbitrary = sized1 tall; shrink (ArrLitFailure es _ _) = mLitFailure (tShrinks es) id       isArr ArrLitFailure
instance Tall        ArrLitFailure where                                                 tall n = mLitFailure (talls n)     elements isArr ArrLitFailure

data ObjLitFailure = ObjLitFailure [ExpToken] ExpObj Int deriving (Show)
instance Arbitrary   ObjLitFailure where arbitrary = sized1 tall; shrink (ObjLitFailure es _ _) = mLitFailure (tShrinks es) id       isObj ObjLitFailure
instance Tall        ObjLitFailure where                                                 tall n = mLitFailure (talls n)     elements isObj ObjLitFailure

data StrLitFailure = StrLitFailure [ExpToken] ExpObj Int deriving (Show)
instance Arbitrary   StrLitFailure where arbitrary = sized1 tall; shrink (StrLitFailure es _ _) = mLitFailure (tShrinks es) id       isStr StrLitFailure
instance Tall        StrLitFailure where                                                 tall n = mLitFailure (talls n)     elements isStr StrLitFailure

data NumLitFailure = NumLitFailure [ExpToken] ExpObj Int deriving (Show)
instance Arbitrary   NumLitFailure where arbitrary = sized1 tall; shrink (NumLitFailure es _ _) = mLitFailure (tShrinks es) id       isNum NumLitFailure
instance Tall        NumLitFailure where                                                 tall n = mLitFailure (talls n)     elements isNum NumLitFailure

data BoolLitFailure = BoolLitFailure [ExpToken] ExpObj Int deriving (Show)
instance Arbitrary    BoolLitFailure where arbitrary = sized1 tall; shrink (BoolLitFailure es _ _) = mLitFailure (tShrinks es) id       isBool BoolLitFailure
instance Tall         BoolLitFailure where                                                  tall n = mLitFailure (talls n)     elements isBool BoolLitFailure

data NullLitFailure = NullLitFailure [ExpToken] ExpObj Int deriving (Show)
instance Arbitrary    NullLitFailure where arbitrary = sized1 tall; shrink (NullLitFailure es _ _) = mLitFailure (tShrinks es) id       isNull NullLitFailure
instance Tall         NullLitFailure where                                                  tall n = mLitFailure (talls n)     elements isNull NullLitFailure

data ArrOfLitFailure = ArrOfLitFailure [ExpToken] ExpObj Int Type deriving (Show)
instance Arbitrary     ArrOfLitFailure where arbitrary = sized1 tall; shrink (ArrOfLitFailure es _ _ _) = mOfLitFailure (tShrinks es :: []  [ArrTS]) id       id       ArrOfLitFailure
instance Tall          ArrOfLitFailure where                                                     tall n = mOfLitFailure (talls n     :: Gen [ArrTS]) elements elements ArrOfLitFailure

data ObjOfLitFailure = ObjOfLitFailure [ExpToken] ExpObj Int Type deriving (Show)
instance Arbitrary     ObjOfLitFailure where arbitrary = sized1 tall; shrink (ObjOfLitFailure es _ _ _) = mOfLitFailure (tShrinks es :: []  [ObjTS]) id       id       ObjOfLitFailure
instance Tall          ObjOfLitFailure where                                                     tall n = mOfLitFailure (talls n     :: Gen [ObjTS]) elements elements ObjOfLitFailure

data OrLitFailure = OrLitFailure [ExpToken] ExpObj Int Type deriving (Show)
instance Arbitrary  OrLitFailure where arbitrary = sized1 tall; shrink (OrLitFailure es _ _ _) = mOrLitFailure (tShrinks es) id       id
instance Tall       OrLitFailure where                                                  tall n = mOrLitFailure (talls n)     elements elements

data TableFuncFailure = TableFuncFailure [(ExpToken,ExpObj)] ExpObj Int deriving (Show)
instance Arbitrary      TableFuncFailure where arbitrary = sized1 tall; shrink (TableFuncFailure ts _ _) = mFuncFailure (tShrinks ts) id       isTable TableFuncFailure 
instance Tall           TableFuncFailure where                                                    tall n = mFuncFailure (talls n)     elements isTable TableFuncFailure

data PlotFuncFailure = PlotFuncFailure [(ExpToken,ExpObj)] ExpObj Int deriving (Show)
instance Arbitrary     PlotFuncFailure where arbitrary = sized1 tall; shrink (PlotFuncFailure ts _ _) = mFuncFailure (tShrinks ts) id       isPlot PlotFuncFailure 
instance Tall          PlotFuncFailure where                                                   tall n = mFuncFailure (talls n)     elements isPlot PlotFuncFailure

data ArrFuncFailure = ArrFuncFailure [(ExpToken,ExpObj)] ExpObj Int deriving (Show)
instance Arbitrary    ArrFuncFailure where arbitrary = sized1 tall; shrink (ArrFuncFailure ts _ _) = mFuncFailure (tShrinks ts) id       isArr ArrFuncFailure 
instance Tall         ArrFuncFailure where                                                  tall n = mFuncFailure (talls n)     elements isArr ArrFuncFailure

data ObjFuncFailure = ObjFuncFailure [(ExpToken,ExpObj)] ExpObj Int deriving (Show)
instance Arbitrary    ObjFuncFailure where arbitrary = sized1 tall; shrink (ObjFuncFailure ts _ _) = mFuncFailure (tShrinks ts) id       isObj ObjFuncFailure 
instance Tall         ObjFuncFailure where                                                  tall n = mFuncFailure (talls n)     elements isObj ObjFuncFailure

data StrFuncFailure = StrFuncFailure [(ExpToken,ExpObj)] ExpObj Int deriving (Show)
instance Arbitrary    StrFuncFailure where arbitrary = sized1 tall; shrink (StrFuncFailure ts _ _) = mFuncFailure (tShrinks ts) id       isStr StrFuncFailure 
instance Tall         StrFuncFailure where                                                  tall n = mFuncFailure (talls n)     elements isStr StrFuncFailure

data NumFuncFailure = NumFuncFailure [(ExpToken,ExpObj)] ExpObj Int deriving (Show)
instance Arbitrary    NumFuncFailure where arbitrary = sized1 tall; shrink (NumFuncFailure ts _ _) = mFuncFailure (tShrinks ts) id       isNum NumFuncFailure 
instance Tall         NumFuncFailure where                                                  tall n = mFuncFailure (talls n)     elements isNum NumFuncFailure

data BoolFuncFailure = BoolFuncFailure [(ExpToken,ExpObj)] ExpObj Int deriving (Show)
instance Arbitrary     BoolFuncFailure where arbitrary = sized1 tall; shrink (BoolFuncFailure ts _ _) = mFuncFailure (tShrinks ts) id       isBool BoolFuncFailure 
instance Tall          BoolFuncFailure where                                                   tall n = mFuncFailure (talls n)     elements isBool BoolFuncFailure

data NullFuncFailure = NullFuncFailure [(ExpToken,ExpObj)] ExpObj Int deriving (Show)
instance Arbitrary     NullFuncFailure where arbitrary = sized1 tall; shrink (NullFuncFailure ts _ _) = mFuncFailure (tShrinks ts) id       isNull NullFuncFailure 
instance Tall          NullFuncFailure where                                                   tall n = mFuncFailure (talls n)     elements isNull NullFuncFailure

data ArrOfFuncFailure = ArrOfFuncFailure [(ExpToken,ExpObj)] ExpObj Int Type deriving (Show)
instance Arbitrary      ArrOfFuncFailure where arbitrary = sized1 tall; shrink (ArrOfFuncFailure ts _ _ _) = mOfFuncFailure (tShrinks ts :: []  [(FuncTA,ArrOA)]) id       id       ArrOfFuncFailure
instance Tall           ArrOfFuncFailure where                                                      tall n = mOfFuncFailure (talls (n-1) :: Gen [(FuncTA,ArrOA)]) elements elements ArrOfFuncFailure
                                                                                                                                                       
data ObjOfFuncFailure = ObjOfFuncFailure [(ExpToken,ExpObj)] ExpObj Int Type deriving (Show)                                                           
instance Arbitrary      ObjOfFuncFailure where arbitrary = sized1 tall; shrink (ObjOfFuncFailure ts _ _ _) = mOfFuncFailure (tShrinks ts :: []  [(FuncTA,ObjOA)]) id       id       ObjOfFuncFailure
instance Tall           ObjOfFuncFailure where                                                      tall n = mOfFuncFailure (talls (n-1) :: Gen [(FuncTA,ObjOA)]) elements elements ObjOfFuncFailure

data OrFuncFailure = OrFuncFailure [(ExpToken,ExpObj)] ExpObj Int Type deriving (Show)
instance Arbitrary   OrFuncFailure where arbitrary = sized1 tall; shrink (OrFuncFailure ts _ _ _) = mOrFuncFailure (tShrinks ts) id       id
instance Tall        OrFuncFailure where                                                   tall n = mOrFuncFailure (talls n)     elements elements
 
data ArrLitSuccess = ArrLitSuccess [ExpToken] ExpObj deriving (Show)
instance Arbitrary   ArrLitSuccess where arbitrary = sized1 tall; shrink (ArrLitSuccess es e) = mLitSuccess isArr ArrLitSuccess (tShrinks es) (tShrink e)
instance Tall        ArrLitSuccess where                                               tall n = mLitSuccess isArr ArrLitSuccess (talls n)     (tall n)   

data ObjLitSuccess = ObjLitSuccess [ExpToken] ExpObj deriving (Show)
instance Arbitrary   ObjLitSuccess where arbitrary = sized1 tall; shrink (ObjLitSuccess es e) = mLitSuccess isObj ObjLitSuccess (tShrinks es) (tShrink e)
instance Tall        ObjLitSuccess where                                               tall n = mLitSuccess isObj ObjLitSuccess (talls n)     (tall n)   

data StrLitSuccess = StrLitSuccess [ExpToken] ExpObj deriving (Show)
instance Arbitrary   StrLitSuccess where arbitrary = sized1 tall; shrink (StrLitSuccess es e) = mLitSuccess isStr StrLitSuccess (tShrinks es) (tShrink e)
instance Tall        StrLitSuccess where                                               tall n = mLitSuccess isStr StrLitSuccess (talls n)     (tall n)   

data NumLitSuccess = NumLitSuccess [ExpToken] ExpObj deriving (Show)
instance Arbitrary   NumLitSuccess where arbitrary = sized1 tall; shrink (NumLitSuccess es e) = mLitSuccess isNum NumLitSuccess (tShrinks es) (tShrink e)
instance Tall        NumLitSuccess where                                               tall n = mLitSuccess isNum NumLitSuccess (talls n)     (tall n)   

data BoolLitSuccess = BoolLitSuccess [ExpToken] ExpObj deriving (Show)
instance Arbitrary    BoolLitSuccess where arbitrary = sized1 tall; shrink (BoolLitSuccess es e) = mLitSuccess isBool BoolLitSuccess (tShrinks es) (tShrink e)
instance Tall         BoolLitSuccess where                                                tall n = mLitSuccess isBool BoolLitSuccess (talls n)     (tall n)   

data NullLitSuccess = NullLitSuccess [ExpToken] ExpObj deriving (Show)
instance Arbitrary    NullLitSuccess where arbitrary = sized1 tall; shrink (NullLitSuccess es e) = mLitSuccess isNull NullLitSuccess (tShrinks es) (tShrink e)
instance Tall         NullLitSuccess where                                                tall n = mLitSuccess isNull NullLitSuccess (talls n)     (tall n)   

data ArrOfLitSuccess = ArrOfLitSuccess [ExpToken] ExpObj Type deriving (Show)
instance Arbitrary     ArrOfLitSuccess where arbitrary = sized1 tall; shrink (ArrOfLitSuccess es e _) = mOfLitSuccess (tShrinks es :: []  [ArrTS]) (tShrink e) id       ArrOfLitSuccess
instance Tall          ArrOfLitSuccess where                                                   tall n = mOfLitSuccess (talls n     :: Gen [ArrTS]) (tall n)    elements ArrOfLitSuccess

data ObjOfLitSuccess = ObjOfLitSuccess [ExpToken] ExpObj Type deriving (Show)
instance Arbitrary     ObjOfLitSuccess where arbitrary = sized1 tall; shrink (ObjOfLitSuccess es e _) = mOfLitSuccess (tShrinks es :: []  [ObjTS]) (tShrink e) id       ObjOfLitSuccess
instance Tall          ObjOfLitSuccess where                                                   tall n = mOfLitSuccess (talls n     :: Gen [ObjTS]) (tall n)    elements ObjOfLitSuccess

data OrLitSuccess = OrLitSuccess [ExpToken] ExpObj  Type deriving (Show)
instance Arbitrary  OrLitSuccess where arbitrary = sized1 tall; shrink (OrLitSuccess es e _) = mOrLitSuccess (tShrinks es) (tShrink e) id
instance Tall       OrLitSuccess where                                                tall n = mOrLitSuccess (talls n)     (tall n)    elements
 
data TableFuncSuccess = TableFuncSuccess [(ExpToken,ExpObj)] ExpObj deriving (Show)
instance Arbitrary      TableFuncSuccess where arbitrary = sized1 tall; shrink (TableFuncSuccess ts e) = mFuncSuccess isTable TableFuncSuccess (tShrinks ts) (tShrink e)
instance Tall           TableFuncSuccess where                                                  tall n = mFuncSuccess isTable TableFuncSuccess (talls n)     (tall n)   

data PlotFuncSuccess = PlotFuncSuccess [(ExpToken,ExpObj)] ExpObj deriving (Show)
instance Arbitrary     PlotFuncSuccess where arbitrary = sized1 tall; shrink (PlotFuncSuccess ts e) = mFuncSuccess isPlot PlotFuncSuccess (tShrinks ts) (tShrink e)
instance Tall          PlotFuncSuccess where                                                 tall n = mFuncSuccess isPlot PlotFuncSuccess (talls n)     (tall n)   

data ArrFuncSuccess = ArrFuncSuccess [(ExpToken,ExpObj)] ExpObj deriving (Show)
instance Arbitrary    ArrFuncSuccess where arbitrary = sized1 tall; shrink (ArrFuncSuccess ts e) = mFuncSuccess isArr ArrFuncSuccess (tShrinks ts) (tShrink e)
instance Tall         ArrFuncSuccess where                                                tall n = mFuncSuccess isArr ArrFuncSuccess (talls n)     (tall n)   

data ObjFuncSuccess = ObjFuncSuccess [(ExpToken,ExpObj)] ExpObj deriving (Show)
instance Arbitrary    ObjFuncSuccess where arbitrary = sized1 tall; shrink (ObjFuncSuccess ts e) = mFuncSuccess isObj ObjFuncSuccess (tShrinks ts) (tShrink e)
instance Tall         ObjFuncSuccess where                                                tall n = mFuncSuccess isObj ObjFuncSuccess (talls n)     (tall n)   

data StrFuncSuccess = StrFuncSuccess [(ExpToken,ExpObj)] ExpObj deriving (Show)
instance Arbitrary    StrFuncSuccess where arbitrary = sized1 tall; shrink (StrFuncSuccess ts e) = mFuncSuccess isStr StrFuncSuccess (tShrinks ts) (tShrink e)
instance Tall         StrFuncSuccess where                                                tall n = mFuncSuccess isStr StrFuncSuccess (talls n)     (tall n)   

data NumFuncSuccess = NumFuncSuccess [(ExpToken,ExpObj)] ExpObj deriving (Show)
instance Arbitrary    NumFuncSuccess where arbitrary = sized1 tall; shrink (NumFuncSuccess ts e) = mFuncSuccess isNum NumFuncSuccess (tShrinks ts) (tShrink e)
instance Tall         NumFuncSuccess where                                                tall n = mFuncSuccess isNum NumFuncSuccess (talls n)     (tall n)   

data BoolFuncSuccess = BoolFuncSuccess [(ExpToken,ExpObj)] ExpObj deriving (Show)
instance Arbitrary     BoolFuncSuccess where arbitrary = sized1 tall; shrink (BoolFuncSuccess ts e) = mFuncSuccess isBool BoolFuncSuccess (tShrinks ts) (tShrink e)
instance Tall          BoolFuncSuccess where                                                 tall n = mFuncSuccess isBool BoolFuncSuccess (talls n)     (tall n)   

data NullFuncSuccess = NullFuncSuccess [(ExpToken,ExpObj)] ExpObj deriving (Show)
instance Arbitrary     NullFuncSuccess where arbitrary = sized1 tall; shrink (NullFuncSuccess ts e) = mFuncSuccess isNull NullFuncSuccess (tShrinks ts) (tShrink e)
instance Tall          NullFuncSuccess where                                                 tall n = mFuncSuccess isNull NullFuncSuccess (talls n)     (tall n)   

data ArrOfFuncSuccess = ArrOfFuncSuccess [(ExpToken,ExpObj)] ExpObj Type deriving (Show)
instance Arbitrary      ArrOfFuncSuccess where arbitrary = sized1 tall; shrink (ArrOfFuncSuccess es e _) = mOfFuncSuccess (tShrinks es :: []  [(FuncTA,ArrOA)]) (tShrink e) id       ArrOfFuncSuccess
instance Tall           ArrOfFuncSuccess where                                                    tall n = mOfFuncSuccess (talls n     :: Gen [(FuncTA,ArrOA)]) (tall n)    elements ArrOfFuncSuccess

data ObjOfFuncSuccess = ObjOfFuncSuccess [(ExpToken,ExpObj)] ExpObj Type deriving (Show)
instance Arbitrary      ObjOfFuncSuccess where arbitrary = sized1 tall; shrink (ObjOfFuncSuccess es e _) = mOfFuncSuccess (tShrinks es :: []  [(FuncTA,ObjOA)]) (tShrink e) id       ObjOfFuncSuccess
instance Tall           ObjOfFuncSuccess where                                                    tall n = mOfFuncSuccess (talls n     :: Gen [(FuncTA,ObjOA)]) (tall n)    elements ObjOfFuncSuccess

data OrFuncSuccess = OrFuncSuccess [(ExpToken,ExpObj)] ExpObj  Type deriving (Show)
instance Arbitrary   OrFuncSuccess where arbitrary = sized1 tall; shrink (OrFuncSuccess es e _) = mOrFuncSuccess (tShrinks es) (tShrink e) id
instance Tall        OrFuncSuccess where                                                 tall n = mOrFuncSuccess (talls n)     (tall n)    elements

instance Unto b ExpObj    => Unto (FuncTA,b) (ExpToken,ExpObj) where un (x,y) = (un x,un y); to (x,y) = (to x,to y)
instance (Tall a, Tall b) => Tall (a,b)                        where tall n = liftM2 (,) (tall n) (tall n)

data ExpTS =  ExpTS ExpToken deriving (Show)
instance Unto ExpTS ExpToken where to = ExpTS; un (ExpTS e) = e
instance Arbitrary ExpTS where arbitrary = sized1 tall; shrink (ExpTS e) = mExpTS (tShrink e)
instance Tall      ExpTS where                                    tall n = mExpTS (tall n)
mExpTS = liftM (ExpTS .removeVarsAndFuncs.un) 

data ArrTS =  ArrTS ExpToken deriving (Show)
instance Unto ArrTS ExpToken where to = ArrTS; un (ArrTS e) = e
instance Arbitrary ArrTS where arbitrary = sized1 tall; shrink (ArrTS e) = mArrTS (tShrink e)
instance Tall      ArrTS where                                    tall n = mArrTS (tall n)
mArrTS = liftM (ArrTS .removeVarsAndFuncs.un) 

data ObjTS =  ObjTS ExpToken deriving (Show)
instance Unto ObjTS ExpToken where to = ObjTS; un (ObjTS e) = e
instance Arbitrary ObjTS where arbitrary = sized1 tall; shrink (ObjTS e) = mObjTS (tShrink e)
instance Tall      ObjTS where                                    tall n = mObjTS (tall n)
mObjTS = liftM (ObjTS .removeVarsAndFuncs.un) 

mLitFailure esa f isType mk = let onEmpty = mk [] (NullO p0) 0 in do
  es <- liftM (map un) esa
  let toChoose = filter (not.isType) es
  nullGuard es onEmpty $ nullGuard toChoose onEmpty $ do
    e <- f toChoose
    let Just i = elemIndex e es
    return $ mk es (unsafeMarshall [] e) i

mOfLitFailure asa chooseType chooseArg mk = let onEmpty = mk [] (NullO p0) 0 Null in do
  args <- liftM (map un) asa
  (t,isType) <- chooseType [(arr,isArr),(obj,isObj),(Str,isStr),(Num,isNum),(Bool,isBool),(Null,isNull)]
  let toChoose = filter (P.any (not.isType).elemsOf) args
  nullGuard args onEmpty $ nullGuard toChoose onEmpty $ do
    a <- chooseArg toChoose
    let Just i = elemIndex a args
        e      = head $ filter (not.isType) $ elemsOf a
    return $ mk args (unsafeMarshall [] e) i t

mFuncFailure tsa f isType mk = let onEmpty = mk [] (NullO p0) 0 in do
  ts <- liftM (map $ first clearParams . un) tsa 
  let toChoose = filter (not.isType.snd) ts
  nullGuard ts onEmpty $ nullGuard toChoose onEmpty $ do
    t <- f toChoose
    let Just i = elemIndex t ts
    return $ mk ts (snd t) i

mOfFuncFailure tsa chooseType chooseArg mk = let onEmpty = mk [] (NullO p0) 0 Null in do
  ts <- liftM (map $ first clearParams . un) tsa
  (ty,isType) <- chooseType [(arr,isArr),(obj,isObj),(Str,isStr),(Num,isNum),(Bool,isBool),(Null,isNull)]
  let toChoose = filter (P.any (not.isType).elemsOf.snd) ts
  nullGuard ts onEmpty $ nullGuard toChoose onEmpty $ do
    t <- chooseArg toChoose
    let Just i = elemIndex t ts
        e      = head $ filter (not.isType) $ elemsOf $ snd t
    return $ mk ts e i ty
    
mOrLitFailure esa chooseType chooseArg = let onEmpty = OrLitFailure [] (NullO p0) 0 Null in do
  es <- liftM (map un) esa
  (t1,t2,toChoose) <- getFailureOfOrTypes es id chooseType
  nullGuard es onEmpty $ nullGuard toChoose onEmpty $ do
    e <- chooseArg toChoose
    let Just i = elemIndex e es
    return $ OrLitFailure es (unsafeMarshall [] e) i $ Or [t1,t2]
    
mOrFuncFailure tsa chooseType chooseArg = let onEmpty = OrFuncFailure [] (NullO p0) 0 Null in do
  ts <- liftM (map $ first clearParams . un) tsa 
  (t1,t2,toChoose) <- getFailureOfOrTypes ts snd chooseType
  nullGuard ts onEmpty $ nullGuard toChoose onEmpty $ do
    t <- chooseArg toChoose
    let Just i = elemIndex t ts
    return $ OrFuncFailure ts (snd t) i $ Or [t1,t2]    
    
chooseT f = f [(arr,isArr),(obj,isObj),(Str,isStr),(Num,isNum),(Bool,isBool),(Null,isNull)]
getFailureOfOrTypes es f chooseType = do
  (t1,isType1) <- chooseT chooseType
  (t2,isType2) <- chooseT chooseType
  let isOneOf x = isType1 x || isType2 x
      toChoose = filter (not.isOneOf.f) es
  return (t1,t2,toChoose)
    
caseLitFailure  tree t s es e i =                     not (null es) ==> Left (TypeMismatch (getPos e) tree  $ getRoot e) == evalStateT (marshall $ mkFunc s es) (litFailure s es i t)
caseFuncFailure tree t s ts e i = validFuncs  s ts && not (null ts) ==> Left (TypeMismatch (getPos e) tree  $ getRoot e) == evalStateT (marshall $ mkFunc s es) (litFailure s es i t ++ entries) where
  (es,entries) = mkUtils ts

caseOfLitFailure  t s es e i mkT =                    not (null es) ==> Left (TypeMismatch (getPos e) (getRoot t) (getRoot e)) == evalStateT (marshall $ mkFunc s es) (litFailure s es i $ mkT t)
caseOfFuncFailure t s ts e i mkT = validFuncs s ts && not (null ts) ==> Left (TypeMismatch (getPos e) (getRoot t) (getRoot e)) == evalStateT (marshall $ mkFunc s es) (litFailure s es i (mkT t) ++ entries) where
  (es,entries) = mkUtils ts
  
caseOrLitFailure  t s es e i =                    not (null es) ==> Left (TypeMismatch (getPos e) (getRoot t) $ getRoot e) == evalStateT (marshall $ mkFunc s es) (litFailure s es i t)
caseOrFuncFailure t s ts e i = validFuncs s ts && not (null ts) ==> Left (TypeMismatch (getPos e) (getRoot t) $ getRoot e) == evalStateT (marshall $ mkFunc s es) (litFailure s es i t ++ entries) where
  (es,entries) = mkUtils ts

mkUtils ts = ( map fst ts
             , map (\(funcT,returnValue) -> (funcName funcT,[],Func $ \_ _ -> return returnValue)) ts)

validFuncs s ts = let ns = map (funcName.fst) ts in s `notElem` ns && ns == nub ns

funcName (FuncT _ (IdT _ _ s) _) = s
funcName x                       = error $ "FunctionEvalUtils::funcName [Unexpected pattern ["++show x++"]]" 

clearParams (FuncT a b _) = FuncT a b []
clearParams x             = error $ "FunctionEvalUtils::clearParams [Unexpected pattern ["++show x++"]]"

removeVarsAndFuncs (FuncT _ (IdT p w _) _) = NullT p w
removeVarsAndFuncs (VarT    (IdT p w _))   = NullT p w
removeVarsAndFuncs (ArrT p w es)           = ArrT p w $ map removeVarsAndFuncs es
removeVarsAndFuncs (ObjT p w ps)           = ObjT p w $ map (mapPair removeVarsAndFuncs) ps
removeVarsAndFuncs x                       = x
    
litFailure s es i t = [(s,zipWith f [0..] $ replicate (length es) any, error "FunctionEvalUtils::litFailure::func [Should not be called]")] where f j e | i == j = t | otherwise = e

{-| Success -}
mLitSuccess  isType mk = liftMF2 mk (filter isType.map un) un

mOfLitSuccess asa ea chooseType mk = do
  (t,isType) <- chooseType [(arr,isArr),(obj,isObj),(Str,isStr),(Num,isNum),(Bool,isBool),(Null,isNull)]
  args <- liftM (map $ filterElems isType.un) asa
  e <- liftM un ea
  return $ mk args e t

mOrLitSuccess esa ea chooseType = do
  es' <- liftM (map un) esa
  (t1,t2,es) <- getSuccessOfOrTypes es' id chooseType
  e <- liftM un ea
  return $ OrLitSuccess es e $ Or [t1,t2]
    

mFuncSuccess isType mk = liftMF2 mk (filter (isType.snd).map (first clearParams.un)) un

mOfFuncSuccess tsa ea chooseType mk = do
  (t,isType) <- chooseType [(arr,isArr),(obj,isObj),(Str,isStr),(Num,isNum),(Bool,isBool),(Null,isNull)]
  ts <- liftM (map $ (clearParams *** filterElems isType).un) tsa
  e <- liftM un ea
  return $ mk ts e t

mOrFuncSuccess tsa ea chooseType = do
  ts' <- liftM (map $ first clearParams . un) tsa
  (t1,t2,ts) <- getSuccessOfOrTypes ts' snd chooseType
  e <- liftM un ea
  return $ OrFuncSuccess ts e $ Or [t1,t2]
    
caseLitSuccess  t s es e =                      Right e == evalStateT (marshall $ mkFunc s es) (litSuccess s es e t)
caseFuncSuccess t s ts e = validFuncs  s ts ==> Right e == evalStateT (marshall $ mkFunc s es) (litSuccess s es e t ++ entries) where
  (es,entries) = mkUtils ts

caseOfLitSuccess  t s es e mkT =                     Right e == evalStateT (marshall $ mkFunc s es) (litSuccess s es e $ mkT t)
caseOfFuncSuccess t s ts e mkT = validFuncs s ts ==> Right e == evalStateT (marshall $ mkFunc s es) (litSuccess s es e (mkT t) ++ entries) where
  (es,entries) = mkUtils ts
  
caseOrLitSuccess  t s es e =                    Right e == evalStateT (marshall $ mkFunc s es) (litSuccess s es e t)
caseOrFuncSuccess t s ts e = validFuncs s ts && Right e == evalStateT (marshall $ mkFunc s es) (litSuccess s es e t ++ entries) where
  (es,entries) = mkUtils ts

litSuccess s es e t = [(s,replicate (length es) t, Func $ \_ _ -> return e)]

getSuccessOfOrTypes es f chooseType = do
  (t1,isType1) <- chooseT chooseType
  (t2,isType2) <- chooseT chooseType
  let isOneOf x = isType1 x || isType2 x
      toChoose = filter (isOneOf.f) es
  return (t1,t2,toChoose)

{-| Mandatory type signatures -}
mNbArgs        :: (Applicative m, Monad m) => m P -> m String -> m ValidInt -> m ValidInt -> m NbArgs
mLitFailure    :: (Applicative m, Monad m) => m [ExpTS]          -> ([ExpToken] -> m ExpToken)                   -> (ExpToken -> Bool) -> ([ExpToken]          -> ExpObj -> Int -> a) -> m a
mFuncFailure   :: (Applicative m, Monad m) => m [(FuncTA,ExpOA)] -> ([(ExpToken,ExpObj)] -> m (ExpToken,ExpObj)) -> (ExpObj -> Bool)   -> ([(ExpToken,ExpObj)] -> ExpObj -> Int -> a) -> m a
mOfLitFailure  :: (Applicative m, Monad m, Unto b ExpToken) => m [b]          -> ([(Type,ExpToken  -> Bool)] -> m (Type,ExpToken -> Bool)) -> ([ ExpToken]         -> m ExpToken)          -> ([ExpToken]          -> ExpObj -> Int -> Type -> a) -> m a
mOfFuncFailure :: (Applicative m, Monad m, Unto b ExpObj)   => m [(FuncTA,b)] -> ([(Type,ExpObj    -> Bool)] -> m (Type,ExpObj   -> Bool)) -> ([(ExpToken,ExpObj)] -> m (ExpToken,ExpObj)) -> ([(ExpToken,ExpObj)] -> ExpObj -> Int -> Type -> a) -> m a
mOrLitFailure  :: (Applicative m, Monad m) => m [ExpTS]          -> ([(Type,ExpToken -> Bool)] -> m (Type,ExpToken -> Bool)) -> ([ExpToken] -> m ExpToken)                   -> m OrLitFailure
mOrFuncFailure :: (Applicative m, Monad m) => m [(FuncTA,ExpOA)] -> ([(Type,ExpObj   -> Bool)] -> m (Type,ExpObj   -> Bool)) -> ([(ExpToken,ExpObj)] -> m (ExpToken,ExpObj)) -> m OrFuncFailure

mLitSuccess    :: (Applicative m, Monad m) => (ExpToken -> Bool) -> ([ ExpToken]         -> ExpObj -> a) -> m [ExpTS]          -> m ExpOA -> m a 
mFuncSuccess   :: (Applicative m, Monad m) => (ExpObj   -> Bool) -> ([(ExpToken,ExpObj)] -> ExpObj -> a) -> m [(FuncTA,ExpOA)] -> m ExpOA -> m a
mOfLitSuccess  :: (Applicative m, Monad m, Unto b ExpToken) => m [b]          -> m ExpOA -> ([(Type,ExpToken -> Bool)] -> m (Type,ExpToken -> Bool)) -> ([ExpToken]          -> ExpObj -> Type -> a) -> m a
mOfFuncSuccess :: (Applicative m, Monad m, Unto b ExpObj)   => m [(FuncTA,b)] -> m ExpOA -> ([(Type,ExpObj   -> Bool)] -> m (Type,ExpObj   -> Bool)) -> ([(ExpToken,ExpObj)] -> ExpObj -> Type -> a) -> m a
mOrLitSuccess  :: (Applicative m, Monad m) => m [ExpTS]          -> m ExpOA -> ([(Type,ExpToken -> Bool)] -> m (Type,ExpToken -> Bool)) -> m OrLitSuccess
mOrFuncSuccess :: (Applicative m, Monad m) => m [(FuncTA,ExpOA)] -> m ExpOA -> ([(Type,ExpObj   -> Bool)] -> m (Type,ExpObj   -> Bool)) -> m OrFuncSuccess
mExpTS         :: (Applicative m, Monad m) => m ExpTA -> m ExpTS
mArrTS         :: (Applicative m, Monad m) => m ArrTA -> m ArrTS
mObjTS         :: (Applicative m, Monad m) => m ObjTA -> m ObjTS

