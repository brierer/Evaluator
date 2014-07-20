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

class    ElemsOf a        where elemsOf :: a -> [a]
instance ElemsOf ExpToken where elemsOf (ArrT _ _ es) = es
                                elemsOf (ObjT _ _ ps) = map pairVal ps
instance ElemsOf ExpObj   where elemsOf (ArrO _ es) = es
                                elemsOf (ObjO _ ps) = map snd ps

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
    
    
caseLitFailure  tree t s es e i =                   not (null es) ==> Left (TypeMismatch (getPos e) tree  $ getRoot e) == evalStateT (marshall $ mkFunc s es) (litFailure s es i t)
caseFuncFailure tree t s ts e i = s `notElem` ns && not (null ts) ==> Left (TypeMismatch (getPos e) tree  $ getRoot e) == evalStateT (marshall $ mkFunc s es) (litFailure s es i t ++ entries) where
  (es,ns,entries) = mkUtils ts

caseOfLitFailure  mkT t s es e i =                   not (null es) ==> Left (TypeMismatch (getPos e) (getRoot t) (getRoot e)) == evalStateT (marshall $ mkFunc s es) (litFailure s es i $ mkT t)
caseOfFuncFailure mkT t s ts e i = s `notElem` ns && not (null ts) ==> Left (TypeMismatch (getPos e) (getRoot t) (getRoot e)) == evalStateT (marshall $ mkFunc s es) (litFailure s es i (mkT t) ++ entries) where
  (es,ns,entries) = mkUtils ts
  
mkUtils ts = ( map fst ts
             , map (funcName.fst) ts
             , map (\(funcT,returnValue) -> (funcName funcT,[],Func $ \_ _ -> return returnValue)) ts)

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

{-| Mandatory type signatures -}
mNbArgs        :: (Applicative m, Monad m) => m P -> m String -> m ValidInt -> m ValidInt -> m NbArgs
mLitFailure    :: (Applicative m, Monad m) => m [ExpTS]          -> ([ExpToken] -> m ExpToken)                   -> (ExpToken -> Bool) -> ([ExpToken]          -> ExpObj -> Int -> a) -> m a
mFuncFailure   :: (Applicative m, Monad m) => m [(FuncTA,ExpOA)] -> ([(ExpToken,ExpObj)] -> m (ExpToken,ExpObj)) -> (ExpObj -> Bool)   -> ([(ExpToken,ExpObj)] -> ExpObj -> Int -> a) -> m a
mOfLitFailure  :: (Unto b ExpToken, Applicative m, Monad m) => m [b]          -> ([(Type,ExpToken  -> Bool)] -> m (Type,ExpToken -> Bool)) -> ([ ExpToken]         -> m ExpToken)          -> ([ExpToken]          -> ExpObj -> Int -> Type -> a) -> m a
mOfFuncFailure :: (Unto b ExpObj,   Applicative m, Monad m) => m [(FuncTA,b)] -> ([(Type,ExpObj     -> Bool)] -> m (Type,ExpObj  -> Bool)) -> ([(ExpToken,ExpObj)] -> m (ExpToken,ExpObj)) -> ([(ExpToken,ExpObj)] -> ExpObj -> Int -> Type -> a) -> m a
mExpTS         :: (Applicative m, Monad m) => m ExpTA -> m ExpTS
mArrTS         :: (Applicative m, Monad m) => m ArrTA -> m ArrTS
mObjTS         :: (Applicative m, Monad m) => m ObjTA -> m ObjTS

