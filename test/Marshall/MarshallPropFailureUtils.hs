{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module Marshall.MarshallPropFailureUtils where

import Control.Arrow hiding (arr)
import Data.List     hiding (any)
import Prelude       hiding (any)

import qualified Prelude as P

import Control.Applicative
import Control.Monad.State
import Data.EvalError
import Data.ExpToken
import Data.ExpObj
import Data.HasPos
import Data.Type
import Eval.Marshall
import Eval.MatchType
import Test.Framework

import Marshall.MarshallPropUtils
import Marshall.MarshallUtils
import MatchType.MatchTypePropUtils
import MatchType.MatchTypeUnitUtils
import MultiPass.MultiPassPropUtils
import Parser.ParserPropUtils
import Parser.ParserUnitUtils

data NbArgs = NbArgs Pos String Int Int deriving (Show)
instance Arbitrary   NbArgs where
  arbitrary                      = mNbArgs  arbitrary  arbitrary   arbitrary   arbitrary
  shrink (NbArgs p s n m) = mNbArgs (tShrink p) (shrink s) (tShrink n) (tShrink m)
mNbArgs = liftMF4 NbArgs un id un un

data ArrLit = ArrLit [ExpToken] ExpObj Int deriving (Show)
instance Arbitrary   ArrLit where arbitrary = sized1 tall; shrink (ArrLit es _ _) = mLit (tShrinks es) id       isArr ArrLit
instance Tall        ArrLit where                                          tall n = mLit (talls n)     elements isArr ArrLit

data ObjLit = ObjLit [ExpToken] ExpObj Int deriving (Show)
instance Arbitrary   ObjLit where arbitrary = sized1 tall; shrink (ObjLit es _ _) = mLit (tShrinks es) id       isObj ObjLit
instance Tall        ObjLit where                                          tall n = mLit (talls n)     elements isObj ObjLit

data StrLit = StrLit [ExpToken] ExpObj Int deriving (Show)
instance Arbitrary   StrLit where arbitrary = sized1 tall; shrink (StrLit es _ _) = mLit (tShrinks es) id       isStr StrLit
instance Tall        StrLit where                                          tall n = mLit (talls n)     elements isStr StrLit

data NumLit = NumLit [ExpToken] ExpObj Int deriving (Show)
instance Arbitrary   NumLit where arbitrary = sized1 tall; shrink (NumLit es _ _) = mLit (tShrinks es) id       isNum NumLit
instance Tall        NumLit where                                          tall n = mLit (talls n)     elements isNum NumLit

data BoolLit = BoolLit [ExpToken] ExpObj Int deriving (Show)
instance Arbitrary    BoolLit where arbitrary = sized1 tall; shrink (BoolLit es _ _) = mLit (tShrinks es) id       isBool BoolLit
instance Tall         BoolLit where                                           tall n = mLit (talls n)     elements isBool BoolLit

data NullLit = NullLit [ExpToken] ExpObj Int deriving (Show)
instance Arbitrary    NullLit where arbitrary = sized1 tall; shrink (NullLit es _ _) = mLit (tShrinks es) id       isNull NullLit
instance Tall         NullLit where                                           tall n = mLit (talls n)     elements isNull NullLit

data ArrOfLit = ArrOfLit [ExpToken] ExpObj Int FiniteType deriving (Show)
instance Arbitrary     ArrOfLit where arbitrary = sized1 tall; shrink (ArrOfLit es _ _ _) = mOfLit (tShrinks es :: []  [ArrTS]) id       id       ArrOfLit
instance Tall          ArrOfLit where                                              tall n = mOfLit (talls n     :: Gen [ArrTS]) elements elements ArrOfLit

data ObjOfLit = ObjOfLit [ExpToken] ExpObj Int FiniteType deriving (Show)
instance Arbitrary     ObjOfLit where arbitrary = sized1 tall; shrink (ObjOfLit es _ _ _) = mOfLit (tShrinks es :: []  [ObjTS]) id       id       ObjOfLit
instance Tall          ObjOfLit where                                              tall n = mOfLit (talls n     :: Gen [ObjTS]) elements elements ObjOfLit

data OrLit = OrLit [ExpToken] ExpObj Int FiniteType deriving (Show)
instance Arbitrary  OrLit where arbitrary = sized1 tall; shrink (OrLit es _ _ _) = mOrLit (tShrinks es) id       id
instance Tall       OrLit where                                           tall n = mOrLit (talls n)     elements elements

data TableFunc = TableFunc [(ExpToken,ExpObj)] ExpObj Int deriving (Show)
instance Arbitrary      TableFunc where arbitrary = sized1 tall; shrink (TableFunc ts _ _) = mFunc (tShrinks ts) id       isTable TableFunc
instance Tall           TableFunc where                                             tall n = mFunc (talls n)     elements isTable TableFunc

data PlotFunc = PlotFunc [(ExpToken,ExpObj)] ExpObj Int deriving (Show)
instance Arbitrary     PlotFunc where arbitrary = sized1 tall; shrink (PlotFunc ts _ _) = mFunc (tShrinks ts) id       isPlot PlotFunc
instance Tall          PlotFunc where                                            tall n = mFunc (talls n)     elements isPlot PlotFunc

data ArrFunc = ArrFunc [(ExpToken,ExpObj)] ExpObj Int deriving (Show)
instance Arbitrary    ArrFunc where arbitrary = sized1 tall; shrink (ArrFunc ts _ _) = mFunc (tShrinks ts) id       isArr ArrFunc
instance Tall         ArrFunc where                                           tall n = mFunc (talls n)     elements isArr ArrFunc

data ObjFunc = ObjFunc [(ExpToken,ExpObj)] ExpObj Int deriving (Show)
instance Arbitrary    ObjFunc where arbitrary = sized1 tall; shrink (ObjFunc ts _ _) = mFunc (tShrinks ts) id       isObj ObjFunc
instance Tall         ObjFunc where                                          tall n = mFunc (talls n)     elements isObj ObjFunc

data StrFunc = StrFunc [(ExpToken,ExpObj)] ExpObj Int deriving (Show)
instance Arbitrary    StrFunc where arbitrary = sized1 tall; shrink (StrFunc ts _ _) = mFunc (tShrinks ts) id       isStr StrFunc
instance Tall         StrFunc where                                           tall n = mFunc (talls n)     elements isStr StrFunc

data NumFunc = NumFunc [(ExpToken,ExpObj)] ExpObj Int deriving (Show)
instance Arbitrary    NumFunc where arbitrary = sized1 tall; shrink (NumFunc ts _ _) = mFunc (tShrinks ts) id       isNum NumFunc
instance Tall         NumFunc where                                           tall n = mFunc (talls n)     elements isNum NumFunc

data BoolFunc = BoolFunc [(ExpToken,ExpObj)] ExpObj Int deriving (Show)
instance Arbitrary     BoolFunc where arbitrary = sized1 tall; shrink (BoolFunc ts _ _) = mFunc (tShrinks ts) id       isBool BoolFunc
instance Tall          BoolFunc where                                            tall n = mFunc (talls n)     elements isBool BoolFunc

data NullFunc = NullFunc [(ExpToken,ExpObj)] ExpObj Int deriving (Show)
instance Arbitrary     NullFunc where arbitrary = sized1 tall; shrink (NullFunc ts _ _) = mFunc (tShrinks ts) id       isNull NullFunc
instance Tall          NullFunc where                                            tall n = mFunc (talls n)     elements isNull NullFunc

data ArrOfFunc = ArrOfFunc [(ExpToken,ExpObj)] ExpObj Int FiniteType deriving (Show)
instance Arbitrary      ArrOfFunc where arbitrary = sized1 tall; shrink (ArrOfFunc ts _ _ _) = mOfFunc (tShrinks ts :: []  [(FuncTA,ArrOA)]) id       id       ArrOfFunc
instance Tall           ArrOfFunc where                                               tall n = mOfFunc (talls (n-1) :: Gen [(FuncTA,ArrOA)]) elements elements ArrOfFunc

data ObjOfFunc = ObjOfFunc [(ExpToken,ExpObj)] ExpObj Int FiniteType deriving (Show)
instance Arbitrary      ObjOfFunc where arbitrary = sized1 tall; shrink (ObjOfFunc ts _ _ _) = mOfFunc (tShrinks ts :: []  [(FuncTA,ObjOA)]) id       id       ObjOfFunc
instance Tall           ObjOfFunc where                                               tall n = mOfFunc (talls (n-1) :: Gen [(FuncTA,ObjOA)]) elements elements ObjOfFunc

data OrFunc = OrFunc [(ExpToken,ExpObj)] ExpObj Int FiniteType deriving (Show)
instance Arbitrary   OrFunc where arbitrary = sized1 tall; shrink (OrFunc ts _ _ _) = mOrFunc (tShrinks ts)   id       id
instance Tall        OrFunc where                                            tall n = mOrFunc (talls n)       elements elements

data FiniteType = FiniteType Type
instance Show FiniteType where 
  show (FiniteType t) = "FiniteType " ++ f 10 t where
    f :: Int -> Type -> String
    f 0 _ = "..."
    f n (ArrOf u) = "(ArrOf " ++ f (n-1) u ++ ")"
    f n (ObjOf u) = "(ObjOf " ++ f (n-1) u ++ ")"
    f n (Or ts)   = "(Or ["++ intercalate "," (map (f $ n - 1) ts) ++ "])"
    f _ e         = show e

mLit esa f isType mk = let onEmpty = mk [] nullO 0 in do
  es <- liftM (map un) esa
  let toChoose = filter (not.isType) es
  nullGuard es onEmpty $ nullGuard toChoose onEmpty $ do
    e <- f toChoose
    let Just i = elemIndex e es
    return $ mk es (unsafeMarshall [] e) i

mOfLit asa chooseType chooseArg mk = let onEmpty = mk [] nullO 0 (FiniteType Null) in do
  args <- liftM (map un) asa
  (t,isType) <- chooseType [(arr,isArr),(obj,isObj),(Str,isStr),(Num,isNum),(Bool,isBool),(Null,isNull)]
  let toChoose = filter (P.any (not.isType).elemsOf) args
  nullGuard args onEmpty $ nullGuard toChoose onEmpty $ do
    a <- chooseArg toChoose
    let Just i = elemIndex a args
        e      = head $ filter (not.isType) $ elemsOf a
    return $ mk args (unsafeMarshall [] e) i $ FiniteType t

mFunc tsa f isType mk = let onEmpty = mk [] nullO 0 in do
  ts <- liftM (map $ first clearParams . un) tsa
  let toChoose = filter (not.isType.snd) ts
  nullGuard ts onEmpty $ nullGuard toChoose onEmpty $ do
    t <- f toChoose
    let Just i = elemIndex t ts
    return $ mk ts (snd t) i

mOfFunc tsa chooseType chooseArg mk = let onEmpty = mk [] nullO 0 (FiniteType Null) in do
  ts <- liftM (map $ first clearParams . un) tsa
  (ty,isType) <- chooseType [(arr,isArr),(obj,isObj),(Str,isStr),(Num,isNum),(Bool,isBool),(Null,isNull)]
  let toChoose = filter (P.any (not.isType).elemsOf.snd) ts
  nullGuard ts onEmpty $ nullGuard toChoose onEmpty $ do
    t <- chooseArg toChoose
    let Just i = elemIndex t ts
        e      = head $ filter (not.isType) $ elemsOf $ snd t
    return $ mk ts e i $ FiniteType ty

mOrLit esa chooseType chooseArg = let onEmpty = OrLit [] nullO 0 (FiniteType Null) in do
  es <- liftM (map un) esa
  (t1,t2,toChoose) <- getOfOrTypes es id chooseType
  nullGuard es onEmpty $ nullGuard toChoose onEmpty $ do
    e <- chooseArg toChoose
    let Just i = elemIndex e es
    return $ OrLit es (unsafeMarshall [] e) i $ FiniteType $ Or [t1,t2]

mOrFunc tsa chooseType chooseArg = let onEmpty = OrFunc [] nullO 0 (FiniteType Null) in do
  ts <- liftM (map $ first clearParams . un) tsa
  (t1,t2,toChoose) <- getOfOrTypes ts snd chooseType
  nullGuard ts onEmpty $ nullGuard toChoose onEmpty $ do
    t <- chooseArg toChoose
    let Just i = elemIndex t ts
    return $ OrFunc ts (snd t) i $ FiniteType $ Or [t1,t2]

getOfOrTypes es f chooseType = do
  (t1,isType1) <- chooseT chooseType
  (t2,isType2) <- chooseT chooseType
  let isOneOf x = isType1 x || isType2 x
      toChoose = filter (not.isOneOf.f) es
  return (t1,t2,toChoose)

caseLit  tree t s es e i =                     not (null es) ==> Left (TypeMismatch $ TMLeaf (getPos e) tree  $ getRoot e) == marshallWith (mkFunc s es) (lit s es i t)
caseFunc tree t s ts e i = validFuncs  s ts && not (null ts) ==> Left (TypeMismatch $ TMLeaf (getPos e) tree  $ getRoot e) == marshallWith (mkFunc s es) (lit s es i t ++ entries) where
  (es,entries) = mkUtils ts

caseOfLit  (FiniteType t) s es e i mkT =                    not (null es) ==> Left (TypeMismatch $ TMLeaf (getPos e) (getRoot t) (getRoot e)) == marshallWith (mkFunc s es) (lit s es i $ mkT t)
caseOfFunc (FiniteType t) s ts e i mkT = validFuncs s ts && not (null ts) ==> Left (TypeMismatch $ TMLeaf (getPos e) (getRoot t) (getRoot e)) == marshallWith (mkFunc s es) (lit s es i (mkT t) ++ entries) where
  (es,entries) = mkUtils ts

caseOrLit  (FiniteType t) s es e i =                    not (null es) ==> Left (TypeMismatch $ TMLeaf (getPos e) (getRoot t) $ getRoot e) == marshallWith (mkFunc s es) (lit s es i t)
caseOrFunc (FiniteType t) s ts e i = validFuncs s ts && not (null ts) ==> Left (TypeMismatch $ TMLeaf (getPos e) (getRoot t) $ getRoot e) == marshallWith (mkFunc s es) (lit s es i t ++ entries) where
  (es,entries) = mkUtils ts

lit s es i t = let ts = zipWith f [0..] $ replicate (length es) any in [(s,ts, error $ "MarshallPropUtils::lit::func [Should not be called ("++show ts++") ["++s++"] ("++show es++") ["++show i++"] ["++show (getRoot t)++"]]")] where f j e | i == j = t | otherwise = e

{-| Mandatory type signatures -}
mNbArgs :: (Applicative m, Monad m) => m P -> m String -> m ValidInt -> m ValidInt -> m NbArgs
mLit    :: (Applicative m, Monad m) => m [ExpTS]          -> ([ExpToken] -> m ExpToken)                   -> (ExpToken -> Bool) -> ([ExpToken]          -> ExpObj -> Int -> a) -> m a
mFunc   :: (Applicative m, Monad m) => m [(FuncTA,ExpOA)] -> ([(ExpToken,ExpObj)] -> m (ExpToken,ExpObj)) -> (ExpObj -> Bool)   -> ([(ExpToken,ExpObj)] -> ExpObj -> Int -> a) -> m a
mOfLit  :: (Applicative m, Monad m, Unto b ExpToken) => m [b]          -> ([(Type,ExpToken  -> Bool)] -> m (Type,ExpToken -> Bool)) -> ([ ExpToken]         -> m ExpToken)          -> ([ExpToken]          -> ExpObj -> Int -> FiniteType -> a) -> m a
mOfFunc :: (Applicative m, Monad m, Unto b ExpObj)   => m [(FuncTA,b)] -> ([(Type,ExpObj    -> Bool)] -> m (Type,ExpObj   -> Bool)) -> ([(ExpToken,ExpObj)] -> m (ExpToken,ExpObj)) -> ([(ExpToken,ExpObj)] -> ExpObj -> Int -> FiniteType -> a) -> m a
mOrLit  :: (Applicative m, Monad m) => m [ExpTS]          -> ([(Type,ExpToken -> Bool)] -> m (Type,ExpToken -> Bool)) -> ([ExpToken] -> m ExpToken)                   -> m OrLit
mOrFunc :: (Applicative m, Monad m) => m [(FuncTA,ExpOA)] -> ([(Type,ExpObj   -> Bool)] -> m (Type,ExpObj   -> Bool)) -> ([(ExpToken,ExpObj)] -> m (ExpToken,ExpObj)) -> m OrFunc
