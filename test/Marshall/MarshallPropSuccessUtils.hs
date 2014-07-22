{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module Marshall.MarshallPropSuccessUtils where

import Control.Arrow hiding (arr)
import Data.List     hiding (any)
import Prelude       hiding (any)

import qualified Prelude as P

import Control.Applicative
import Control.Monad.State
import Data.Eval
import Data.ExpToken
import Data.ExpObj
import Data.Type
import Eval.Marshall
import Eval.MatchType
import Test.Framework

import Marshall.MarshallPropUtils
import MatchType.MatchTypePropUtils
import Parser.ParserPropUtils
import Parser.ParserUnitUtils

data ArrLit = ArrLit [ExpToken] ExpObj deriving (Show)
instance Arbitrary   ArrLit where arbitrary = sized1 tall; shrink (ArrLit es e) = mLit isArr ArrLit (tShrinks es) (tShrink e)
instance Tall        ArrLit where                                               tall n = mLit isArr ArrLit (talls n)     (tall n)

data ObjLit = ObjLit [ExpToken] ExpObj deriving (Show)
instance Arbitrary   ObjLit where arbitrary = sized1 tall; shrink (ObjLit es e) = mLit isObj ObjLit (tShrinks es) (tShrink e)
instance Tall        ObjLit where                                               tall n = mLit isObj ObjLit (talls n)     (tall n)

data StrLit = StrLit [ExpToken] ExpObj deriving (Show)
instance Arbitrary   StrLit where arbitrary = sized1 tall; shrink (StrLit es e) = mLit isStr StrLit (tShrinks es) (tShrink e)
instance Tall        StrLit where                                               tall n = mLit isStr StrLit (talls n)     (tall n)

data NumLit = NumLit [ExpToken] ExpObj deriving (Show)
instance Arbitrary   NumLit where arbitrary = sized1 tall; shrink (NumLit es e) = mLit isNum NumLit (tShrinks es) (tShrink e)
instance Tall        NumLit where                                               tall n = mLit isNum NumLit (talls n)     (tall n)

data BoolLit = BoolLit [ExpToken] ExpObj deriving (Show)
instance Arbitrary    BoolLit where arbitrary = sized1 tall; shrink (BoolLit es e) = mLit isBool BoolLit (tShrinks es) (tShrink e)
instance Tall         BoolLit where                                                tall n = mLit isBool BoolLit (talls n)     (tall n)

data NullLit = NullLit [ExpToken] ExpObj deriving (Show)
instance Arbitrary    NullLit where arbitrary = sized1 tall; shrink (NullLit es e) = mLit isNull NullLit (tShrinks es) (tShrink e)
instance Tall         NullLit where                                                tall n = mLit isNull NullLit (talls n)     (tall n)

data ArrOfLit = ArrOfLit [ExpToken] ExpObj Type deriving (Show)
instance Arbitrary     ArrOfLit where arbitrary = sized1 tall; shrink (ArrOfLit es e _) = mOfLit (tShrinks es :: []  [ArrTS]) (tShrink e) id       ArrOfLit
instance Tall          ArrOfLit where                                                   tall n = mOfLit (talls n     :: Gen [ArrTS]) (tall n)    elements ArrOfLit

data ObjOfLit = ObjOfLit [ExpToken] ExpObj Type deriving (Show)
instance Arbitrary     ObjOfLit where arbitrary = sized1 tall; shrink (ObjOfLit es e _) = mOfLit (tShrinks es :: []  [ObjTS]) (tShrink e) id       ObjOfLit
instance Tall          ObjOfLit where                                                   tall n = mOfLit (talls n     :: Gen [ObjTS]) (tall n)    elements ObjOfLit

data OrLit = OrLit [ExpToken] ExpObj  Type deriving (Show)
instance Arbitrary  OrLit where arbitrary = sized1 tall; shrink (OrLit es e _) = mOrLit (tShrinks es) (tShrink e) id
instance Tall       OrLit where                                                tall n = mOrLit (talls n)     (tall n)    elements

data TableFunc = TableFunc [(ExpToken,ExpObj)] ExpObj deriving (Show)
instance Arbitrary      TableFunc where arbitrary = sized1 tall; shrink (TableFunc ts e) = mFunc isTable TableFunc (tShrinks ts) (tShrink e)
instance Tall           TableFunc where                                                  tall n = mFunc isTable TableFunc (talls n)     (tall n)

data PlotFunc = PlotFunc [(ExpToken,ExpObj)] ExpObj deriving (Show)
instance Arbitrary     PlotFunc where arbitrary = sized1 tall; shrink (PlotFunc ts e) = mFunc isPlot PlotFunc (tShrinks ts) (tShrink e)
instance Tall          PlotFunc where                                                 tall n = mFunc isPlot PlotFunc (talls n)     (tall n)

data ArrFunc = ArrFunc [(ExpToken,ExpObj)] ExpObj deriving (Show)
instance Arbitrary    ArrFunc where arbitrary = sized1 tall; shrink (ArrFunc ts e) = mFunc isArr ArrFunc (tShrinks ts) (tShrink e)
instance Tall         ArrFunc where                                                tall n = mFunc isArr ArrFunc (talls n)     (tall n)

data ObjFunc = ObjFunc [(ExpToken,ExpObj)] ExpObj deriving (Show)
instance Arbitrary    ObjFunc where arbitrary = sized1 tall; shrink (ObjFunc ts e) = mFunc isObj ObjFunc (tShrinks ts) (tShrink e)
instance Tall         ObjFunc where                                                tall n = mFunc isObj ObjFunc (talls n)     (tall n)

data StrFunc = StrFunc [(ExpToken,ExpObj)] ExpObj deriving (Show)
instance Arbitrary    StrFunc where arbitrary = sized1 tall; shrink (StrFunc ts e) = mFunc isStr StrFunc (tShrinks ts) (tShrink e)
instance Tall         StrFunc where                                                tall n = mFunc isStr StrFunc (talls n)     (tall n)

data NumFunc = NumFunc [(ExpToken,ExpObj)] ExpObj deriving (Show)
instance Arbitrary    NumFunc where arbitrary = sized1 tall; shrink (NumFunc ts e) = mFunc isNum NumFunc (tShrinks ts) (tShrink e)
instance Tall         NumFunc where                                                tall n = mFunc isNum NumFunc (talls n)     (tall n)

data BoolFunc = BoolFunc [(ExpToken,ExpObj)] ExpObj deriving (Show)
instance Arbitrary     BoolFunc where arbitrary = sized1 tall; shrink (BoolFunc ts e) = mFunc isBool BoolFunc (tShrinks ts) (tShrink e)
instance Tall          BoolFunc where                                                 tall n = mFunc isBool BoolFunc (talls n)     (tall n)

data NullFunc = NullFunc [(ExpToken,ExpObj)] ExpObj deriving (Show)
instance Arbitrary     NullFunc where arbitrary = sized1 tall; shrink (NullFunc ts e) = mFunc isNull NullFunc (tShrinks ts) (tShrink e)
instance Tall          NullFunc where                                                 tall n = mFunc isNull NullFunc (talls n)     (tall n)

data ArrOfFunc = ArrOfFunc [(ExpToken,ExpObj)] ExpObj Type deriving (Show)
instance Arbitrary      ArrOfFunc where arbitrary = sized1 tall; shrink (ArrOfFunc es e _) = mOfFunc (tShrinks es :: []  [(FuncTA,ArrOA)]) (tShrink e) id       ArrOfFunc
instance Tall           ArrOfFunc where                                                    tall n = mOfFunc (talls n     :: Gen [(FuncTA,ArrOA)]) (tall n)    elements ArrOfFunc

data ObjOfFunc = ObjOfFunc [(ExpToken,ExpObj)] ExpObj Type deriving (Show)
instance Arbitrary      ObjOfFunc where arbitrary = sized1 tall; shrink (ObjOfFunc es e _) = mOfFunc (tShrinks es :: []  [(FuncTA,ObjOA)]) (tShrink e) id       ObjOfFunc
instance Tall           ObjOfFunc where                                                    tall n = mOfFunc (talls n     :: Gen [(FuncTA,ObjOA)]) (tall n)    elements ObjOfFunc

data OrFunc = OrFunc [(ExpToken,ExpObj)] ExpObj  Type deriving (Show)
instance Arbitrary   OrFunc where arbitrary = sized1 tall; shrink (OrFunc es e _) = mOrFunc (tShrinks es) (tShrink e) id
instance Tall        OrFunc where                                                 tall n = mOrFunc (talls n)     (tall n)    elements

mLit  isType mk = liftMF2 mk (filter isType.map un) un

mOfLit asa ea chooseType mk = do
  (t,isType) <- chooseType [(arr,isArr),(obj,isObj),(Str,isStr),(Num,isNum),(Bool,isBool),(Null,isNull)]
  args <- liftM (map $ filterElems isType.un) asa
  e <- liftM un ea
  return $ mk args e t

mOrLit esa ea chooseType = do
  es' <- liftM (map un) esa
  (t1,t2,es) <- getOfOrTypes es' id chooseType
  e <- liftM un ea
  return $ OrLit es e $ Or [t1,t2]


mFunc isType mk = liftMF2 mk (filter (isType.snd).map (first clearParams.un)) un

mOfFunc tsa ea chooseType mk = do
  (t,isType) <- chooseType [(arr,isArr),(obj,isObj),(Str,isStr),(Num,isNum),(Bool,isBool),(Null,isNull)]
  ts <- liftM (map $ (clearParams *** filterElems isType).un) tsa
  e <- liftM un ea
  return $ mk ts e t

mOrFunc tsa ea chooseType = do
  ts' <- liftM (map $ first clearParams . un) tsa
  (t1,t2,ts) <- getOfOrTypes ts' snd chooseType
  e <- liftM un ea
  return $ OrFunc ts e $ Or [t1,t2]

caseLit  t s es e =                      Right e == evalStateT (marshall $ mkFunc s es) (lit s es e t)
caseFunc t s ts e = validFuncs  s ts ==> Right e == evalStateT (marshall $ mkFunc s es) (lit s es e t ++ entries) where
  (es,entries) = mkUtils ts

caseOfLit  t s es e mkT =                     Right e == evalStateT (marshall $ mkFunc s es) (lit s es e $ mkT t)
caseOfFunc t s ts e mkT = validFuncs s ts ==> Right e == evalStateT (marshall $ mkFunc s es) (lit s es e (mkT t) ++ entries) where
  (es,entries) = mkUtils ts

caseOrLit  t s es e =                    Right e == evalStateT (marshall $ mkFunc s es) (lit s es e t)
caseOrFunc t s ts e = validFuncs s ts && Right e == evalStateT (marshall $ mkFunc s es) (lit s es e t ++ entries) where
  (es,entries) = mkUtils ts

lit s es e t = [(s,replicate (length es) t, Func $ \_ _ -> return e)]

getOfOrTypes es f chooseType = do
  (t1,isType1) <- chooseT chooseType
  (t2,isType2) <- chooseT chooseType
  let isOneOf x = isType1 x || isType2 x
      toChoose = filter (isOneOf.f) es
  return (t1,t2,toChoose)

{-| Mandatory type signatures -}
mLit    :: (Applicative m, Monad m) => (ExpToken -> Bool) -> ([ ExpToken]         -> ExpObj -> a) -> m [ExpTS]          -> m ExpOA -> m a
mFunc   :: (Applicative m, Monad m) => (ExpObj   -> Bool) -> ([(ExpToken,ExpObj)] -> ExpObj -> a) -> m [(FuncTA,ExpOA)] -> m ExpOA -> m a
mOfLit  :: (Applicative m, Monad m, Unto b ExpToken) => m [b]          -> m ExpOA -> ([(Type,ExpToken -> Bool)] -> m (Type,ExpToken -> Bool)) -> ([ExpToken]          -> ExpObj -> Type -> a) -> m a
mOfFunc :: (Applicative m, Monad m, Unto b ExpObj)   => m [(FuncTA,b)] -> m ExpOA -> ([(Type,ExpObj   -> Bool)] -> m (Type,ExpObj   -> Bool)) -> ([(ExpToken,ExpObj)] -> ExpObj -> Type -> a) -> m a
mOrLit  :: (Applicative m, Monad m) => m [ExpTS]          -> m ExpOA -> ([(Type,ExpToken -> Bool)] -> m (Type,ExpToken -> Bool)) -> m OrLit
mOrFunc :: (Applicative m, Monad m) => m [(FuncTA,ExpOA)] -> m ExpOA -> ([(Type,ExpObj   -> Bool)] -> m (Type,ExpObj   -> Bool)) -> m OrFunc
