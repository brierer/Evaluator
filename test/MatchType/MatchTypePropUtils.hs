{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module MatchType.MatchTypePropUtils where

import Prelude hiding (any)

import qualified Prelude as P

import Control.Arrow  (second)

import Control.Applicative
import Control.Monad.State
import Data.ExpObj
import Data.List
import Data.Type
import Eval.MatchType
import Test.Framework

import Parser.ParserPropUtils
import Parser.ParserUtils

data TableTypeFailure = TableTypeFailure ExpObj deriving (Show)
instance Arbitrary      TableTypeFailure where
  arbitrary                 = mTableTypeFailure arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary
  shrink (TableTypeFailure e) = mShrinkTypeFailure    TableTypeFailure $ tShrink e
mTableTypeFailure             = mArbitraryTypeFailure TableTypeFailure

data PlotTypeFailure = PlotTypeFailure ExpObj deriving (Show)
instance Arbitrary     PlotTypeFailure where
  arbitrary                = mPlotTypeFailure arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary
  shrink (PlotTypeFailure e) = mShrinkTypeFailure    PlotTypeFailure $ tShrink e
mPlotTypeFailure             = mArbitraryTypeFailure PlotTypeFailure

data ArrTypeFailure = ArrTypeFailure ExpObj deriving (Show)
instance Arbitrary    ArrTypeFailure where
  arbitrary               = mArrTypeFailure arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary
  shrink (ArrTypeFailure e) = mShrinkTypeFailure    ArrTypeFailure $ tShrink e
mArrTypeFailure             = mArbitraryTypeFailure ArrTypeFailure

data ObjTypeFailure = ObjTypeFailure ExpObj deriving (Show)
instance Arbitrary    ObjTypeFailure where
  arbitrary               = mObjTypeFailure arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary
  shrink (ObjTypeFailure e) = mShrinkTypeFailure    ObjTypeFailure $ tShrink e
mObjTypeFailure             = mArbitraryTypeFailure ObjTypeFailure

data StrTypeFailure = StrTypeFailure ExpObj deriving (Show)
instance Arbitrary    StrTypeFailure where
  arbitrary               = mStrTypeFailure arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary
  shrink (StrTypeFailure e) = mShrinkTypeFailure    StrTypeFailure $ tShrink e
mStrTypeFailure             = mArbitraryTypeFailure StrTypeFailure

data NumTypeFailure = NumTypeFailure ExpObj deriving (Show)
instance Arbitrary    NumTypeFailure where
  arbitrary               = mNumTypeFailure arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary
  shrink (NumTypeFailure e) = mShrinkTypeFailure    NumTypeFailure $ tShrink e
mNumTypeFailure             = mArbitraryTypeFailure NumTypeFailure

data BoolTypeFailure = BoolTypeFailure ExpObj deriving (Show)
instance Arbitrary     BoolTypeFailure where
  arbitrary                = mBoolTypeFailure arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary
  shrink (BoolTypeFailure e) = mShrinkTypeFailure    BoolTypeFailure $ tShrink e
mBoolTypeFailure             = mArbitraryTypeFailure BoolTypeFailure

data NullTypeFailure = NullTypeFailure ExpObj deriving (Show)
instance Arbitrary     NullTypeFailure where
  arbitrary                = mNullTypeFailure arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary
  shrink (NullTypeFailure e) = mShrinkTypeFailure    NullTypeFailure $ tShrink e
mNullTypeFailure             = mArbitraryTypeFailure NullTypeFailure

mArbitraryTypeFailure f a1 a2 a3 a4 a5 a6 a7 = (liftM f.elements) =<< sequence [liftM un a1,liftM un a2,liftM un a3,liftM un a4,liftM un a5,liftM un a6,liftM un a7]
mShrinkTypeFailure f = map (f.un)

data ExpOA = ExpOA ExpObj deriving (Eq,Show)
instance Unto ExpOA ExpObj where to = ExpOA; un (ExpOA e) = e
instance Arbitrary ExpOA where arbitrary = sized1 tall; shrink (ExpOA e) = mExpOA (shrinkO e)
instance Tall      ExpOA where                                    tall n = mExpOA (randomO n)
randomO n = join $ elements $ [mStrO, mNumO, mBoolO, mNullO] ++ if n > 0 then [mTableO n,mPlotO n, mArrO n, mObjO n] else []
mExpOA = liftM ExpOA

mTableO n = liftM un (tall (n-1) :: Gen TableOA)
mPlotO  n = liftM un (tall (n-1) :: Gen PlotOA)
mArrO   n = liftM un (tall (n-1) :: Gen ArrOA)
mObjO   n = liftM un (tall (n-1) :: Gen ObjOA)
mStrO     = liftM un (arbitrary  :: Gen StrOA)
mNumO     = liftM un (arbitrary  :: Gen NumOA)
mBoolO    = liftM un (arbitrary  :: Gen BoolOA)
mNullO    = liftM un (arbitrary  :: Gen NullOA)

shrinkO x@(TableO{}) = liftM un $ sShrink (to x :: TableOA)
shrinkO x@(PlotO{})  = liftM un $ sShrink (to x :: PlotOA)
shrinkO x@(ArrO{})   = liftM un $ sShrink (to x :: ArrOA)
shrinkO x@(ObjO{})   = liftM un $ sShrink (to x :: ObjOA)
shrinkO x@(StrO{})   = liftM un $ sShrink (to x :: StrOA)
shrinkO x@(NumO{})   = liftM un $ sShrink (to x :: NumOA)
shrinkO x@(BoolO{})  = liftM un $ sShrink (to x :: BoolOA)
shrinkO x@(NullO{})  = liftM un $ sShrink (to x :: NullOA)

data TableOA = TableOA ExpObj deriving (Show)
instance Unto  TableOA ExpObj where to = TableOA; un (TableOA t) = t
instance Arbitrary TableOA where arbitrary = sized1 tall; shrink (TableOA (TableO p ess es)) = mTableOA (tShrink p) (shrink $ map (map to) ess)  (tShrinks es)
instance Tall TableOA      where                                                      tall n = mTableOA  arbitrary  (sListOf $ talls n)          (talls n)
mTableOA = liftMF3 f un (equalize.map (map un)) (map un) where f x y = TableOA .TableO x y

equalize xss = let n = minimum $ map length xss in map (take n) xss

data PlotOA = PlotOA ExpObj deriving (Show)
instance Unto PlotOA ExpObj where to = PlotOA; un (PlotOA p) = p
instance Arbitrary PlotOA where arbitrary = sized1 tall; shrink (PlotOA (PlotO p ps o)) = mPlotOA (tShrink p) (tShrinks ps) (tShrinks o)
instance Tall      PlotOA where                                                  tall n = mPlotOA  arbitrary  (talls n)     (talls n)
mPlotOA = liftMF3 f un (map un) (map un) where f x y = PlotOA .PlotO x y
instance Unto (ExpOA,ExpOA) (ExpObj,ExpObj) where un (x,y) = (un x, un y); to (x,y) = (to x, to y)
instance Tall (ExpOA,ExpOA) where tall n =  liftM2 (,) (tall n) (tall n)

data ArrOA = ArrOA ExpObj deriving (Show)
instance Unto  ArrOA ExpObj where to = ArrOA; un (ArrOA a) = a
instance Arbitrary ArrOA where arbitrary = sized1 tall; shrink (ArrOA (ArrO p es)) = mArrOA (tShrink p) (tShrinks es)
instance Tall      ArrOA where                                              tall n = mArrOA  arbitrary  (talls n)
mArrOA = liftMF2 f un (map un) where f x = ArrOA .ArrO x

data ObjOA =  ObjOA ExpObj deriving (Show)
instance Unto ObjOA ExpObj where to = ObjOA; un (ObjOA o) = o
instance Arbitrary ObjOA where arbitrary = sized1 tall; shrink (ObjOA (ObjO p ps)) = mObjOA (tShrink p) (tShrinks ps)
instance Tall      ObjOA where                                              tall n = mObjOA  arbitrary  (talls n)
mObjOA = liftMF2 f un (map (second un)) where f x = ObjOA .ObjO x
instance Unto (String,ExpOA) (String,ExpObj) where un = second un; to = second to
instance Tall (String,ExpOA) where tall n =  liftM2 (,) arbitrary (tall n)

data StrOA =  StrOA ExpObj deriving (Show)
instance Unto StrOA ExpObj where to = StrOA; un (StrOA s) = s
instance Arbitrary StrOA where
  arbitrary                 = mStrOA  arbitrary   arbitrary
  shrink (StrOA (StrO p s)) = mStrOA (tShrink p) (sShrink s)
mStrOA = liftMF2 f un id where f x = StrOA .StrO x

data NumOA =  NumOA ExpObj deriving (Show)
instance Unto NumOA ExpObj where to  = NumOA; un (NumOA n) = n
instance Arbitrary NumOA where
  arbitrary                 = mNumOA arbitrary         arbitrary
  shrink (NumOA (NumO p v)) = mNumOA (tShrink p) (sShrink v)
mNumOA = liftMF2 f un id where f x = NumOA .NumO x

data BoolOA = BoolOA ExpObj deriving (Show)
instance Unto BoolOA ExpObj where to = BoolOA; un (BoolOA b) = b
instance Arbitrary BoolOA where
  arbitrary                   = mBoolOA arbitrary         arbitrary
  shrink (BoolOA (BoolO p v)) = mBoolOA (tShrink p) (sShrink v)
mBoolOA = liftMF2 f un id where f x = BoolOA .BoolO x

data NullOA = NullOA ExpObj deriving (Show)
instance Unto NullOA ExpObj where to = NullOA; un (NullOA n) = n
instance Arbitrary NullOA where
  arbitrary                 = mNullOA arbitrary
  shrink (NullOA (NullO p)) = mNullOA (tShrink p)
mNullOA = liftM (NullOA .NullO .un)

data TypeA =  TypeA Type deriving (Show)
instance Unto TypeA Type where to = TypeA; un (TypeA t) = t
instance Tall TypeA where tall = mTypeA
instance Arbitrary TypeA where
  arbitrary = sized1 tall

  shrink (TypeA (ArrOf t)) = [TypeA $ ArrOf $ un  (t' :: TypeA)     | t' <- tShrink t]
  shrink (TypeA (ObjOf t)) = [TypeA $ ObjOf $ un  (t' :: TypeA)     | t' <- tShrink t]
  shrink (TypeA (Or ts))   = [TypeA $ Or $ map un (ts' :: [TypeA])  | ts' <- tShrinks ts]
  shrink _         = []

mTypeA n = do
  t <- liftM un $ mTypeA (n-1)
  ts <- liftM (nub.map un) (talls $ n-1 :: Gen [TypeA])
  liftM TypeA $ elements $ [Table,Plot,Str,Num,Bool,Null] ++ if n == 0 then [] else [ArrOf t,ObjOf t,Or ts]


data ArrOfTypeFailure = ArrOfTypeFailure ExpObj ExpObj Type Type deriving(Show)
instance Arbitrary ArrOfTypeFailure where arbitrary = sized1 tall; shrink (ArrOfTypeFailure _ _ at bt) = mArrOfTypeFailure (tShrink at) (tShrink bt)
instance Tall      ArrOfTypeFailure where                                                       tall n = mArrOfTypeFailure (tall n)     (tall n)
mArrOfTypeFailure ata bta = do
  arrType <- liftM (noOr.un) ata
  badType <- liftM (noOr.un) bta
  let (at,bt) = if arrType `accepts` badType then (arrType,ArrOf arrType) else (arrType,badType)
      ArrO _ es = makeMatchingExp $ ArrOf at
      e = makeMatchingExp bt
      a = ArrO p0 $ es ++ [e]
  return $ ArrOfTypeFailure a e at bt

data ObjOfTypeFailure = ObjOfTypeFailure ExpObj ExpObj Type Type deriving(Show)
instance Arbitrary ObjOfTypeFailure where arbitrary = sized1 tall; shrink (ObjOfTypeFailure _ _ ot bt) = mObjOfTypeFailure (tShrink ot) (tShrink bt)
instance Tall      ObjOfTypeFailure where                                                       tall n = mObjOfTypeFailure (tall n)     (tall n)
mObjOfTypeFailure ota bta = do
  objType <- liftM (noOr.un) ota
  badType <- liftM (noOr.un) bta
  let (ot,bt) = if objType `accepts` badType then (objType,ObjOf objType) else (objType,badType)
      ObjO _ es = makeMatchingExp $ ObjOf ot
      e = makeMatchingExp bt
      o = ObjO p0 $ es ++ [("",e)]
  return $ ObjOfTypeFailure o e ot bt

data OrTypeFailure = OrTypeFailure ExpObj Type [Type] deriving(Show)
instance Arbitrary OrTypeFailure where arbitrary = sized1 tall; shrink (OrTypeFailure _ t ts) = mOrTypeFailure (tShrink t) (tShrinks ts)
instance Tall      OrTypeFailure where                                                 tall n = mOrTypeFailure (tall n)    (talls n)
mOrTypeFailure t ts = do
    t'  <- liftM (noOr.un) t
    ts' <- liftM (nub.filter (not.(`accepts`t')).map un) ts
    return $ OrTypeFailure (makeMatchingExp t') t' ts'

data ArrOfTypeSuccess = ArrOfTypeSuccess ExpObj Type deriving(Show)
instance Arbitrary ArrOfTypeSuccess where arbitrary = sized1 tall; shrink (ArrOfTypeSuccess _ t) = mArrOfTypeSuccess (tShrink t)
instance Tall      ArrOfTypeSuccess where                                                 tall n = mArrOfTypeSuccess (tall n)
mArrOfTypeSuccess ta = do
  t <- liftM (noOr.un) ta
  t2 <- liftM un ta
  return $ ArrOfTypeSuccess (makeMatchingExp $ ArrOf t) $ Or [t2,t]

data ObjOfTypeSuccess = ObjOfTypeSuccess ExpObj Type deriving(Show)
instance Arbitrary ObjOfTypeSuccess where arbitrary = sized1 tall; shrink (ObjOfTypeSuccess _ t) = mObjOfTypeSuccess (tShrink t)
instance Tall      ObjOfTypeSuccess where                                                 tall n = mObjOfTypeSuccess (tall n)
mObjOfTypeSuccess ta = do
  t <- liftM (noOr.un) ta
  t2 <- liftM un ta
  return $ ObjOfTypeSuccess (makeMatchingExp $ ObjOf t) $ Or [t2,t]

data OrTypeSuccess = OrTypeSuccess ExpObj Type [Type] deriving(Show)
instance Arbitrary OrTypeSuccess where arbitrary = sized1 tall; shrink (OrTypeSuccess _ t ts) = mOrTypeSuccess (tShrink t) (tShrinks ts)
instance Tall      OrTypeSuccess where                                                 tall n = mOrTypeSuccess (tall n)    (talls n)
mOrTypeSuccess t ts = do
    t'  <- liftM (noOr.un) t
    ts' <- liftM (filter (`accepts`t').map un) ts
    return $ OrTypeSuccess (makeMatchingExp t') t' (nub $ t':ts')

simpleMatch e t = evalStateT (matchType t e) []

_          `accepts` (Or ts)    = error $ "TypeEvalUtils::accepts [Cannot accept the Or of types "++show ts++"]"
(ArrOf t1) `accepts` (ArrOf t2) = t1 `accepts` t2
(ObjOf t1) `accepts` (ObjOf t2) = t1 `accepts` t2
(Or ts)    `accepts` t          = P.any (`accepts`t) ts
t1         `accepts` t2         = t1 == t2

noOr (Or []) = Null
noOr (Or (t:_)) = noOr t
noOr (ArrOf t) = ArrOf $ noOr t
noOr (ObjOf t) = ObjOf $ noOr t
noOr t         = t

makeMatchingExp Table     = TableO p0 [] []
makeMatchingExp Plot      = PlotO  p0 [] []
makeMatchingExp Str       = StrO   p0 ""
makeMatchingExp Num       = NumO   p0 0
makeMatchingExp Bool      = BoolO  p0 False
makeMatchingExp Null      = NullO  p0
makeMatchingExp (ArrOf t) = ArrO p0 [makeMatchingExp t]
makeMatchingExp (ObjOf t) = ObjO p0 [("",makeMatchingExp t)]

{-| Mandatory type signatures -}
mTableTypeFailure  :: Gen PlotOA  -> Gen ArrOA  -> Gen ObjOA -> Gen StrOA -> Gen NumOA -> Gen BoolOA -> Gen NullOA -> Gen TableTypeFailure
mPlotTypeFailure   :: Gen TableOA -> Gen ArrOA  -> Gen ObjOA -> Gen StrOA -> Gen NumOA -> Gen BoolOA -> Gen NullOA -> Gen PlotTypeFailure
mArrTypeFailure    :: Gen TableOA -> Gen PlotOA -> Gen ObjOA -> Gen StrOA -> Gen NumOA -> Gen BoolOA -> Gen NullOA -> Gen ArrTypeFailure
mObjTypeFailure    :: Gen TableOA -> Gen PlotOA -> Gen ArrOA -> Gen StrOA -> Gen NumOA -> Gen BoolOA -> Gen NullOA -> Gen ObjTypeFailure
mStrTypeFailure    :: Gen TableOA -> Gen PlotOA -> Gen ArrOA -> Gen ObjOA -> Gen NumOA -> Gen BoolOA -> Gen NullOA -> Gen StrTypeFailure
mNumTypeFailure    :: Gen TableOA -> Gen PlotOA -> Gen ArrOA -> Gen ObjOA -> Gen StrOA -> Gen BoolOA -> Gen NullOA -> Gen NumTypeFailure
mBoolTypeFailure   :: Gen TableOA -> Gen PlotOA -> Gen ArrOA -> Gen ObjOA -> Gen StrOA -> Gen NumOA  -> Gen NullOA -> Gen BoolTypeFailure
mNullTypeFailure   :: Gen TableOA -> Gen PlotOA -> Gen ArrOA -> Gen ObjOA -> Gen StrOA -> Gen NumOA  -> Gen BoolOA -> Gen NullTypeFailure

mShrinkTypeFailure :: (ExpObj -> b) -> [ExpOA] -> [b]

mExpOA   :: (Applicative m, Monad m) => m ExpObj                                       -> m ExpOA
mTableOA :: (Applicative m, Monad m) => m P -> m [[ExpOA]]       -> m [ExpOA]          -> m TableOA
mPlotOA  :: (Applicative m, Monad m) => m P -> m [(ExpOA,ExpOA)] -> m [(String,ExpOA)] -> m PlotOA
mArrOA   :: (Applicative m, Monad m) => m P -> m [ExpOA]                               -> m ArrOA
mObjOA   :: (Applicative m, Monad m) => m P -> m [(String,ExpOA)]                      -> m ObjOA
mStrOA   :: (Applicative m, Monad m) => m P -> m String                                -> m StrOA
mNumOA   :: (Applicative m, Monad m) => m P -> m Double                                -> m NumOA
mBoolOA  :: (Applicative m, Monad m) => m P -> m Bool                                  -> m BoolOA
mNullOA  :: (Applicative m, Monad m) => m P                                            -> m NullOA

mArrOfTypeFailure :: (Applicative m, Monad m) => m TypeA -> m  TypeA  -> m ArrOfTypeFailure
mObjOfTypeFailure :: (Applicative m, Monad m) => m TypeA -> m  TypeA  -> m ObjOfTypeFailure
mOrTypeFailure    :: (Applicative m, Monad m) => m TypeA -> m [TypeA] -> m OrTypeFailure
mArrOfTypeSuccess :: (Applicative m, Monad m) => m TypeA              -> m ArrOfTypeSuccess
mObjOfTypeSuccess :: (Applicative m, Monad m) => m TypeA              -> m ObjOfTypeSuccess
mOrTypeSuccess    :: (Applicative m, Monad m) => m TypeA -> m [TypeA] -> m OrTypeSuccess

