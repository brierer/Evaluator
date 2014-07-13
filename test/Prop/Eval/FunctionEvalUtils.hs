{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Prop.Eval.FunctionEvalUtils where

import Prelude hiding (any)

import Control.Arrow  (second)

import Control.Applicative
import Control.Monad.State
import Data.ExpToken
import Data.ExpObj
import Test.Framework

import Prop.Parser.MonolithicParserUtils

{-# ANN module "HLint: ignore Use camelCase" #-}

data NbArgs = NbArgs Pos String Int Int deriving (Show)
instance Arbitrary NbArgs where
  arbitrary               = mNbArgs  arbitrary  arbitrary   arbitrary   arbitrary
  shrink (NbArgs p s n m) = mNbArgs (tShrink p) (shrink s) (tShrink n) (tShrink m)
mNbArgs = liftMF4 NbArgs un id un un

data TableTypeError = TableTypeError ExpObj deriving (Show)
instance Arbitrary    TableTypeError where
  arbitrary                 = mTableTypeError arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary
  shrink (TableTypeError e) = mShrinkTypeError    TableTypeError $ tShrink e
mTableTypeError             = mArbitraryTypeError TableTypeError  
  
data PlotTypeError = PlotTypeError ExpObj deriving (Show)
instance Arbitrary   PlotTypeError where
  arbitrary                = mPlotTypeError arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary
  shrink (PlotTypeError e) = mShrinkTypeError    PlotTypeError $ tShrink e
mPlotTypeError             = mArbitraryTypeError PlotTypeError  

data ArrTypeError = ArrTypeError ExpObj deriving (Show)
instance Arbitrary  ArrTypeError where
  arbitrary               = mArrTypeError arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary
  shrink (ArrTypeError e) = mShrinkTypeError    ArrTypeError $ tShrink e
mArrTypeError             = mArbitraryTypeError ArrTypeError  
  
data ObjTypeError = ObjTypeError ExpObj deriving (Show)
instance Arbitrary  ObjTypeError where
  arbitrary               = mObjTypeError arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary
  shrink (ObjTypeError e) = mShrinkTypeError    ObjTypeError $ tShrink e
mObjTypeError             = mArbitraryTypeError ObjTypeError  
  
data StrTypeError = StrTypeError ExpObj deriving (Show)
instance Arbitrary  StrTypeError where
  arbitrary               = mStrTypeError arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary
  shrink (StrTypeError e) = mShrinkTypeError    StrTypeError $ tShrink e
mStrTypeError             = mArbitraryTypeError StrTypeError  
  
data NumTypeError = NumTypeError ExpObj deriving (Show)
instance Arbitrary  NumTypeError where
  arbitrary               = mNumTypeError arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary
  shrink (NumTypeError e) = mShrinkTypeError    NumTypeError $ tShrink e
mNumTypeError             = mArbitraryTypeError NumTypeError  
  
data BoolTypeError = BoolTypeError ExpObj deriving (Show)
instance Arbitrary   BoolTypeError where
  arbitrary                = mBoolTypeError arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary
  shrink (BoolTypeError e) = mShrinkTypeError    BoolTypeError $ tShrink e
mBoolTypeError             = mArbitraryTypeError BoolTypeError  
  
data NullTypeError = NullTypeError ExpObj deriving (Show)
instance Arbitrary   NullTypeError where
  arbitrary                = mNullTypeError arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary
  shrink (NullTypeError e) = mShrinkTypeError    NullTypeError $ tShrink e
mNullTypeError             = mArbitraryTypeError NullTypeError  

mArbitraryTypeError f a1 a2 a3 a4 a5 a6 a7 = (liftM f.elements) =<< sequence [liftM un a1,liftM un a2,liftM un a3,liftM un a4,liftM un a5,liftM un a6,liftM un a7]
mShrinkTypeError f = map (f.un)

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
instance Tall TableOA      where                                                      tall n = mTableOA  arbitrary  (sListOf $ sListOf $ tall n) (sListOf $ tall n)
mTableOA = liftMF3 f un (equalize.map (map un)) (map un) where f x y = TableOA .TableO x y

equalize xss = let n = minimum $ map length xss in map (take n) xss

data PlotOA = PlotOA ExpObj deriving (Show)
instance Unto PlotOA ExpObj where to = PlotOA; un (PlotOA p) = p
instance Arbitrary PlotOA where arbitrary = sized1 tall; shrink (PlotOA (PlotO p ps o)) = mPlotOA (tShrink p) (tShrinks ps)      (tShrinks o)
instance Tall      PlotOA where                                                  tall n = mPlotOA  arbitrary  (sListOf $ tall n) (sListOf $ tall n)
mPlotOA = liftMF3 f un (map un) (map un) where f x y = PlotOA .PlotO x y
instance Unto (ExpOA,ExpOA) (ExpObj,ExpObj) where un (x,y) = (un x, un y); to (x,y) = (to x, to y)
instance Tall (ExpOA,ExpOA) where tall n =  liftM2 (,) (tall n) (tall n)

data ArrOA = ArrOA ExpObj deriving (Show)
instance Unto  ArrOA ExpObj where to = ArrOA; un (ArrOA a) = a
instance Arbitrary ArrOA where arbitrary = sized1 tall; shrink (ArrOA (ArrO p es)) = mArrOA (tShrink p) (tShrinks es)
instance Tall      ArrOA where                                              tall n = mArrOA  arbitrary  (sizes n)
mArrOA = liftMF2 f un (map un) where f x = ArrOA .ArrO x

data ObjOA =  ObjOA ExpObj deriving (Show)
instance Unto ObjOA ExpObj where to = ObjOA; un (ObjOA o) = o
instance Arbitrary ObjOA where arbitrary = sized1 tall; shrink (ObjOA (ObjO p ps)) = mObjOA (tShrink p) (tShrinks ps)
instance Tall      ObjOA where                                              tall n = mObjOA  arbitrary  (sizes n)
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

{-| Mandatory type signatures -}
mNbArgs :: (Applicative m, Monad m) => m P -> m String -> m ValidInt -> m ValidInt -> m NbArgs

mTableTypeError  :: Gen PlotOA  -> Gen ArrOA  -> Gen ObjOA -> Gen StrOA -> Gen NumOA -> Gen BoolOA -> Gen NullOA -> Gen TableTypeError
mPlotTypeError   :: Gen TableOA -> Gen ArrOA  -> Gen ObjOA -> Gen StrOA -> Gen NumOA -> Gen BoolOA -> Gen NullOA -> Gen PlotTypeError
mArrTypeError    :: Gen TableOA -> Gen PlotOA -> Gen ObjOA -> Gen StrOA -> Gen NumOA -> Gen BoolOA -> Gen NullOA -> Gen ArrTypeError
mObjTypeError    :: Gen TableOA -> Gen PlotOA -> Gen ArrOA -> Gen StrOA -> Gen NumOA -> Gen BoolOA -> Gen NullOA -> Gen ObjTypeError
mStrTypeError    :: Gen TableOA -> Gen PlotOA -> Gen ArrOA -> Gen ObjOA -> Gen NumOA -> Gen BoolOA -> Gen NullOA -> Gen StrTypeError
mNumTypeError    :: Gen TableOA -> Gen PlotOA -> Gen ArrOA -> Gen ObjOA -> Gen StrOA -> Gen BoolOA -> Gen NullOA -> Gen NumTypeError
mBoolTypeError   :: Gen TableOA -> Gen PlotOA -> Gen ArrOA -> Gen ObjOA -> Gen StrOA -> Gen NumOA  -> Gen NullOA -> Gen BoolTypeError
mNullTypeError   :: Gen TableOA -> Gen PlotOA -> Gen ArrOA -> Gen ObjOA -> Gen StrOA -> Gen NumOA  -> Gen BoolOA -> Gen NullTypeError

mShrinkTypeError :: (ExpObj -> b) -> [ExpOA] -> [b]

mExpOA   :: (Applicative m, Monad m) => m ExpObj                                       -> m ExpOA
mTableOA :: (Applicative m, Monad m) => m P -> m [[ExpOA]]       -> m [ExpOA]          -> m TableOA
mPlotOA  :: (Applicative m, Monad m) => m P -> m [(ExpOA,ExpOA)] -> m [(String,ExpOA)] -> m PlotOA
mArrOA   :: (Applicative m, Monad m) => m P -> m [ExpOA]                               -> m ArrOA
mObjOA   :: (Applicative m, Monad m) => m P -> m [(String,ExpOA)]                      -> m ObjOA
mStrOA   :: (Applicative m, Monad m) => m P -> m String                                -> m StrOA
mNumOA   :: (Applicative m, Monad m) => m P -> m Double                                -> m NumOA
mBoolOA  :: (Applicative m, Monad m) => m P -> m Bool                                  -> m BoolOA
mNullOA  :: (Applicative m, Monad m) => m P                                            -> m NullOA
