{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}
module MatchType.MatchTypePropUtils where

import Prelude   hiding (any)
import Data.List hiding (any)

import qualified Prelude as P

import Control.Applicative
import Control.Monad.State
import Data.Eval
import Data.EvalError
import Data.ExpObj
import Data.Type
import Eval.MatchType
import Test.Framework

import Marshall.MarshallPropUtils
import Marshall.MarshallUtils
import MatchType.MatchTypeUtils
import Parser.ParserPropUtils

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

type TableOA = TablePA COP
type PlotOA  = PlotPA  COP
type ArrOA   = ArrPA   COP
type ObjOA   = ObjPA   COP
type StrOA   = StrPA   UOP
type NumOA   = NumPA   UOP
type BoolOA  = BoolPA  UOP
type NullOA  = NullPA  UOP

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
      a = arrO $ es ++ [e]
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
      o = objO $ es ++ [("",e)]
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

makeMatchingExp Table     = tableO [] []
makeMatchingExp Plot      = plotO  [] []
makeMatchingExp Str       = strO   ""
makeMatchingExp Num       = numO   0
makeMatchingExp Bool      = boolO  False
makeMatchingExp Null      = nullO
makeMatchingExp (ArrOf t) = arrO [makeMatchingExp t]
makeMatchingExp (ObjOf t) = objO [("",makeMatchingExp t)]

matchOr ::  ExpObj -> [Type] -> Eval a
matchOr e ts = simplify e $ map (f.simpleMatch e) ts where
  f (Left (TypeMismatch t)) = t
  f x                       = error $ "Eval.MatchType::orType::f [Unexpected pattern ["++show x++"]]"

simplify :: ExpObj -> [TMTree] -> Eval a
simplify x [] = Left $ TypeMismatch $ TMLeaf (getPos x) (NodeOr []) (getRoot x)
simplify _ ts | P.any (not.(\t -> case t of TMLeaf{} -> True; _ -> False)) ts = Left $ TypeMismatch $ TMNode ts
              | otherwise = let (canBeSame,orTypes) = unzip $ map (\(TMLeaf x y z) -> ((x,z),y)) ts
                                (p,t2) = head canBeSame
                                canBeSimplified = length (nub canBeSame) == 1
                            in Left $ TypeMismatch $ if canBeSimplified then TMLeaf p (NodeOr $ sortBy (flip compare) (nub orTypes)) t2 else TMNode ts

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

mArrOfTypeFailure :: (Applicative m, Monad m) => m TypeA -> m  TypeA  -> m ArrOfTypeFailure
mObjOfTypeFailure :: (Applicative m, Monad m) => m TypeA -> m  TypeA  -> m ObjOfTypeFailure
mOrTypeFailure    :: (Applicative m, Monad m) => m TypeA -> m [TypeA] -> m OrTypeFailure
mArrOfTypeSuccess :: (Applicative m, Monad m) => m TypeA              -> m ArrOfTypeSuccess
mObjOfTypeSuccess :: (Applicative m, Monad m) => m TypeA              -> m ObjOfTypeSuccess
mOrTypeSuccess    :: (Applicative m, Monad m) => m TypeA -> m [TypeA] -> m OrTypeSuccess

