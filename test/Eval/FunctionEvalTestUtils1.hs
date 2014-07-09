{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Eval.FunctionEvalTestUtils1 where

import Prelude hiding                   (any)

import Control.Applicative              (Applicative)
import Control.Arrow                    (second)
import Control.Monad                    (join,liftM,liftM2)
import Control.Monad.State              (evalStateT,get)
import Data.Eval                        (Func(..))
import Data.ExpObj                      (Type (..),ExpObj(..))
import Data.ExpToken                    (ExpToken (..),IdToken (..),PairToken (..))
import Eval.Function                    (any,withFuncs)
import Eval.MultiPass                   (mapPair)
import Parser.MonolithicParserTestUtils (ArrayTA (..),BoolTA (..),ExpTA (..),IdTA (..),NullTA (..),NumTA (..),ObjTA (..),P (..),StrTA (..),Tall (..),Unto (..),
                                         liftMF2,liftMF3,sListOf,sShrink,sized1,sizes,tShrink,tShrinks,uns)
import Test.Framework                   (Arbitrary (..),Gen,elements)

data TestToks = TestToks [ExpToken] deriving (Show)
instance Arbitrary TestToks where
  arbitrary                         = mTestToks (tall zero) (tall zero)  arbitrary   arbitrary    arbitrary   arbitrary
  shrink (TestToks [a,o,s,nb,b,nu]) = mTestToks (tShrink a) (tShrink o) (tShrink s) (tShrink nb) (tShrink b) (tShrink nu)
  shrink x                          = error $ "FunctionEvalTestUtils::shrink<TestToks> [Pattern mismatch ["++show x++"]]"
mTestToks a o s n b n' = liftM TestToks $ sequence [liftM un a, liftM un o, liftM un s, liftM un n, liftM un b, liftM un n']
zero = 0 :: Int

data TestObjs = TestObjs [ExpObj] deriving (Show)
instance Arbitrary TestObjs where
  arbitrary               = mTestObjs (tall zero) (tall zero)
  shrink (TestObjs [t,p]) = mTestObjs (tShrink t) (tShrink p)
  shrink x                = error $ "FunctionEvalTestUtils::shrink<TestObjs> [Pattern mismatch ["++show x++"]]"
mTestObjs t p = liftM TestObjs $ sequence [liftM un t, liftM un p]

data ExpOA = ExpOA ExpObj deriving (Eq,Show)
instance Unto ExpOA ExpObj where to = ExpOA; un (ExpOA e) = e
instance Arbitrary ExpOA where arbitrary = sized1 tall; shrink (ExpOA e) = mExpOA (shrinkO e)
instance Tall      ExpOA where                                    tall n =  mExpOA (randomO n)
randomO n = join $ elements $ [mStrO, mNumO, mBoolO, mNullO] ++ if n > 0 then [mTableO n,mPlotO n, mArrayO n, mObjO n] else []
mExpOA = liftM ExpOA

mTableO n = liftM un (tall (n-1) :: Gen TableOA)
mPlotO  n = liftM un (tall (n-1) :: Gen PlotOA)
mArrayO n = liftM un (tall (n-1) :: Gen ArrayOA)
mObjO   n = liftM un (tall (n-1) :: Gen ObjOA)
mStrO     = liftM un (arbitrary  :: Gen StrOA)
mNumO     = liftM un (arbitrary  :: Gen NumOA)
mBoolO    = liftM un (arbitrary  :: Gen BoolOA)
mNullO    = liftM un (arbitrary  :: Gen NullOA)

shrinkO x@(TableO{})  = liftM un $ sShrink (to x :: TableOA)
shrinkO x@(PlotO{})   = liftM un $ sShrink (to x :: PlotOA)
shrinkO x@(ArrayO{})  = liftM un $ sShrink (to x :: ArrayOA)
shrinkO x@(ObjO{})    = liftM un $ sShrink (to x :: ObjOA)
shrinkO x@(StrO{})    = liftM un $ sShrink (to x :: StrOA)
shrinkO x@(NumO{})    = liftM un $ sShrink (to x :: NumOA)
shrinkO x@(BoolO{})   = liftM un $ sShrink (to x :: BoolOA)
shrinkO x@(NullO{})   = liftM un $ sShrink (to x :: NullOA)

data TableOA = TableOA ExpObj deriving (Show)
instance Unto  TableOA ExpObj where to = TableOA; un (TableOA t) = t
instance Arbitrary TableOA where arbitrary = sized1 tall; shrink (TableOA (TableO p ess es)) = mTableOA (tShrink p) (shrink $ map (map to) ess)  (tShrinks es)
instance Tall TableOA      where                                                      tall n = mTableOA  arbitrary  (sListOf $ sListOf $ tall n) (sListOf $ tall n)
mTableOA = liftMF3 mkTable un (equalize.map (map un)) uns where mkTable x y = TableOA .TableO x y

equalize xss = let n = minimum $ map length xss in map (take n) xss

data PlotOA = PlotOA ExpObj deriving (Show)
instance Unto PlotOA ExpObj where to = PlotOA; un (PlotOA p) = p
instance Arbitrary PlotOA where arbitrary = sized1 tall; shrink (PlotOA (PlotO p ps o)) = mPlotOA (tShrink p) (tShrinks ps)      (tShrinks o)
instance Tall      PlotOA where                                                  tall n = mPlotOA  arbitrary  (sListOf $ tall n) (sListOf $ tall n)
mPlotOA = liftMF3 mkPlot un uns uns where mkPlot x y = PlotOA .PlotO x y
instance Unto (ExpOA,ExpOA) (ExpObj,ExpObj) where un (x,y) = (un x, un y); to (x,y) = (to x, to y)
instance Tall (ExpOA,ExpOA) where tall n =  liftM2 (,) (tall n) (tall n)

data ArrayOA = ArrayOA ExpObj deriving (Show)
instance Unto  ArrayOA ExpObj where to = ArrayOA; un (ArrayOA a) = a
instance Arbitrary ArrayOA where arbitrary = sized1 tall; shrink (ArrayOA (ArrayO p es)) = mArrayOA (tShrink p) (tShrinks es)
instance Tall      ArrayOA where                                                  tall n = mArrayOA  arbitrary  (sizes n)
mArrayOA = liftMF2 mkArray un uns where mkArray x = ArrayOA .ArrayO x

data ObjOA =  ObjOA ExpObj deriving (Show)
instance Unto ObjOA ExpObj where to = ObjOA; un (ObjOA o) = o
instance Arbitrary ObjOA where arbitrary = sized1 tall; shrink (ObjOA (ObjO p ps)) = mObjOA (tShrink p) (tShrinks ps)
instance Tall      ObjOA where                                              tall n = mObjOA  arbitrary  (sizes n)
mObjOA = liftMF2 mkObj un (map (second un)) where mkObj x = ObjOA .ObjO x
instance Unto (String,ExpOA) (String,ExpObj) where un = second un; to = second to
instance Tall (String,ExpOA) where tall n =  liftM2 (,) arbitrary (tall n)

data StrOA =  StrOA ExpObj deriving (Show)
instance Unto StrOA ExpObj where to = StrOA; un (StrOA s) = s
instance Arbitrary StrOA where
  arbitrary                 = mStrOA  arbitrary   arbitrary
  shrink (StrOA (StrO p s)) = mStrOA (tShrink p) (sShrink s)
mStrOA = liftMF2 mkStr un id where mkStr x = StrOA .StrO x

data NumOA =  NumOA ExpObj deriving (Show)
instance Unto NumOA ExpObj where to  = NumOA; un (NumOA n) = n
instance Arbitrary NumOA where
  arbitrary                 = mNumOA arbitrary         arbitrary
  shrink (NumOA (NumO p v)) = mNumOA (tShrink p) (sShrink v)
mNumOA = liftMF2 mkNum un id where mkNum x = NumOA .NumO x

data BoolOA = BoolOA ExpObj deriving (Show)
instance Unto BoolOA ExpObj where to = BoolOA; un (BoolOA b) = b
instance Arbitrary BoolOA where
  arbitrary                   = mBoolOA arbitrary         arbitrary
  shrink (BoolOA (BoolO p v)) = mBoolOA (tShrink p) (sShrink v)
mBoolOA = liftMF2 mkBool un id where mkBool x = BoolOA .BoolO x

data NullOA = NullOA ExpObj deriving (Show)
instance Unto NullOA ExpObj where to = NullOA; un (NullOA n) = n
instance Arbitrary NullOA where
  arbitrary                 = mNullOA arbitrary
  shrink (NullOA (NullO p)) = mNullOA (tShrink p)
mNullOA = liftM (NullOA .NullO .un)

data ExpTS =  ExpTS ExpToken deriving (Eq,Show)
instance Unto ExpTS ExpToken where to = ExpTS; un (ExpTS e) = e
instance Arbitrary ExpTS where arbitrary = sized1 tall; shrink (ExpTS e) = mExpTS (tShrink e)
instance Tall ExpTS where                                         tall n = mExpTS (tall n)
mExpTS = liftM (ExpTS .simplify.un)

data ArrayTS = ArrayTS ExpToken deriving (Show)
instance Unto ArrayTS ExpToken where to = ArrayTS; un (ArrayTS a) = a
instance Arbitrary ArrayTS where arbitrary = sized1 tall; shrink (ArrayTS e) = mArrayTS (tShrink e)
instance Tall      ArrayTS where                                      tall n =  mArrayTS (tall n)
mArrayTS = liftM (ArrayTS .simplify.un)

data ObjTS = ObjTS ExpToken deriving (Show)
instance Unto ObjTS ExpToken where to = ObjTS; un (ObjTS o) = o
instance Arbitrary ObjTS where arbitrary = sized1 tall; shrink (ObjTS e) = mObjTS (tShrink e)
instance Tall      ObjTS where                                    tall n =  mObjTS (tall n)
mObjTS = liftM (ObjTS .simplify.un)

simplify (FuncT{})          = NullT p0 w2
simplify (ArrayT p w es)    = ArrayT p w $ map  simplify          es
simplify (ObjT p w ps)      = ObjT   p w $ map (mapPair simplify) ps
simplify (VarT (IdT p w _)) = NullT  p w
simplify e                  = e

data ArrayTF =  ArrayTF ExpToken ExpObj IdToken deriving (Show)
instance Arbitrary ArrayTF where arbitrary = sized1 tall; shrink (ArrayTF e o i) = mArrayTF (tShrink e) (tShrink o) (tShrink i)
instance Tall      ArrayTF where                                          tall n = mArrayTF (tall n)    (tall n)     arbitrary
mArrayTF = makeTF ArrayTF

data ObjTF =  ObjTF ExpToken ExpObj IdToken deriving (Show)
instance Arbitrary ObjTF where arbitrary = sized1 tall; shrink (ObjTF e o i) = mObjTF (tShrink e) (tShrink o) (tShrink i)
instance Tall      ObjTF where                                        tall n = mObjTF (tall n)    (tall n)     arbitrary
mObjTF = makeTF ObjTF

makeTF f ea oa ia = do e <- ea; o <- oa; i <- ia; return $ f (simplifyF (un i) $ un e) (un o) (un i)

simplifyF i (FuncT w _ _)      = FuncT    w i []
simplifyF i (ArrayT p w es)    = ArrayT p w $ map  (simplifyF i)          es
simplifyF i (ObjT p w ps)      = ObjT   p w $ map (mapPair $ simplifyF i) ps
simplifyF _ (VarT (IdT p w _)) = NullT  p w
simplifyF _ e                  = e

applyFunc fs p n es = withFuncs fs any (mkFunc p n es)

testFunc os es p n = fromRight $ applyFunc (mkFuncs os es) p n []

fromRight (Right x) = x
fromRight x         = error $ "FunctionEvalTestUtils::fromRight [Failed pattern match ["++show x++"]]"

mkFunc p n = FuncT w1 (IdT p w2 n)

funcNamesLit   = ["arrayTestF","objTestF","strTestF","numTestF","boolTestF","nullTestF"]
funcNamesNoLit = ["tableTestF","plotTestF"]
mkFuncs os es = zipWith f funcNamesNoLit os ++ zipWith g funcNamesLit es
  where f name e = (name,([],Func $ \_ _ -> return e))
        g name e = (name,([],Func $ \_ _ -> testE e))

forAll = flip all
w1 = ""
w2 = ("","")
ws2 = w2
p0 = (0 :: Int,0 :: Int)

types = [Table,Plot,Arr,Obj,Str,Num,Bool,Null]
removeEntry n = filter $ \(m,_,_) -> n /= m

getTall (FuncT _ _ es)  = length es
getTall (ArrayT _ _ es) = length es
getTall (ObjT _ _ ps)   = length ps

toTupleF (PairT (IdT _ _ x) y) = liftM2 (,) (return x) (testE y)

testS = testF []
testF fs = flip evalStateT fs .testE

testE (FuncT _ (IdT p _ i) es) = get >>= \fs -> case lookup i fs of Just (_,Func f) -> mapM testE es >>= f p
testE (ArrayT p _ es) = liftM (ArrayO p) $ mapM testE es
testE (ObjT p _ ps)   = liftM (ObjO p)   $ mapM toTupleF ps
testE (StrT p _ s)    = return $ StrO p s
testE (NumT p _ _ n)  = return $ NumO p n
testE (BoolT p _ b)   = return $ BoolO p b
testE (NullT p _)     = return $ NullO p
testE e               = error $ "FunctionEvalTestUtils::testF [Failed pattern match ["++show e++"]]"

{-| Monomorphism restriction -}
mTestToks :: (Applicative m, Monad m) => m ArrayTS -> m ObjTS -> m StrTA -> m NumTA -> m BoolTA -> m NullTA -> m TestToks
mTestObjs :: (Applicative m, Monad m) => m TableOA -> m PlotOA                                              -> m TestObjs

mExpOA    :: (Applicative m, Monad m) => m ExpObj                                       -> m ExpOA
mTableOA  :: (Applicative m, Monad m) => m P -> m [[ExpOA]]       -> m [ExpOA]          -> m TableOA
mPlotOA   :: (Applicative m, Monad m) => m P -> m [(ExpOA,ExpOA)] -> m [(String,ExpOA)] -> m PlotOA
mArrayOA  :: (Applicative m, Monad m) => m P -> m [ExpOA]                               -> m ArrayOA
mObjOA    :: (Applicative m, Monad m) => m P -> m [(String,ExpOA)]                      -> m ObjOA
mStrOA    :: (Applicative m, Monad m) => m P -> m String                                -> m StrOA
mNumOA    :: (Applicative m, Monad m) => m P -> m Double                                -> m NumOA
mBoolOA   :: (Applicative m, Monad m) => m P -> m Bool                                  -> m BoolOA
mNullOA   :: (Applicative m, Monad m) => m P                                            -> m NullOA

mExpTS    :: (Applicative m, Monad m) => m ExpTA   -> m ExpTS
mArrayTS  :: (Applicative m, Monad m) => m ArrayTA -> m ArrayTS
mObjTS    :: (Applicative m, Monad m) => m ObjTA   -> m ObjTS

mArrayTF  :: (Applicative m, Monad m) => m ArrayTA -> m ExpOA -> m IdTA -> m ArrayTF
mObjTF    :: (Applicative m, Monad m) => m ObjTA   -> m ExpOA -> m IdTA -> m ObjTF


