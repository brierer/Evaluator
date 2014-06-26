{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Eval.FunctionEvalTestUtils where

import Prelude hiding                   (null)

import Control.Arrow                    (second)
import Control.Applicative              (Applicative)
import Control.Monad                    (liftM,liftM2,join)
import Data.Eval                        (ExpObj(..),Eval)
import Data.Token                       (PairToken(..),IdToken(..),ExpToken(..))
import Eval.MultiPass                   (applyFunc)
import Parser.MonolithicParserTestUtils (P(..),sizedArrayTA,sizedObjTA,toArrayTA,toObjTA,toStrTA,toNumTA,toBoolTA,toNullTA,unArrayTA,unObjTA,unStrTA,unNumTA,unBoolTA,unNullTA,sized1,sShrink,unP,toP,liftMF2,sizedListOf)
import Test.Framework                   (Arbitrary(..),elements)

class Is a where
  isFunc  :: a -> Bool
  isArray :: a -> Bool
  isObj   :: a -> Bool
  isVar   :: a -> Bool
  isStr   :: a -> Bool
  isNum   :: a -> Bool
  isBool  :: a -> Bool
  isNull  :: a -> Bool
  
instance Is ExpToken where
  isFunc  (FuncT{})  = True; isFunc _  = False
  isArray (ArrayT{}) = True; isArray _ = False
  isObj   (ObjT{})   = True; isObj _   = False
  isVar   (VarT{})   = True; isVar _   = False
  isStr   (StrT{})   = True; isStr _   = False
  isNum   (NumT{})   = True; isNum _   = False
  isBool  (BoolT{})  = True; isBool _  = False
  isNull  (NullT{})  = True; isNull _  = False
  
instance Is ExpObj where
  isFunc = error "FunctionEvalTestUtils::isFunc<Obj> [Should not be called]"
  isVar  = error "FunctionEvalTestUtils::isVar<Obj> [Should not be called]"
  
  isArray (ArrayO{}) = True; isArray _ = False
  isObj   (ObjO{})   = True; isObj _   = False
  isStr   (StrO{})   = True; isStr _   = False
  isNum   (NumO{})   = True; isNum _   = False
  isBool  (BoolO{})  = True; isBool _  = False
  isNull  (NullO{})  = True; isNull _  = False

data TestFuncs = TF [ExpToken] deriving (Show)
instance Arbitrary TestFuncs where
  arbitrary                  = mTestFuncs (sizedArrayTA zero)    (sizedObjTA zero)    arbitrary            arbitrary            arbitrary             arbitrary
  shrink (TF [a,o,s,n,b,n']) = mTestFuncs (shrink $ toArrayTA a) (shrink $ toObjTA o) (shrink $ toStrTA s) (shrink $ toNumTA n) (shrink $ toBoolTA b) (shrink $ toNullTA n')
  shrink _                   = error "FunctionEvalTest::shrink [Pattern mismatch in TestFuncs instance]"
mTestFuncs a o s n b n' = liftM TF $ sequence [liftM unArrayTA a, liftM unObjTA o, liftM unStrTA s, liftM unNumTA n, liftM unBoolTA b, liftM unNullTA n']
zero = 0 :: Integer

data ExpOA = ExpOA ExpObj deriving (Show)
instance Arbitrary ExpOA where
  arbitrary        = sized1 sizedExpOA
  shrink (ExpOA e) = mExpOA (shrinkExpO e)
sizedExpOA n = mExpOA $ join $ elements $ [arbStrO, arbNumO, arbBoolO, arbNullO] ++ if n > 0 then [arbArrayO n, arbObjO n] else []
mExpOA = liftM ExpOA

toExpOA = ExpOA
unExpOA (ExpOA e) = e

arbArrayO n = liftM unArrayOA $ sizedArrayOA $ n-1
arbObjO   n = liftM unObjOA   $ sizedObjOA   $ n-1
arbStrO     = liftM unStrOA   arbitrary
arbNumO     = liftM unNumOA   arbitrary
arbBoolO    = liftM unBoolOA  arbitrary
arbNullO    = liftM unNullOA  arbitrary

shrinkExpO x@(ArrayO{}) = liftM unArrayOA $ sShrink $ toArrayOA x
shrinkExpO x@(ObjO{})   = liftM unObjOA   $ sShrink $ toObjOA x
shrinkExpO x@(StrO{})   = liftM unStrOA   $ sShrink $ toStrOA x
shrinkExpO x@(NumO{})   = liftM unNumOA   $ sShrink $ toNumOA x
shrinkExpO x@(BoolO{})  = liftM unBoolOA  $ sShrink $ toBoolOA x
shrinkExpO x@(NullO{})  = liftM unNullOA  $ sShrink $ toNullOA x

data ArrayOA = ArrayOA ExpObj deriving (Show)
instance Arbitrary ArrayOA where
  arbitrary                      = sized1 sizedArrayOA
  shrink (ArrayOA (ArrayO p es)) = mArrayOA (sShrink $ toP p) (sShrink $ map toExpOA es)
sizedArrayOA n                   = mArrayOA arbitrary         (sizedListOf $ sizedExpOA n)
mArrayOA = liftMF2 mkArray unP (map unExpOA) where mkArray x = ArrayOA .ArrayO x
toArrayOA = ArrayOA
unArrayOA (ArrayOA a) = a

data ObjOA = ObjOA ExpObj deriving (Show)
instance Arbitrary ObjOA where
  arbitrary                  = sized1 sizedObjOA
  shrink (ObjOA (ObjO p ps)) = mObjOA (sShrink $ toP p) (sShrink $ map (second toExpOA) ps)
sizedObjOA n                 = mObjOA arbitrary         (sizedListOf $ sizedPairOA n)
mObjOA = liftMF2 mkObj unP (map (second unExpOA)) where mkObj x = ObjOA .ObjO x
sizedPairOA n = liftM2 (,) arbitrary (sizedExpOA n)
toObjOA = ObjOA
unObjOA (ObjOA o) = o

data StrOA = StrOA ExpObj deriving (Show)
instance Arbitrary StrOA where
  arbitrary                 = mStrOA arbitrary         arbitrary
  shrink (StrOA (StrO p s)) = mStrOA (sShrink $ toP p) (sShrink s)
mStrOA = liftMF2 mkStr unP id where mkStr x = StrOA .StrO x
toStrOA = StrOA
unStrOA (StrOA s) = s

data NumOA = NumOA ExpObj deriving (Show)
instance Arbitrary NumOA where
  arbitrary                 = mNumOA arbitrary         arbitrary   
  shrink (NumOA (NumO p v)) = mNumOA (sShrink $ toP p) (sShrink v) 
mNumOA = liftMF2 mkNum unP id where mkNum x = NumOA .NumO x
toNumOA  = NumOA
unNumOA (NumOA n) = n

data BoolOA = BoolOA ExpObj deriving (Show)
instance Arbitrary BoolOA where
  arbitrary                   = mBoolOA arbitrary         arbitrary
  shrink (BoolOA (BoolO p v)) = mBoolOA (sShrink $ toP p) (sShrink v)
mBoolOA = liftMF2 mkBool unP id where mkBool x = BoolOA .BoolO x
toBoolOA = BoolOA
unBoolOA (BoolOA b) = b

data NullOA = NullOA ExpObj deriving (Show)
instance Arbitrary NullOA where
  arbitrary                 = mNullOA arbitrary        
  shrink (NullOA (NullO p)) = mNullOA (sShrink $ toP p)
mNullOA = liftM (NullOA .NullO .unP) 
toNullOA = NullOA
unNullOA (NullOA n) = n

applyFunc' fs p n es = applyFunc fs (mkFunc p n es)
testFunc fs p n = fromRight $ applyFunc' (mkFuncs fs) p n []
fromRight (Right x) = x
fromRight x         = error $ "FunctionEvalTestUtils::fromRight [Failed pattern match ["++show x++"]]"

mkFunc p n = FuncT p w1 (IdT p w2 n)
forAll = flip all
w1 = ""
w2 = ("","")
p0 = (0 :: Int,0 :: Int)

funcNames = ["arrayTestF", "objTestF","strTestF","numTestF","boolTestF","nullTestF"]
mkFuncs = zipWith f funcNames where f name e = (name,([],testF' e))

removeEntry s = filter $ \(x,_,_) -> s /= x

toTupleF (PairT _ (IdT _ _ x) y) = liftM2 (,) (return x) (testF y)

testF' e [] = testF e
testF :: ExpToken -> Eval ExpObj
testF (ArrayT p _ es) = liftM (ArrayO p) $ mapM testF es
testF (ObjT p _ ps)   = liftM (ObjO p)   $ mapM toTupleF ps
testF (StrT p _ s)    = return $ StrO p s
testF (NumT p _ _ n)  = return $ NumO p n
testF (BoolT p _ b)   = return $ BoolO p b 
testF (NullT p _)     = return $ NullO p
testF (VarT p _)      = return $ NullO p
testF e               = error $ "FunctionEvalTestUtils::testF [Failed pattern match ["++show e++"]]"

{-| Monomorphism restriction -}
mExpOA   :: (Applicative m, Monad m) => m ExpObj -> m ExpOA
mArrayOA :: (Applicative m, Monad m) => m P -> m [ExpOA]          -> m ArrayOA
mObjOA   :: (Applicative m, Monad m) => m P -> m [(String,ExpOA)] -> m ObjOA
mStrOA   :: (Applicative m, Monad m) => m P -> m String           -> m StrOA
mNumOA   :: (Applicative m, Monad m) => m P -> m Double           -> m NumOA
mBoolOA  :: (Applicative m, Monad m) => m P -> m Bool             -> m BoolOA
mNullOA  :: (Applicative m, Monad m) => m P -> m NullOA

