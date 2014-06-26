{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Eval.FunctionEvalTestUtils where

import Prelude hiding                   (null)

import Control.Arrow                    (second)
import Control.Applicative              (Applicative)
import Control.Monad                    (liftM,liftM2,join)
import Data.Eval                        (ExpObj(..),Eval)
import Data.Token                       (PairToken(..),IdToken(..),ExpToken(..))
import Eval.Function                    (Marshallable(..),applyFunc,types)
import Eval.MultiPass                   (mapPair)
import Parser.MonolithicParserTestUtils (ExpTA(..),ArrayTA(..),ObjTA(..),P(..),sizedArrayTA,sizedObjTA,toExpTA,toArrayTA,toObjTA,toStrTA,toNumTA,toBoolTA,toNullTA,unExpTA,unArrayTA,unObjTA,
                                         unStrTA,unNumTA,unBoolTA,unNullTA,sized1,sShrink,unP,toP,liftMF2,liftMF3,liftMF4,sizedListOf)
import Test.Framework                   (Arbitrary(..),elements)

class Is a where
  isFunc     :: a -> Bool
  isVar      :: a -> Bool

  isTable    :: a -> Bool
  isPlot     :: a -> Bool
  isArray    :: a -> Bool
  isObj      :: a -> Bool
  isStr      :: a -> Bool
  isNum      :: a -> Bool
  isBool     :: a -> Bool
  isNull     :: a -> Bool

instance Is ExpToken where
  isTable    = error "FunctionEvalTestUtils::isFunc<Obj> [Should not be called]"
  isPlot     = error "FunctionEvalTestUtils::isFunc<Obj> [Should not be called]"

  isFunc   (FuncT{})  = True; isFunc _  = False
  isVar    (VarT{})   = True; isVar _   = False
  isArray  (ArrayT{}) = True; isArray _ = False
  isObj    (ObjT{})   = True; isObj _   = False
  isStr    (StrT{})   = True; isStr _   = False
  isNum    (NumT{})   = True; isNum _   = False
  isBool   (BoolT{})  = True; isBool _  = False
  isNull   (NullT{})  = True; isNull _  = False

instance Is ExpObj where
  isFunc = error "FunctionEvalTestUtils::isFunc<Obj> [Should not be called]"
  isVar  = error "FunctionEvalTestUtils::isVar<Obj>  [Should not be called]"

  isTable  (TableO{})  = True; isTable _  = False
  isPlot   (PlotO{})   = True; isPlot _   = False
  isArray  (ArrayO{})  = True; isArray _  = False
  isObj    (ObjO{})    = True; isObj _    = False
  isStr    (StrO{})    = True; isStr _    = False
  isNum    (NumO{})    = True; isNum _    = False
  isBool   (BoolO{})   = True; isBool _   = False
  isNull   (NullO{})   = True; isNull _   = False

data TestFuncs = TF [ExpToken] deriving (Show)
instance Arbitrary TestFuncs where
  arbitrary                   = mTestFuncs (sizedArrayTS zero)    (sizedObjTS zero)    arbitrary            arbitrary             arbitrary             arbitrary
  shrink (TF [a,o,s,nb,b,nu]) = mTestFuncs (shrink $ toArrayTS a) (shrink $ toObjTS o) (shrink $ toStrTA s) (shrink $ toNumTA nb) (shrink $ toBoolTA b) (shrink $ toNullTA nu)
  shrink x                    = error $ "FunctionEvalTest::shrink<TestFuncs> [Pattern mismatch ["++show x++"]]"
mTestFuncs a o s n b n' = liftM TF $ sequence [liftM unArrayTS a, liftM unObjTS o, liftM unStrTA s, liftM unNumTA n, liftM unBoolTA b, liftM unNullTA n']
zero = 0 :: Integer

data TestFuncs' = TF' [ExpObj] deriving (Show)
instance Arbitrary TestFuncs' where
  arbitrary          = mTestFuncs' (sizedTableOA zero)    (sizedPlotOA zero)
  shrink (TF' [t,p]) = mTestFuncs' (shrink $ toTableOA t) (shrink $ toPlotOA p)
  shrink x           = error $ "FunctionEvalTest::shrink<TestFuncs'> [Pattern mismatch ["++show x++"]]"
mTestFuncs' t p = liftM TF' $ sequence [liftM unTableOA t, liftM unPlotOA p]

data ExpOA = ExpOA ExpObj deriving (Show)
instance Arbitrary ExpOA where
  arbitrary        = sized1 sizedExpOA
  shrink (ExpOA e) = mExpOA (shrinkExpO e)
sizedExpOA n = mExpOA $ join $ elements $ [arbStrO, arbNumO, arbBoolO, arbNullO] ++ if n > 0 then [arbTable n,arbPlot n, arbArrayO n, arbObjO n] else []
mExpOA = liftM ExpOA

toExpOA = ExpOA
unExpOA (ExpOA e) = e

arbTable    n = liftM unTableOA  $ sizedTableOA      $ n-1
arbPlot     n = liftM unPlotOA   $ sizedPlotOA       $ n-1
arbArrayO   n = liftM unArrayOA  $ sizedArrayOA      $ n-1
arbObjO     n = liftM unObjOA    $ sizedObjOA        $ n-1
arbStrO       = liftM unStrOA    arbitrary
arbNumO       = liftM unNumOA    arbitrary
arbBoolO      = liftM unBoolOA   arbitrary
arbNullO      = liftM unNullOA   arbitrary

shrinkExpO x@(TableO{})  = liftM unTableOA  $ sShrink $ toTableOA x
shrinkExpO x@(PlotO{})   = liftM unPlotOA   $ sShrink $ toPlotOA x
shrinkExpO x@(ArrayO{})  = liftM unArrayOA  $ sShrink $ toArrayOA x
shrinkExpO x@(ObjO{})    = liftM unObjOA    $ sShrink $ toObjOA x
shrinkExpO x@(StrO{})    = liftM unStrOA    $ sShrink $ toStrOA x
shrinkExpO x@(NumO{})    = liftM unNumOA    $ sShrink $ toNumOA x
shrinkExpO x@(BoolO{})   = liftM unBoolOA   $ sShrink $ toBoolOA x
shrinkExpO x@(NullO{})   = liftM unNullOA   $ sShrink $ toNullOA x

data TableOA = TableOA ExpObj deriving (Show)
instance Arbitrary TableOA where
  arbitrary                       = sized1 sizedTableOA
  shrink (TableOA (TableO p a o)) = mTableOA (sShrink $ toP p) (sShrink $ toArrayOA a)   (sShrink $ toObjOA o)
sizedTableOA n                    = mTableOA arbitrary         (sizedArrayOA n)          (sizedObjOA n)
mTableOA = liftMF3 mkTable unP unArrayOA unObjOA where mkTable x y = TableOA .TableO x y
toTableOA = TableOA
unTableOA (TableOA t) = t

data PlotOA = PlotOA ExpObj deriving (Show)
instance Arbitrary PlotOA where
  arbitrary                          = sized1 sizedPlotOA
  shrink (PlotOA (PlotO p xs ys ps)) = mPlotOA (sShrink $ toP p) (sShrink $ toArrayOA xs)   (sShrink $ toArrayOA ys)   (sShrink $ toObjOA ps)
sizedPlotOA n                        = mPlotOA arbitrary         (sizedArrayOA n)           (sizedArrayOA n)           (sizedObjOA n)
mPlotOA = liftMF4 mkPlot unP unArrayOA unArrayOA unObjOA where mkPlot x y z = PlotOA .PlotO x y z
toPlotOA = PlotOA
unPlotOA (PlotOA p) = p

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

data ExpTS = ExpTS ExpToken deriving (Show)
instance Arbitrary ExpTS where
  arbitrary        = mExpTS arbitrary
  shrink (ExpTS e) = mExpTS (shrink $ toExpTA e)
mExpTS = liftM (ExpTS .simplify.unExpTA)

data ArrayTS = ArrayTS ExpToken deriving (Show)
instance Arbitrary ArrayTS where
  arbitrary          = sized1 sizedArrayTS
  shrink (ArrayTS e) = mArrayTS (shrink $ toArrayTA e)
sizedArrayTS n       = mArrayTS (sizedArrayTA n)
mArrayTS = liftM (ArrayTS .simplify.unArrayTA)
toArrayTS = ArrayTS
unArrayTS (ArrayTS a) = a

data ObjTS = ObjTS ExpToken deriving (Show)
instance Arbitrary ObjTS where
  arbitrary        = sized1 sizedObjTS
  shrink (ObjTS e) = mObjTS (shrink $ toObjTA e)
sizedObjTS n       = mObjTS (sizedObjTA n)
mObjTS = liftM (ObjTS .simplify.unObjTA)
toObjTS = ObjTS
unObjTS (ObjTS o) = o

simplify (FuncT p _ _ _)      = NullT p w2
simplify (ArrayT p w es)      = ArrayT p w $ map simplify es
simplify (ObjT p w ps)        = ObjT p w $ map (mapPair simplify) ps
simplify (VarT _ (IdT p w _)) = NullT p w
simplify e                    = e

applyFunc' fs p n es = applyFunc fs (mkFunc p n es)
testFunc os fs p n = fromRight $ applyFunc' (mkFuncs os fs) p n []
fromRight (Right x) = x
fromRight x         = error $ "FunctionEvalTestUtils::fromRight [Failed pattern match ["++show x++"]]"

mkFunc p n = FuncT p w1 (IdT p w2 n)
forAll = flip all
w1 = ""
w2 = ("","")
p0 = (0 :: Int,0 :: Int)

funcNamesLit   = ["arrayTestF","objTestF","strTestF","numTestF","boolTestF","nullTestF"]
funcNamesNoLit = ["tableTestF","plotTestF"]
mkFuncs os es = zipWith f funcNamesNoLit os ++ zipWith g funcNamesLit es
  where f name e = (name,([],const $ return e))
        g name e = (name,([],testF' e))

data TokOrObj = Tok ExpToken | Obj ExpObj deriving (Show)
instance Marshallable TokOrObj where
  table    = error "Eval.FunctionEvalTestUtils::table<TOkOrObj>    [Should not be called]"
  plot     = error "Eval.FunctionEvalTestUtils::plot<TOkOrObj>     [Should not be called]"
  array    = error "Eval.FunctionEvalTestUtils::array<TOkOrObj>    [Should not be called]"
  obj      = error "Eval.FunctionEvalTestUtils::obj<TOkOrObj>      [Should not be called]"
  str      = error "Eval.FunctionEvalTestUtils::str<TOkOrObj>      [Should not be called]"
  num      = error "Eval.FunctionEvalTestUtils::num<TOkOrObj>      [Should not be called]"
  bool     = error "Eval.FunctionEvalTestUtils::bool<TOkOrObj>     [Should not be called]"
  null     = error "Eval.FunctionEvalTestUtils::null<TOkOrObj>     [Should not be called]"
  getT     = error "Eval.FunctionEvalTestUtils::getT<TOkOrObj>     [Should not be called]"
  
  getP (Tok x) = getP x
  getP (Obj x) = getP x

mkEntries n es os = removeEntry n $ zip3 (funcNamesNoLit++funcNamesLit) types (map Obj os++ map Tok es)
removeEntry n = filter $ \(m,_,_) -> n /= m

getSize (FuncT _ _ _ es)  = length es
getSize (ArrayT _ _ es)   = length es
getSize (ObjT _ _ ps)     = length ps

toTupleF (PairT _ (IdT _ _ x) y) = liftM2 (,) (return x) (testF y)

testF' e [] = testF e
testF :: ExpToken -> Eval ExpObj
testF (ArrayT p _ es) = liftM (ArrayO p) $ mapM testF es
testF (ObjT p _ ps)   = liftM (ObjO p)   $ mapM toTupleF ps
testF (StrT p _ s)    = return $ StrO p s
testF (NumT p _ _ n)  = return $ NumO p n
testF (BoolT p _ b)   = return $ BoolO p b
testF (NullT p _)     = return $ NullO p
testF e               = error $ "FunctionEvalTestUtils::testF [Failed pattern match ["++show e++"]]"

{-| Monomorphism restriction -}
mExpOA      :: (Applicative m, Monad m) => m ExpObj                                  -> m ExpOA
mTableOA    :: (Applicative m, Monad m) => m P -> m ArrayOA  -> m ObjOA              -> m TableOA
mPlotOA     :: (Applicative m, Monad m) => m P -> m ArrayOA  -> m ArrayOA -> m ObjOA -> m PlotOA
mArrayOA    :: (Applicative m, Monad m) => m P -> m [ExpOA]                          -> m ArrayOA
mObjOA      :: (Applicative m, Monad m) => m P -> m [(String,ExpOA)]                 -> m ObjOA
mStrOA      :: (Applicative m, Monad m) => m P -> m String                           -> m StrOA
mNumOA      :: (Applicative m, Monad m) => m P -> m Double                           -> m NumOA
mBoolOA     :: (Applicative m, Monad m) => m P -> m Bool                             -> m BoolOA
mNullOA     :: (Applicative m, Monad m) => m P                                       -> m NullOA

mExpTS      :: (Applicative m, Monad m) => m ExpTA   -> m ExpTS
mArrayTS    :: (Applicative m, Monad m) => m ArrayTA -> m ArrayTS
mObjTS      :: (Applicative m, Monad m) => m ObjTA   -> m ObjTS


