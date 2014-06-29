{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Eval.FunctionEvalTestUtils where

import Prelude hiding                   (any,null)

import Control.Arrow                    (second)
import Control.Applicative              (Applicative)
import Control.Monad                    (liftM,liftM2,join)
import Data.Eval                        (EvalError(..),ExpObj(..),Type(..))
import Data.List                        (permutations,(\\))
import Data.Token                       (PairToken(..),IdToken(..),ExpToken(..))
import Eval.Function                    (Marshallable(..),any,lit,applyFunc,(<|>),(<!>))
import Eval.MultiPass                   (mapPair)
import Parser.MonolithicParserTestUtils (Unto(..),Tall(..),IdTA(..),ExpTA(..),ArrayTA(..),ObjTA(..),StrTA(..),NumTA(..),BoolTA(..),NullTA(..),P(..),
                                         sShrink,tShrink,tShrinks,sizes,sized1,liftMF2,liftMF3,liftMF4)
import Test.Framework                   (Arbitrary(..),Gen,elements,choose)

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
  isTable    = error "FunctionEvalTestUtils::isTable<ExpToken> [Should not be called]"
  isPlot     = error "FunctionEvalTestUtils::isPlot<ExpToken> [Should not be called]"

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

data ExpOA = ExpOA ExpObj deriving (Show)
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
instance Arbitrary TableOA where arbitrary = sized1 tall; shrink (TableOA (TableO p a o)) = mTableOA (tShrink p) (tShrink a) (tShrink o)
instance Tall TableOA      where                                                   tall n =  mTableOA  arbitrary  (tall n)    (tall n)
mTableOA = liftMF3 mkTable un un un where mkTable x y = TableOA .TableO x y

data PlotOA = PlotOA ExpObj deriving (Show)
instance Unto PlotOA ExpObj where to = PlotOA; un (PlotOA p) = p
instance Arbitrary PlotOA where arbitrary = sized1 tall; shrink (PlotOA (PlotO p xs ys ps)) = mPlotOA (tShrink p) (tShrink xs)   (tShrink ys)   (tShrink ps)
instance Tall      PlotOA where                                                      tall n =  mPlotOA  arbitrary  (tall n)       (tall n)       (tall n)
mPlotOA = liftMF4 mkPlot un un un un where mkPlot x y z = PlotOA .PlotO x y z

data ArrayOA = ArrayOA ExpObj deriving (Show)
instance Unto  ArrayOA ExpObj where to = ArrayOA; un (ArrayOA a) = a
instance Arbitrary ArrayOA where arbitrary = sized1 tall; shrink (ArrayOA (ArrayO p es)) = mArrayOA (tShrink p) (tShrinks es)
instance Tall      ArrayOA where                                                  tall n =  mArrayOA  arbitrary  (sizes n)
mArrayOA = liftMF2 mkArray un (map un) where mkArray x = ArrayOA .ArrayO x

data ObjOA =  ObjOA ExpObj deriving (Show)
instance Unto ObjOA ExpObj where to = ObjOA; un (ObjOA o) = o
instance Arbitrary ObjOA where arbitrary = sized1 tall; shrink (ObjOA (ObjO p ps)) = mObjOA (tShrink p) (tShrinks ps)
instance Tall      ObjOA where                                              tall n =  mObjOA  arbitrary  (sizes n)
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

data ExpTS =  ExpTS ExpToken deriving (Show)
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

simplify (FuncT p _ _ _)      = NullT  p w2
simplify (ArrayT p w es)      = ArrayT p w $ map  simplify          es
simplify (ObjT p w ps)        = ObjT   p w $ map (mapPair simplify) ps
simplify (VarT _ (IdT p w _)) = NullT  p w
simplify e                    = e

data ExpTF =  ExpTF ExpToken ExpObj IdToken deriving (Show)
instance Arbitrary ExpTF where arbitrary = sized1 tall; shrink (ExpTF e o i) = mExpTF (tShrink e) (tShrink o) (tShrink i) 
instance Tall      ExpTF where                                        tall n = mExpTF (tall n)    (tall n)     arbitrary
mExpTF = moo ExpTF 

data ArrayTF =  ArrayTF ExpToken ExpObj IdToken deriving (Show)
instance Arbitrary ArrayTF where arbitrary = sized1 tall; shrink (ArrayTF e o i) = mArrayTF (tShrink e) (tShrink o) (tShrink i) 
instance Tall      ArrayTF where                                          tall n = mArrayTF (tall n)    (tall n)     arbitrary
mArrayTF = moo ArrayTF 

data ObjTF =  ObjTF ExpToken ExpObj IdToken deriving (Show)
instance Arbitrary ObjTF where arbitrary = sized1 tall; shrink (ObjTF e o i) = mObjTF (tShrink e) (tShrink o) (tShrink i) 
instance Tall      ObjTF where                                        tall n = mObjTF (tall n)    (tall n)     arbitrary
mObjTF = moo ObjTF

moo f ea oa ia = do e <- ea; o <- oa; i <- ia; return $ f (simplifyF (un i) $ un e) (un o) (un i)

simplifyF i (FuncT p w _ _)      = FuncT  p w i [] 
simplifyF i (ArrayT p w es)      = ArrayT p w $ map  (simplifyF i)          es
simplifyF i (ObjT p w ps)        = ObjT   p w $ map (mapPair $ simplifyF i) ps
simplifyF _ (VarT _ (IdT p w _)) = NullT  p w
simplifyF _ e                    = e

data TokOrObj = Tok ExpToken | Obj ExpObj deriving (Eq,Show)
instance Marshallable TokOrObj where
  table    = error "Eval.FunctionEvalTestUtils::table<TOkOrObj>    [Should not be called]"
  plot     = error "Eval.FunctionEvalTestUtils::plot<TOkOrObj>     [Should not be called]"
  funCall  = error "Eval.FunctionEvalTestUtils::plot<TOkOrObj>     [Should not be called]"
  array    = error "Eval.FunctionEvalTestUtils::funCall<TOkOrObj>  [Should not be called]"
  obj      = error "Eval.FunctionEvalTestUtils::obj<TOkOrObj>      [Should not be called]"
  str      = error "Eval.FunctionEvalTestUtils::str<TOkOrObj>      [Should not be called]"
  num      = error "Eval.FunctionEvalTestUtils::num<TOkOrObj>      [Should not be called]"
  bool     = error "Eval.FunctionEvalTestUtils::bool<TOkOrObj>     [Should not be called]"
  null     = error "Eval.FunctionEvalTestUtils::null<TOkOrObj>     [Should not be called]"
  getT     = error "Eval.FunctionEvalTestUtils::getT<TOkOrObj>     [Should not be called]"

  getP (Tok x) = getP x; getP (Obj x) = getP x

data TestIndexesT = TestIndexesT [Int] [Int] deriving (Show)
data TestIndexesO = TestIndexesO [Int] [Int] deriving (Show)
instance Arbitrary TestIndexesT where arbitrary = arbIndexes (permutations [0..5]) TestIndexesT; shrink (TestIndexesT is rest) = shrinkIndexes is rest TestIndexesT
instance Arbitrary TestIndexesO where arbitrary = arbIndexes (permutations [0..7]) TestIndexesO; shrink (TestIndexesO is rest) = shrinkIndexes is rest TestIndexesO
  
arbIndexes is f = do
    i <- choose (0,length is-1)
    j <- choose (2,4)
    let (indexes,rest) = splitAt j (is !! i)
    return $ f indexes rest
    
shrinkIndexes is rest f | length is <= 2 = [] | otherwise = [f (is \\ [i]) (i:rest) | i <- is]

orCase es vs indexes rest f = 
  let ps = zip es vs
      toOr   = map (ps!!) indexes
      wrongs = map (fst.(ps!!)) rest
      validator    = foldl1 (<|>) (map snd toOr) <!> expectedType
      expectedType = foldl1  Or   (map (getT.fst) toOr)  
  in  forAll toOr   (\(e,v) -> f e == v e) &&
      forAll wrongs (\e -> Left (TypeMismatch (getP e) expectedType (getT e)) == validator e)

applyFunc' fs p n es = applyFunc fs (mkFunc p n es)
testFunc os fs p n = fromRight $ applyFunc' (mkFuncs os fs) p n []
fromRight (Right x) = x
fromRight x         = error $ "FunctionEvalTestUtils::fromRight [Failed pattern match ["++show x++"]]"

mkFunc p n = FuncT p w1 (IdT p w2 n)
forAll = flip all
w1 = ""
w2 = ("","")
p0 = (0 :: Int,0 :: Int)
constM = const.return

funcNamesLit   = ["arrayTestF","objTestF","strTestF","numTestF","boolTestF","nullTestF"]
funcNamesNoLit = ["tableTestF","plotTestF"]
mkFuncs os es = fs
  where fs = zipWith f funcNamesNoLit os ++ zipWith g funcNamesLit es
        f name e = (name,([],constM e))
        g name e = (name,([],const $ testF fs e))

mkEntries ns es os = foldr removeEntry (zip3 (funcNamesNoLit++funcNamesLit) types (map Obj os++ map Tok es)) ns
types = [Table,Plot,Array,Object,String,Number,Boolean,Null]
removeEntry n = filter $ \(m,_,_) -> n /= m

getTall (FuncT _ _ _ es)  = length es
getTall (ArrayT _ _ es)   = length es
getTall (ObjT _ _ ps)     = length ps

anyCase os es (name,_,Obj e) = Right e == any [] (testFunc os es (getP e) name)
anyCase os es (name,_,Tok e) = testS e == any [] (testFunc os es (getP e) name)

litCase os es (name,_,Obj e) = Right e == lit [] (testFunc os es (getP e) name)
litCase os es (name,_,Tok e) = testS e == lit [] (testFunc os es (getP e) name)

toTupleF fs (PairT _ (IdT _ _ x) y) = liftM2 (,) (return x) (testF fs y)

testS = testF []
testF fs f@(FuncT{})     = applyFunc fs f
testF fs (ArrayT p _ es) = liftM (ArrayO p) $ mapM (testF fs) es
testF fs (ObjT p _ ps)   = liftM (ObjO p)   $ mapM (toTupleF fs) ps
testF _  (StrT p _ s)    = return $ StrO p s
testF _  (NumT p _ _ n)  = return $ NumO p n
testF _  (BoolT p _ b)   = return $ BoolO p b
testF _  (NullT p _)     = return $ NullO p
testF _  e               = error $ "FunctionEvalTestUtils::testF [Failed pattern match ["++show e++"]]"

{-| Monomorphism restriction -}
mTestToks :: (Applicative m, Monad m) => m ArrayTS -> m ObjTS -> m StrTA -> m NumTA -> m BoolTA -> m NullTA -> m TestToks
mTestObjs :: (Applicative m, Monad m) => m TableOA -> m PlotOA                                              -> m TestObjs

mExpOA    :: (Applicative m, Monad m) => m ExpObj                                  -> m ExpOA
mTableOA  :: (Applicative m, Monad m) => m P -> m ArrayOA  -> m ObjOA              -> m TableOA
mPlotOA   :: (Applicative m, Monad m) => m P -> m ArrayOA  -> m ArrayOA -> m ObjOA -> m PlotOA
mArrayOA  :: (Applicative m, Monad m) => m P -> m [ExpOA]                          -> m ArrayOA
mObjOA    :: (Applicative m, Monad m) => m P -> m [(String,ExpOA)]                 -> m ObjOA
mStrOA    :: (Applicative m, Monad m) => m P -> m String                           -> m StrOA
mNumOA    :: (Applicative m, Monad m) => m P -> m Double                           -> m NumOA
mBoolOA   :: (Applicative m, Monad m) => m P -> m Bool                             -> m BoolOA
mNullOA   :: (Applicative m, Monad m) => m P                                       -> m NullOA

mExpTS    :: (Applicative m, Monad m) => m ExpTA   -> m ExpTS
mArrayTS  :: (Applicative m, Monad m) => m ArrayTA -> m ArrayTS
mObjTS    :: (Applicative m, Monad m) => m ObjTA   -> m ObjTS

mExpTF    :: (Applicative m, Monad m) => m ExpTA   -> m ExpOA -> m IdTA -> m ExpTF
mArrayTF  :: (Applicative m, Monad m) => m ArrayTA -> m ExpOA -> m IdTA -> m ArrayTF
mObjTF    :: (Applicative m, Monad m) => m ObjTA   -> m ExpOA -> m IdTA -> m ObjTF


