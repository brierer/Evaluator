{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Parser.ParserPropUtils where

import Control.Applicative
import Control.Monad
import Data.ExpToken
import Data.List
import Eval.Parser
import Numeric
import Test.Framework

import Parser.ParserUtils

class Unto a b where to :: b -> a; un :: a -> b
class Tall a   where tall :: Int -> Gen a

data ProgTA = ProgTA ProgToken deriving (Eq,Show)
instance Arbitrary ProgTA where arbitrary = sized1 tall; shrink (ProgTA (ProgT p fs)) = mProgTA (tShrink p) (tShrinks fs)
instance Tall      ProgTA where                                                tall n = mProgTA  arbitrary  (talls n)
mProgTA = liftMF2 mk un (map un) where mk x = ProgTA . ProgT x

data FormTA = FormTA FormToken deriving (Show)
instance Unto FormTA FormToken where to = FormTA; un (FormTA f) = f
instance Arbitrary FormTA where arbitrary = sized1 tall; shrink (FormTA (FormT i e)) = mFormTA (tShrink i) (tShrink e)
instance Tall      FormTA where                                               tall n = mFormTA arbitrary  (tall n)
mFormTA = liftMF2 mk un un where mk x = FormTA .FormT x

data PairTA = PairTA PairToken deriving (Show)
instance Unto PairTA PairToken where to = PairTA; un (PairTA p) = p
instance Arbitrary PairTA where arbitrary = sized1 tall; shrink (PairTA (PairT i v)) = mPairTA (tShrink i) (tShrink v)
instance Tall      PairTA where                                               tall n = mPairTA  arbitrary  (tall n)
mPairTA = liftMF2 mk un un where mk x = PairTA .PairT x

data IdTA =   IdTA IdToken deriving (Show)
instance Unto IdTA IdToken where to = IdTA; un (IdTA s) = s
instance Arbitrary IdTA where
  arbitrary                 = mIdTA  arbitrary   arbitrary  (liftM2 (:) (elements alpha) $ sListOf $ elements alphaDigit)
  shrink (IdTA (IdT p w s)) = mIdTA (tShrink p) (tShrink w) (sList $ filter validId $ sShrink s)
mIdTA = liftMF3 mk un un id where mk x y = IdTA .IdT x y
validId s =  all (`elem` alphaDigit) s && not (null s) && head s `elem` alpha
alpha = ['a'..'z']++['A'..'Z']
alphaDigit = alpha ++ ['0'..'9']

data ExpTA =  ExpTA ExpToken deriving (Show)
instance Unto ExpTA ExpToken where to = ExpTA; un (ExpTA e) = e
instance Arbitrary ExpTA where arbitrary = sized1 tall; shrink (ExpTA e) = mExpTA (shrinkT e)
instance Tall      ExpTA where                                    tall n = mExpTA (randomT n)
randomT n = join $ elements $ [mVarT, mStrT, mNumT, mBoolT, mNullT] ++ if n > 0 then [mFuncT n, mArrT n, mObjT n] else []
mExpTA = liftM ExpTA

mFuncT n = liftM un (tall (n-1) :: Gen FuncTA)
mArrT  n = liftM un (tall (n-1) :: Gen ArrTA)
mObjT  n = liftM un (tall (n-1) :: Gen ObjTA)
mVarT    = liftM un (arbitrary  :: Gen VarTA)
mStrT    = liftM un (arbitrary  :: Gen StrTA)
mNumT    = liftM un (arbitrary  :: Gen NumTA)
mBoolT   = liftM un (arbitrary  :: Gen BoolTA)
mNullT   = liftM un (arbitrary  :: Gen NullTA)

shrinkT x@(FuncT{}) = liftM un $ sShrink (to x :: FuncTA)
shrinkT x@(ArrT{})  = liftM un $ sShrink (to x :: ArrTA)
shrinkT x@(ObjT{})  = liftM un $ sShrink (to x :: ObjTA)
shrinkT x@(VarT{})  = liftM un $ sShrink (to x :: VarTA)
shrinkT x@(StrT{})  = liftM un $ sShrink (to x :: StrTA)
shrinkT x@(NumT{})  = liftM un $ sShrink (to x :: NumTA)
shrinkT x@(BoolT{}) = liftM un $ sShrink (to x :: BoolTA)
shrinkT x@(NullT{}) = liftM un $ sShrink (to x :: NullTA)

data FuncTA = FuncTA ExpToken deriving (Show)
instance Unto FuncTA ExpToken where to = FuncTA; un (FuncTA f) = f
instance Arbitrary FuncTA where arbitrary = sized1 tall; shrink (FuncTA (FuncT w i es)) = mFuncTA (tShrink w) (tShrink i) (tShrinks es)
instance Tall      FuncTA where                                                  tall n = mFuncTA  arbitrary   arbitrary  (talls n)
mFuncTA = liftMF3 mk un un (map un) where mk x y = FuncTA .FuncT x y

data ArrTA =  ArrTA ExpToken deriving (Show)
instance Unto ArrTA ExpToken where to = ArrTA; un (ArrTA a) = a
instance Arbitrary ArrTA where arbitrary = sized1 tall; shrink (ArrTA (ArrT p w es)) = mArrTA (tShrink p) (tShrink w) (tShrinks es)
instance Tall      ArrTA where                                                tall n = mArrTA  arbitrary   arbitrary  (talls n)
mArrTA = liftMF3 mk un un (map un) where mk x y = ArrTA .ArrT x y

data ObjTA =  ObjTA ExpToken deriving (Show)
instance Unto ObjTA ExpToken where to = ObjTA; un (ObjTA o) = o
instance Arbitrary ObjTA where arbitrary = sized1 tall; shrink (ObjTA (ObjT p w ps)) = mObjTA (sShrink $ to p) (sShrink $ to w) (tShrinks ps)
instance Tall      ObjTA where                                                tall n = mObjTA arbitrary        arbitrary        (talls n)
mObjTA = liftMF3 mk un un (map un) where mk x y = ObjTA .ObjT x y

data VarTA =  VarTA ExpToken deriving (Show)
instance Unto VarTA ExpToken where to = VarTA; un (VarTA v) = v
instance Arbitrary VarTA where
  arbitrary               = mVarTA  arbitrary
  shrink (VarTA (VarT i)) = mVarTA (tShrink i)
mVarTA = liftM (VarTA .VarT .un)

data StrTA =  StrTA ExpToken deriving (Show)
instance Unto StrTA ExpToken where to = StrTA; un (StrTA s) = s
instance Arbitrary StrTA where
  arbitrary                   = mStrTA  arbitrary   arbitrary  (sListOf $ elements valids)
  shrink (StrTA (StrT p w s)) = mStrTA (tShrink p) (tShrink w) (liftM (filter (`elem` valids)) $ sShrink s)
mStrTA = liftMF3 mk un un id where mk x y = StrTA .StrT x y
valids = [' '..'~'] \\ "\"\\"

data NumType = Int | Flt | Exp deriving (Eq,Show)
data NumTA = NumTA NumType ExpToken deriving (Show)
instance Unto NumTA ExpToken where
  un (NumTA _ n) = n
  to n@(NumT _ _ s v)
   | s == showAsInt (floor v) = NumTA Int n
   | s == showASFlt        v  = NumTA Flt n
   | s == showAsExp        v  = NumTA Exp n
   | otherwise              = error $ "Invalid num string ["++s++"]"

instance Arbitrary NumTA where
  arbitrary                       = mNumTA  arbitrary   arbitrary   arbitrary  (elements [Int,Flt,Exp])
  shrink (NumTA t (NumT p w _ v)) = mNumTA (tShrink p) (tShrink w) (sShrink v) [t]
mNumTA pa wa va ta = do
  p <- pa
  w <- wa
  t <- ta
  (v,vs) <- case t of
    Int -> do x <- fmap floor va; return (fromIntegral x, showAsInt x)
    Flt -> do x <- va;            return (             x, showASFlt x)
    Exp -> do x <- va;            return (             x, showAsExp x)
  return $ NumTA t $ NumT (un p) (un w) vs v

showAsInt x = show (x :: Integer)
showASFlt x = show (x :: Double)
showAsExp x = showEFloat Nothing x ""

data BoolTA = BoolTA ExpToken deriving (Show)
instance Unto BoolTA ExpToken where to = BoolTA; un (BoolTA b) = b
instance Arbitrary BoolTA where
  arbitrary                     = mBoolTA  arbitrary   arbitrary   arbitrary
  shrink (BoolTA (BoolT p w v)) = mBoolTA (tShrink p) (tShrink w) (sShrink v)
mBoolTA = liftMF3 mk un un id where mk x y = BoolTA .BoolT x y

data NullTA = NullTA ExpToken deriving (Show)
instance Unto NullTA ExpToken where to = NullTA; un (NullTA n) = n
instance Arbitrary NullTA where
  arbitrary                   = mNullTA  arbitrary   arbitrary
  shrink (NullTA (NullT p w)) = mNullTA (tShrink p) (tShrink w)
mNullTA = liftMF2 mk un un where mk x = NullTA .NullT x

data P = P (Int,Int) deriving (Show)
instance Unto P (Int,Int) where to = P; un (P p) = p
instance Arbitrary P where
  arbitrary        = mP  arbitrary   arbitrary
  shrink (P (l,c)) = mP (tShrink l) (tShrink c)
mP = liftMF2 mkP un un where mkP x = P.(,) x

data ValidInt = ValidInt Int deriving (Show)
instance Unto   ValidInt Int where to = ValidInt; un (ValidInt n) = n
instance Arbitrary ValidInt where
  arbitrary           = mValidInt (choose validInts)
  shrink (ValidInt i) = mValidInt (shrinkValidInts i)
mValidInt = liftM ValidInt
validInts = (0,1000)
shrinkValidInts 0 = []
shrinkValidInts x = [x `div` 2, x - 1]

data W = W String deriving (Show)
instance Unto W String where to  = W; un (W a) = a
instance Unto (W,W) (String,String) where to (a,b) = (W a, W b); un (W a, W b) = (a,b)
instance Arbitrary W where
  arbitrary    = mW $ sListOf $ elements " \t\n\v"
  shrink (W w) = mW $ filter (`elem` " \t\n\v") <$> sShrink w
mW = liftM W

class    NoPos a         where noPos :: a -> a
instance NoPos ProgToken where noPos (ProgT _ fs) = ProgT p0 $ map noPos fs
instance NoPos FormToken where noPos (FormT i e)  = FormT (noPos i) (noPos e)
instance NoPos PairToken where noPos (PairT i e)  = PairT (noPos i) (noPos e)
instance NoPos IdToken   where noPos (IdT _ w i)  = IdT   p0 w i
instance NoPos ExpToken  where
  noPos(FuncT   w  i es) = FuncT    w (noPos i) $ map noPos es
  noPos(ArrT  _ w    es) = ArrT  p0 w           $ map noPos es
  noPos(ObjT  _ w    ps) = ObjT  p0 w           $ map noPos ps
  noPos(VarT       i   ) = VarT       (noPos i)
  noPos(StrT  _ w   v  ) = StrT  p0 w   v
  noPos(NumT  _ w s v  ) = NumT  p0 w s v
  noPos(BoolT _ w   v  ) = BoolT p0 w   v
  noPos(NullT _ w      ) = NullT p0 w

testCase p = unsafeParse p.unparse
a .= b = noPos a == noPos b

liftMF2 g f1 f2       x1 x2        = g <$> liftM f1 x1 <*> liftM f2 x2
liftMF3 g f1 f2 f3    x1 x2 x3     = g <$> liftM f1 x1 <*> liftM f2 x2 <*> liftM f3 x3
liftMF4 g f1 f2 f3 f4 x1 x2 x3 x4  = g <$> liftM f1 x1 <*> liftM f2 x2 <*> liftM f3 x3 <*> liftM f4 x4

talls n = sListOf $ tall n

sized1 p = sized f where f i = p $ i `mod` 3
sListOf  = liftM sList . listOf
sList1Of = liftM sList . listOf1
sList = take 10

sShrink  = take 1.shrink
tShrink  = sShrink.to
tShrinks = sShrink.map to

{-| Mandatory type signatures -}
mProgTA :: (Applicative m, Monad m) => m P               -> m [FormTA]              -> m ProgTA
mFormTA :: (Applicative m, Monad m) =>                      m IdTA     -> m ExpTA   -> m FormTA
mPairTA :: (Applicative m, Monad m) =>                      m IdTA     -> m ExpTA   -> m PairTA
mIdTA   :: (Applicative m, Monad m) => m P -> m (W,W)    -> m String                -> m IdTA
mExpTA  :: (Applicative m, Monad m) =>                      m ExpToken              -> m ExpTA
mFuncTA :: (Applicative m, Monad m) =>        m  W       -> m IdTA     -> m [ExpTA] -> m FuncTA
mArrTA  :: (Applicative m, Monad m) => m P -> m (W,W)    -> m [ExpTA]               -> m ArrTA
mObjTA  :: (Applicative m, Monad m) => m P -> m (W,W)    -> m [PairTA]              -> m ObjTA
mVarTA  :: (Applicative m, Monad m) =>                      m IdTA                  -> m VarTA
mStrTA  :: (Applicative m, Monad m) => m P -> m (W,W)    -> m String                -> m StrTA
mNumTA  :: (Applicative m, Monad m) => m P -> m (W,W)    -> m Double   -> m NumType -> m NumTA
mBoolTA :: (Applicative m, Monad m) => m P -> m (W,W)    -> m Bool                  -> m BoolTA
mNullTA :: (Applicative m, Monad m) => m P -> m (W,W)                               -> m NullTA

mValidInt :: (Applicative m, Monad m) => m Int                    -> m ValidInt 
mP        :: (Applicative m, Monad m) => m ValidInt -> m ValidInt -> m P
mW        :: (Applicative m, Monad m) => m String                 -> m W

sShrink   :: Arbitrary a => a -> [a]
tShrink   :: (Arbitrary b,Unto b a) => a -> [b]
tShrinks :: (Arbitrary b,Unto b a) => [a] -> [[b]]