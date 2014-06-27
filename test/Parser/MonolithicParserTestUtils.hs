{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns  #-}
module Parser.MonolithicParserTestUtils where

import Data.List                       ((\\))
import Data.Token                      (ProgToken(..),FormToken(..),PairToken(..),IdToken(..),ExpToken(..))
import Control.Applicative             (Applicative,(<$>),(<*>))
import Control.Monad                   (liftM,liftM2,join,replicateM)
import Numeric                         (showEFloat)
import Parser.Monolithic               (unparse)
import Test.Framework                  (Arbitrary,Gen,Positive(..),arbitrary,shrink,elements,sized,listOf,choose)
import Text.ParserCombinators.Parsec   (parse)

class Unto a b where to :: b -> a; un :: a -> b
class Tall a   where tall :: Int -> Gen a

data ProgTA = ProgTA ProgToken deriving (Eq,Show)
instance Arbitrary ProgTA where arbitrary = sized1 tall; shrink (ProgTA (ProgT p fs)) = mProgTA (tShrink p) (tShrinks fs)
instance Tall      ProgTA where                                                tall n =  mProgTA  arbitrary  (sizes n) 
mProgTA = liftMF2 mkProg un uns where mkProg x = ProgTA . ProgT x

data FormTA = FormTA FormToken deriving (Show)
instance Unto FormTA FormToken where to = FormTA; un (FormTA f) = f
instance Arbitrary FormTA where arbitrary = sized1 tall; shrink (FormTA (FormT p i e)) = mFormTA (tShrink p) (tShrink i) (tShrink e)
instance Tall      FormTA where                                                 tall n =  mFormTA  arbitrary   arbitrary  (tall n) 
mFormTA = liftMF3 mkForm un un un where mkForm x y = FormTA .FormT x y

data PairTA = PairTA PairToken deriving (Show)
instance Unto PairTA PairToken where to = PairTA; un (PairTA p) = p
instance Arbitrary PairTA where arbitrary = sized1 tall; shrink (PairTA (PairT p i v)) = mPairTA (tShrink p) (tShrink i) (tShrink v)
instance Tall      PairTA where                                                 tall n =  mPairTA  arbitrary   arbitrary  (tall n) 
mPairTA = liftMF3 mkPair un un un where mkPair x y = PairTA .PairT x y

data IdTA =   IdTA IdToken deriving (Show)
instance Unto IdTA IdToken where to = IdTA; un (IdTA s) = s
instance Arbitrary IdTA where
  arbitrary                 = mIdTA  arbitrary   arbitrary  (liftM2 (:) (elements alpha) $ sListOf $ elements alphaDigit)
  shrink (IdTA (IdT p w s)) = mIdTA (tShrink p) (tShrink w) (sList $ filter validId $ sShrink s)
mIdTA = liftMF3 mkId un un id where mkId x y = IdTA .IdT x y 
validId s =  all (`elem` alphaDigit) s && not (null s) && head s `elem` alpha
alpha = ['a'..'z']++['A'..'Z']
alphaDigit = alpha ++ ['0'..'9']

data ExpTA =  ExpTA ExpToken deriving (Show)
instance Unto ExpTA ExpToken where to = ExpTA; un (ExpTA e) = e
instance Arbitrary ExpTA where arbitrary = sized1 tall; shrink (ExpTA e) = mExpTA (shrinkT e)
instance Tall      ExpTA where                                    tall n =  mExpTA (randomT n)
randomT n = join $ elements $ [mVarT, mStrT, mNumT, mBoolT, mNullT] ++ if n > 0 then [mFuncT n, mArrayT n, mObjT n] else []
mExpTA = liftM ExpTA

mFuncT  n = liftM un (tall (n-1) :: Gen FuncTA)
mArrayT n = liftM un (tall (n-1) :: Gen ArrayTA)
mObjT   n = liftM un (tall (n-1) :: Gen ObjTA)
mVarT     = liftM un (arbitrary  :: Gen VarTA)
mStrT     = liftM un (arbitrary  :: Gen StrTA)
mNumT     = liftM un (arbitrary  :: Gen NumTA)
mBoolT    = liftM un (arbitrary  :: Gen BoolTA)
mNullT    = liftM un (arbitrary  :: Gen NullTA)

shrinkT x@(FuncT{})  = liftM un $ sShrink (to x :: FuncTA) 
shrinkT x@(ArrayT{}) = liftM un $ sShrink (to x :: ArrayTA)
shrinkT x@(ObjT{})   = liftM un $ sShrink (to x :: ObjTA)  
shrinkT x@(VarT{})   = liftM un $ sShrink (to x :: VarTA)  
shrinkT x@(StrT{})   = liftM un $ sShrink (to x :: StrTA)  
shrinkT x@(NumT{})   = liftM un $ sShrink (to x :: NumTA)  
shrinkT x@(BoolT{})  = liftM un $ sShrink (to x :: BoolTA) 
shrinkT x@(NullT{})  = liftM un $ sShrink (to x :: NullTA) 

data FuncTA = FuncTA ExpToken deriving (Show)
instance Unto FuncTA ExpToken where to = FuncTA; un (FuncTA f) = f
instance Arbitrary FuncTA where arbitrary = sized1 tall; shrink (FuncTA (FuncT p w i es)) = mFuncTA (tShrink p) (tShrink w) (tShrink i) (tShrinks es)
instance Tall      FuncTA where                                                    tall n =  mFuncTA  arbitrary   arbitrary   arbitrary  (sizes n)
mFuncTA = liftMF4 mkFunc un un un uns where mkFunc x y z = FuncTA .FuncT x y z

data ArrayTA = ArrayTA ExpToken deriving (Show)
instance Unto  ArrayTA ExpToken where to = ArrayTA; un (ArrayTA a) = a
instance Arbitrary ArrayTA where arbitrary = sized1 tall; shrink (ArrayTA (ArrayT p w es)) = mArrayTA (tShrink p) (tShrink w) (tShrinks es)
instance Tall      ArrayTA where                                                    tall n =  mArrayTA  arbitrary   arbitrary  (sizes n)
mArrayTA = liftMF3 mkArray un un uns where mkArray x y = ArrayTA .ArrayT x y

data ObjTA =  ObjTA ExpToken deriving (Show)
instance Unto ObjTA ExpToken where to = ObjTA; un (ObjTA o) = o
instance Arbitrary ObjTA where arbitrary = sized1 tall; shrink (ObjTA (ObjT p w ps)) = mObjTA (sShrink $ to p) (sShrink $ to w) (tShrinks ps)
instance Tall      ObjTA where                                                tall n =  mObjTA arbitrary        arbitrary        (sizes n)
mObjTA = liftMF3 mkObj un un uns where mkObj x y = ObjTA .ObjT x y

data VarTA =  VarTA ExpToken deriving (Show)
instance Unto VarTA ExpToken where to = VarTA; un (VarTA v) = v
instance Arbitrary VarTA where
  arbitrary                 = mVarTA  arbitrary   arbitrary
  shrink (VarTA (VarT p i)) = mVarTA (tShrink p) (tShrink i)
mVarTA = liftMF2 mkVar un un where mkVar x = VarTA .VarT x

data StrTA =  StrTA ExpToken deriving (Show)
instance Unto StrTA ExpToken where to = StrTA; un (StrTA s) = s
instance Arbitrary StrTA where
  arbitrary                   = mStrTA  arbitrary   arbitrary  (sListOf $ elements valids)
  shrink (StrTA (StrT p w s)) = mStrTA (tShrink p) (tShrink w) (liftM (filter (`elem` valids)) $ sShrink s)
mStrTA = liftMF3 mkStr un un id where mkStr x y = StrTA .StrT x y
valids = [' '..'~'] \\ "\"\\"

data NumType = Int | Flt | Exp deriving (Eq,Show)
data NumTA = NumTA NumType ExpToken deriving (Show)
instance Unto NumTA ExpToken where
  un (NumTA _ n) = n
  to n@(NumT _ _ s v) 
   | s == showInt (floor v) = NumTA Int n
   | s == showFlt        v  = NumTA Flt n
   | s == showExp        v  = NumTA Exp n
   | otherwise              = error $ "Invalid num string ["++s++"]"
  
instance Arbitrary NumTA where
  arbitrary                       = mNumTA  arbitrary   arbitrary   arbitrary  (elements [Int,Flt,Exp])
  shrink (NumTA t (NumT p w _ v)) = mNumTA (tShrink p) (tShrink w) (sShrink v) [t]
mNumTA pa wa va ta = do
  p <- pa
  w <- wa
  t <- ta
  (v,vs) <- case t of
    Int -> do x <- fmap floor va; return (fromIntegral x, showInt x)
    Flt -> do x <- va;            return (             x, showFlt x)
    Exp -> do x <- va;            return (             x, showExp x)
  return $ NumTA t $ NumT (un p) (un w) vs v

showExp x = showEFloat Nothing x ""
showFlt x = show (x :: Double)
showInt x = show (x :: Integer)

data BoolTA = BoolTA ExpToken deriving (Show)
instance Unto BoolTA ExpToken where to = BoolTA; un (BoolTA b) = b
instance Arbitrary BoolTA where
  arbitrary                     = mBoolTA  arbitrary   arbitrary   arbitrary
  shrink (BoolTA (BoolT p w v)) = mBoolTA (tShrink p) (tShrink w) (sShrink v)
mBoolTA = liftMF3 mkBool un un id where mkBool x y = BoolTA .BoolT x y

data NullTA = NullTA ExpToken deriving (Show)
instance Unto NullTA ExpToken where to = NullTA; un (NullTA n) = n
instance Arbitrary NullTA where
  arbitrary                   = mNullTA  arbitrary   arbitrary
  shrink (NullTA (NullT p w)) = mNullTA (tShrink p) (tShrink w)
mNullTA = liftMF2 mkNull un un where mkNull x = NullTA .NullT x

data P = P (Int,Int) deriving (Show)
instance Unto P (Int,Int) where to = P; un (P p) = p
instance Arbitrary P where
  arbitrary        = mP (choose validP)  (choose validP)
  shrink (P (l,c)) = mP (shrinkValidP l) (shrinkValidP c)
mP = liftMF2 mkP id id where mkP x = P.(,) x 
validP = (1,1000)
shrinkValidP 0 = []
shrinkValidP x = [x `div` 2, x -1]

data W = W String deriving (Show)
instance Unto W String where to  = W; un (W a) = a
instance Unto (W,W) (String,String) where to (a,b) = (W a, W b); un (W a, W b) = (a,b)
instance Arbitrary W where
  arbitrary    = mW $ sListOf $ elements " \t\n\v"
  shrink (W w) = mW $ filter (`elem` " \t\n\v") <$> sShrink w
mW = liftM W

testCase p = (\(Right x) -> x) . parse p "" . unparse

liftMF2 g f1 f2       x1 x2        = g <$> liftM f1 x1 <*> liftM f2 x2
liftMF3 g f1 f2 f3    x1 x2 x3     = g <$> liftM f1 x1 <*> liftM f2 x2 <*> liftM f3 x3
liftMF4 g f1 f2 f3 f4 x1 x2 x3 x4  = g <$> liftM f1 x1 <*> liftM f2 x2 <*> liftM f3 x3 <*> liftM f4 x4

sizes p = do Positive m <- arbitrary; ps <- replicateM m $ tall p; return $ sList ps

sized1 p = sized f where f i = p $ i `mod` 3
sListOf = liftM sList . listOf
sList = take 10

sShrink :: Arbitrary a => a -> [a]
sShrink = take 1.shrink

tShrink :: (Arbitrary b, Unto b a) => a -> [b]
tShrink = sShrink.to

tShrinks :: (Arbitrary b,Unto b a) => [a] -> [[b]]
tShrinks = sShrink.map to

uns :: Unto a b => [a] -> [b]
uns = map un

mProgTA  :: (Applicative m, Monad m) => m P               -> m [FormTA]              -> m ProgTA
mFormTA  :: (Applicative m, Monad m) => m P               -> m IdTA     -> m ExpTA   -> m FormTA
mPairTA  :: (Applicative m, Monad m) => m P               -> m IdTA     -> m ExpTA   -> m PairTA
mIdTA    :: (Applicative m, Monad m) => m P -> m (W,W)    -> m String                -> m IdTA
mExpTA   :: (Applicative m, Monad m) =>                      m ExpToken              -> m ExpTA
mFuncTA  :: (Applicative m, Monad m) => m P -> m  W       -> m IdTA     -> m [ExpTA] -> m FuncTA
mArrayTA :: (Applicative m, Monad m) => m P -> m (W,W)    -> m [ExpTA]               -> m ArrayTA
mObjTA   :: (Applicative m, Monad m) => m P -> m (W,W)    -> m [PairTA]              -> m ObjTA
mVarTA   :: (Applicative m, Monad m) => m P ->               m IdTA                  -> m VarTA
mStrTA   :: (Applicative m, Monad m) => m P -> m (W,W)    -> m String                -> m StrTA
mNumTA   :: (Applicative m, Monad m) => m P -> m (W,W)    -> m Double   -> m NumType -> m NumTA
mBoolTA  :: (Applicative m, Monad m) => m P -> m (W,W)    -> m Bool                  -> m BoolTA
mNullTA  :: (Applicative m, Monad m) => m P -> m (W,W)                               -> m NullTA

mP       :: (Applicative m, Monad m) => m Int      -> m Int -> m P
mW       :: (Applicative m, Monad m) => m String            -> m W

