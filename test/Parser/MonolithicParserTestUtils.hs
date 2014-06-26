{-# OPTIONS_GHC -fno-warn-incomplete-patterns  #-}
module Parser.MonolithicParserTestUtils where

import Data.List                       ((\\))
import Data.Token                      (ProgToken(..),FormToken(..),PairToken(..),IdToken(..),ExpToken(..))
import Control.Applicative             (Applicative,(<$>),(<*>))
import Control.Monad                   (liftM,join,replicateM)
import Numeric                         (showEFloat)
import Parser.Monolithic               (unparse)
import Test.Framework                  (Arbitrary,Positive(..),arbitrary,shrink,elements,sized,listOf,choose)
import Text.ParserCombinators.Parsec   (parse)

testCase p = (\(Right x) -> x) . parse p "" . unparse

data ProgTA = ProgTA ProgToken deriving (Eq,Show)
instance Arbitrary ProgTA where
  arbitrary                    = sized1 sizedProgTA
  shrink (ProgTA (ProgT p fs)) = mProgTA (sShrink $ toP p) (sShrink $ map toFormTA fs)
sizedProgTA n                  = mProgTA arbitrary         (sizedListOf $ sizedFormTA n)
mProgTA = liftMF2 mkProg unP  (map unFormTA) where mkProg x = ProgTA . ProgT x
toProgTA = ProgTA
unProgTA (ProgTA p) = p

liftMF2 g f1 f2       x1 x2        = g <$> liftM f1 x1 <*> liftM f2 x2
liftMF3 g f1 f2 f3    x1 x2 x3     = g <$> liftM f1 x1 <*> liftM f2 x2 <*> liftM f3 x3
liftMF4 g f1 f2 f3 f4 x1 x2 x3 x4  = g <$> liftM f1 x1 <*> liftM f2 x2 <*> liftM f3 x3 <*> liftM f4 x4

data FormTA = FormTA FormToken deriving (Show)
instance Arbitrary FormTA where
  arbitrary                     = sized1 sizedFormTA
  shrink (FormTA (FormT p i e)) = mFormTA (sShrink $ toP p) (sShrink $ toIdTA i) (sShrink $ toExpTA e)
sizedFormTA n                   = mFormTA arbitrary         arbitrary            (sizedExpTA n)
mFormTA = liftMF3 mkForm unP unIdTA unExpTA where mkForm x y = FormTA .FormT x y
toFormTA = FormTA
unFormTA (FormTA f) = f

data PairTA = PairTA PairToken deriving (Show)
instance Arbitrary PairTA where
  arbitrary                     = sized1 sizedPairTA
  shrink (PairTA (PairT p i v)) = mPairTA (sShrink $ toP p) (sShrink $ toIdTA i) (sShrink $ toExpTA v)
sizedPairTA n                   = mPairTA arbitrary         arbitrary            (sizedExpTA n)
mPairTA = liftMF3 mkPair unP unIdTA unExpTA where mkPair x y = PairTA .PairT x y
toPairTA = PairTA
unPairTA (PairTA p) = p

data IdTA = IdTA IdToken deriving (Show)
instance Arbitrary IdTA where
  arbitrary                 = mIdTA arbitrary         arbitrary          ((:) <$> elements alpha <*> sListOf (elements alphaDigit))
  shrink (IdTA (IdT p w s)) = mIdTA (sShrink $ toP p) (sShrink $ toW2 w) (sList $ filter validId $ sShrink s)
mIdTA = liftMF3 mkId unP unW2 id where mkId x y = IdTA .IdT x y 
validId s =  all (`elem` alphaDigit) s && not (null s) && head s `elem` alpha
alpha = ['a'..'z']++['A'..'Z']
alphaDigit = alpha ++ ['0'..'9']
toIdTA = IdTA
unIdTA (IdTA s) = s

data ExpTA = ExpTA ExpToken deriving (Show)
instance Arbitrary ExpTA where
  arbitrary        = sized1 sizedExpTA
  shrink (ExpTA e) = mExpTA (shrinkExp e)
sizedExpTA n = mExpTA $ join $ elements $ [arbVar, arbStr, arbNum, arbBool, arbNull] ++ if n > 0 then [arbFunc n, arbArray n, arbObj n] else []
mExpTA = liftM ExpTA

toExpTA = ExpTA
unExpTA (ExpTA e) = e

arbFunc  n = liftM unFuncTA  $ sizedFuncTA  $ n-1
arbArray n = liftM unArrayTA $ sizedArrayTA $ n-1
arbObj   n = liftM unObjTA   $ sizedObjTA   $ n-1
arbVar     = liftM unVarTA   arbitrary
arbStr     = liftM unStrTA   arbitrary
arbNum     = liftM unNumTA   arbitrary
arbBool    = liftM unBoolTA  arbitrary
arbNull    = liftM unNullTA  arbitrary

shrinkExp x@(FuncT{})  = liftM unFuncTA  $ sShrink $ toFuncTA x
shrinkExp x@(ArrayT{}) = liftM unArrayTA $ sShrink $ toArrayTA x
shrinkExp x@(ObjT{})   = liftM unObjTA   $ sShrink $ toObjTA x
shrinkExp x@(VarT{})   = liftM unVarTA   $ sShrink $ toVarTA x
shrinkExp x@(StrT{})   = liftM unStrTA   $ sShrink $ toStrTA x
shrinkExp x@(NumT{})   = liftM unNumTA   $ sShrink $ toNumTA x
shrinkExp x@(BoolT{})  = liftM unBoolTA  $ sShrink $ toBoolTA x
shrinkExp x@(NullT{})  = liftM unNullTA  $ sShrink $ toNullTA x

data FuncTA = FuncTA ExpToken deriving (Show)
instance Arbitrary FuncTA where
  arbitrary                        = sized1 sizedFuncTA
  shrink (FuncTA (FuncT p w i es)) = mFuncTA (sShrink $ toP p) (sShrink $ toW w) (sShrink $ toIdTA i) (sShrink $ map toExpTA es)
sizedFuncTA n                      = mFuncTA arbitrary         arbitrary         arbitrary            (sizedListOf $ sizedExpTA n)
mFuncTA = liftMF4 mkFunc unP unW unIdTA (map unExpTA) where mkFunc x y z = FuncTA .FuncT x y z
toFuncTA = FuncTA
unFuncTA (FuncTA f) = f

data ArrayTA = ArrayTA ExpToken deriving (Show)
instance Arbitrary ArrayTA where
  arbitrary                        = sized1 sizedArrayTA
  shrink (ArrayTA (ArrayT p w es)) = mArrayTA (sShrink $ toP p) (sShrink $ toW2 w) (sShrink $ map toExpTA es)
sizedArrayTA n                     = mArrayTA arbitrary         arbitrary          (sizedListOf $ sizedExpTA n)
mArrayTA = liftMF3 mkArray unP unW2 (map unExpTA) where mkArray x y = ArrayTA .ArrayT x y
toArrayTA = ArrayTA
unArrayTA (ArrayTA a) = a

data ObjTA = ObjTA ExpToken deriving (Show)
instance Arbitrary ObjTA where
  arbitrary                    = sized1 sizedObjTA
  shrink (ObjTA (ObjT p w ps)) = mObjTA (sShrink $ toP p) (sShrink $ toW2 w) (sShrink $ map toPairTA ps)
sizedObjTA n                   = mObjTA arbitrary         arbitrary          (sizedListOf $ sizedPairTA n)
mObjTA = liftMF3 mkObj unP unW2 (map unPairTA) where mkObj x y = ObjTA .ObjT x y
toObjTA = ObjTA
unObjTA (ObjTA o) = o

data VarTA = VarTA ExpToken deriving (Show)
instance Arbitrary VarTA where
  arbitrary                 = mVarTA arbitrary        arbitrary
  shrink (VarTA (VarT p i)) = mVarTA (sShrink $ toP p) (sShrink $ toIdTA i)
mVarTA = liftMF2 mkVar unP unIdTA where mkVar x = VarTA .VarT x
toVarTA = VarTA
unVarTA (VarTA v) = v

data StrTA = StrTA ExpToken deriving (Show)
instance Arbitrary StrTA where
  arbitrary                   = mStrTA arbitrary         arbitrary          (sListOf $ elements valids)
  shrink (StrTA (StrT p w s)) = mStrTA (sShrink $ toP p) (sShrink $ toW2 w) (liftM (filter (`elem` valids)) $ sShrink s)
mStrTA = liftMF3 mkStr unP unW2 id where mkStr x y = StrTA .StrT x y
valids = [' '..'~'] \\ "\"\\"
toStrTA = StrTA
unStrTA (StrTA s) = s

data NumType = Int | Flt | Exp deriving (Eq,Show)
data NumTA = NumTA NumType ExpToken deriving (Show)
instance Arbitrary NumTA where
  arbitrary                       = mNumTA arbitrary         arbitrary          arbitrary   (elements [Int,Flt,Exp])
  shrink (NumTA t (NumT p w _ v)) = mNumTA (sShrink $ toP p) (sShrink $ toW2 w) (sShrink v) [t]
mNumTA pa wa va ta = do
  p <- pa
  w <- wa
  t <- ta
  (v,vs) <- case t of
    Int -> do x <- fmap floor va; return (fromIntegral x, showInt x)
    Flt -> do x <- va;            return (             x, showFlt x)
    Exp -> do x <- va;            return (             x, showExp x)
  return $ NumTA t $ NumT (unP p) (unW2 w) vs v

showExp x = showEFloat Nothing x ""
showFlt x = show (x :: Double)
showInt x = show (x :: Integer)

toNumTA n@(NumT _ _ s v) | s == showInt (floor v) = NumTA Int n
                         | s == showFlt        v  = NumTA Flt n
                         | s == showExp        v  = NumTA Exp n
                         | otherwise              = error $ "Invalid num string ["++s++"]"
unNumTA (NumTA _ n) = n

data BoolTA = BoolTA ExpToken deriving (Show)
instance Arbitrary BoolTA where
  arbitrary                     = mBoolTA arbitrary         arbitrary          arbitrary
  shrink (BoolTA (BoolT p w v)) = mBoolTA (sShrink $ toP p) (sShrink $ toW2 w) (sShrink v)
mBoolTA = liftMF3 mkBool unP unW2 id where mkBool x y = BoolTA .BoolT x y
toBoolTA = BoolTA
unBoolTA (BoolTA b) = b

data NullTA = NullTA ExpToken deriving (Show)
instance Arbitrary NullTA where
  arbitrary                   = mNullTA arbitrary         arbitrary
  shrink (NullTA (NullT p w)) = mNullTA (sShrink $ toP p) (sShrink $ toW2 w)
mNullTA = liftMF2 mkNull unP unW2 where mkNull x = NullTA .NullT x
toNullTA = NullTA
unNullTA (NullTA n) = n

data P = P (Int,Int) deriving (Show)
instance Arbitrary P where
  arbitrary        = mP (choose validP)  (choose validP)
  shrink (P (l,c)) = mP (shrinkValidP l) (shrinkValidP c)
mP = liftMF2 mkP id id where mkP x = P.(,) x 
validP = (1,1000)
shrinkValidP 0 = []
shrinkValidP x = [x `div` 2, x -1]
toP = P
unP (P p) = p

data W = W String deriving (Show)
instance Arbitrary W where
  arbitrary    = mW $ sListOf $ elements " \t\n\v"
  shrink (W w) = mW $ filter (`elem` " \t\n\v") <$> sShrink w
mW = liftM W

toW  = W
unW (W a) = a

toW2 (a,b) = (W a, W b)
unW2 (W a, W b) = (a,b)

sizedListOf p = do Positive m <- arbitrary; ps <- replicateM m p; return $ sList ps

sized1 p = sized f where f i = p $ i `mod` 3
sListOf = liftM sList . listOf
sList = take 10

sShrink :: Arbitrary a => a -> [a]
sShrink = take 1 . shrink

{-| Monomorphism restriction -}
mProgTA  :: (Applicative m, Monad m) => m P -> m [FormTA] -> m ProgTA
mFormTA  :: (Applicative m, Monad m) => m P -> m IdTA     -> m ExpTA    -> m FormTA
mPairTA  :: (Applicative m, Monad m) => m P -> m IdTA     -> m ExpTA    -> m PairTA
mIdTA    :: (Applicative m, Monad m) => m P -> m (W,W)    -> m String   -> m IdTA
mExpTA   :: (Applicative m, Monad m) => m ExpToken -> m ExpTA
mFuncTA  :: (Applicative m, Monad m) => m P -> m  W       -> m IdTA     -> m [ExpTA] -> m FuncTA
mArrayTA :: (Applicative m, Monad m) => m P -> m (W,W)    -> m [ExpTA]  -> m ArrayTA
mObjTA   :: (Applicative m, Monad m) => m P -> m (W,W)    -> m [PairTA] -> m ObjTA
mVarTA   :: (Applicative m, Monad m) => m P ->               m IdTA     -> m VarTA
mStrTA   :: (Applicative m, Monad m) => m P -> m (W,W)    -> m String   -> m StrTA
mBoolTA  :: (Applicative m, Monad m) => m P -> m (W,W)    -> m Bool     -> m BoolTA
mNullTA  :: (Applicative m, Monad m) => m P -> m (W,W)    -> m NullTA

mP       :: (Applicative m, Monad m) => m Int      -> m Int -> m P
mW       :: (Applicative m, Monad m) => m String   -> m W

