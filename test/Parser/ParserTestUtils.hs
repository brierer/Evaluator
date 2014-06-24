{-# OPTIONS_GHC -fno-warn-incomplete-patterns  #-}
module Parser.ParserTestUtils where

import Data.List                       ((\\))
import Data.Token                      (ProgToken(..),FormToken(..),PairToken(..),IdToken(..),ExpToken(..))
import Control.Applicative             ((<$>),(<*>))
import Control.Monad                   (liftM,liftM2,liftM3,join,replicateM)
import Numeric                         (showEFloat)
import Parser.Monolithic               (unparse)
import Test.Framework                  (Arbitrary,Positive(..),arbitrary,shrink,elements,sized,listOf)
import Text.ParserCombinators.Parsec   (parse)

testCase p = (\(Right x) -> x) . parse p "" . unparse

data ProgTA = ProgTA ProgToken deriving (Eq,Show)
instance Arbitrary ProgTA where
  arbitrary                  = sized1 sizedProgTA
  shrink (ProgTA (ProgT fs)) = mProgTA (shrink $ map toFormTA fs)
sizedProgTA n                = mProgTA (sizedListOf $ sizedFormTA n)
mProgTA = liftM (ProgTA .ProgT .map unFormTA)
toProgTA = ProgTA
unProgTA (ProgTA p) = p

data FormTA = FormTA FormToken deriving (Show)
instance Arbitrary FormTA where
  arbitrary                   = sized1 sizedFormTA
  shrink (FormTA (FormT i e)) = mFormTA (shrink $ toIdTA i) (shrink $ toExpTA e)
sizedFormTA n                 = mFormTA arbitrary          (sizedExpTA n)
mFormTA = liftM2 $ \i e -> FormTA $ FormT (unIdTA i) $ unExpTA e
toFormTA = FormTA
unFormTA (FormTA f) = f

data PairTA = PairTA PairToken deriving (Show)
instance Arbitrary PairTA where
  arbitrary                   = sized1 sizedPairTA
  shrink (PairTA (PairT i v)) = mPairTA (shrink $ toIdTA i) (shrink $ toExpTA v)
sizedPairTA n                 = mPairTA  arbitrary         (sizedExpTA n)
mPairTA = liftM2 $ \ i e -> PairTA $ PairT (unIdTA i) $ unExpTA e
toPairTA = PairTA
unPairTA (PairTA p) = p

data IdTA = IdTA IdToken deriving (Show)
instance Arbitrary IdTA where
  arbitrary               = mIdTA arbitrary         ((:) <$> elements alpha <*> sListOf (elements alphaDigit))
  shrink (IdTA (IdT w s)) = mIdTA (shrink (toW2 w)) (sList $ filter validId $ shrink s)
mIdTA wa ia = do w <- wa; i <- ia; return $ IdTA $ IdT (unW2 w) i 
validId s =  all (`elem` alphaDigit) s && not (null s) && head s `elem` alpha
alpha = ['a'..'z']++['A'..'Z']
alphaDigit = alpha ++ ['0'..'9']
toIdTA = IdTA
unIdTA (IdTA s) = s

data ExpTA = ExpTA ExpToken deriving (Show)
instance Arbitrary ExpTA where
  arbitrary        = sized1 sizedExpTA
  shrink (ExpTA e) = mExpTA (shrinkExp e)
mExpTA = liftM ExpTA

sizedExpTA n = mExpTA $ join $ elements $ [arbVar, arbStr, arbNum, arbBool, arbNull] ++ if n > 0 then [arbFunc n, arbArray n, arbObj n] else []

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

shrinkExp x@(FuncT{})  = liftM unFuncTA  $ shrink $ toFuncTA x
shrinkExp x@(ArrayT{}) = liftM unArrayTA $ shrink $ toArrayTA x
shrinkExp x@(ObjT{})   = liftM unObjTA   $ shrink $ toObjTA x
shrinkExp x@(VarT{})   = liftM unVarTA   $ shrink $ toVarTA x
shrinkExp x@(StrT{})   = liftM unStrTA   $ shrink $ toStrTA x
shrinkExp x@(NumT{})   = liftM unNumTA   $ shrink $ toNumTA x
shrinkExp x@(BoolT{})  = liftM unBoolTA  $ shrink $ toBoolTA x
shrinkExp x@(NullT{})  = liftM unNullTA  $ shrink $ toNullTA x

data FuncTA = FuncTA ExpToken deriving (Show)
instance Arbitrary FuncTA where
  arbitrary                      = sized1 sizedFuncTA
  shrink (FuncTA (FuncT w i es)) = mFuncTA (shrink $ toW w) (shrink $ toIdTA i) (shrink $ map toExpTA es)
sizedFuncTA n                    = mFuncTA arbitrary         arbitrary         (sizedListOf $ sizedExpTA n)
mFuncTA = liftM3 $ \w i es -> FuncTA $ FuncT (unW w) (unIdTA i) $ map unExpTA es
toFuncTA = FuncTA
unFuncTA (FuncTA f) = f

data ArrayTA = ArrayTA ExpToken deriving (Show)
instance Arbitrary ArrayTA where
  arbitrary                      = sized1 sizedArrayTA
  shrink (ArrayTA (ArrayT w es)) = mArrayTA (shrink $ toW2 w) (shrink $ map toExpTA es)
sizedArrayTA n                   = mArrayTA arbitrary         (sizedListOf $ sizedExpTA n)
mArrayTA = liftM2 $ \w es -> ArrayTA $ ArrayT (unW2 w) $ map unExpTA es
toArrayTA = ArrayTA
unArrayTA (ArrayTA a) = a

data ObjTA = ObjTA ExpToken deriving (Show)
instance Arbitrary ObjTA where
  arbitrary                  = sized1 sizedObjTA
  shrink (ObjTA (ObjT w ps)) = mObjTA (shrink $ toW2 w) (shrink $ map toPairTA ps)
sizedObjTA n                 = mObjTA arbitrary         (sizedListOf $ sizedPairTA n)
mObjTA = liftM2 $ \w ps -> ObjTA $ ObjT (unW2 w) $ map unPairTA ps
toObjTA = ObjTA
unObjTA (ObjTA o) = o

data VarTA = VarTA ExpToken deriving (Show)
instance Arbitrary VarTA where
  arbitrary               = mVarTA arbitrary
  shrink (VarTA (VarT i)) = mVarTA (shrink $ toIdTA i)
mVarTA = liftM $ \i -> VarTA $ VarT $ unIdTA i
toVarTA = VarTA
unVarTA (VarTA v) = v

data StrTA = StrTA ExpToken deriving (Show)
instance Arbitrary StrTA where
  arbitrary                 = mStrTA arbitrary         (sListOf $ elements valids)
  shrink (StrTA (StrT w s)) = mStrTA (shrink (toW2 w)) (liftM (sList.filter (`elem` valids)) $ shrink s)
mStrTA = liftM2 $ \w v -> StrTA $ StrT (unW2 w) v
valids = [' '..'~'] \\ "\"\\"
toStrTA = StrTA
unStrTA (StrTA s) = s

data NumType = Int | Flt | Exp deriving (Eq,Show)
data NumTA = NumTA NumType ExpToken deriving (Show)
instance Arbitrary NumTA where
  arbitrary                     = mNumTA arbitrary         arbitrary  (elements [Int,Flt,Exp])
  shrink (NumTA t (NumT w _ v)) = mNumTA (shrink $ toW2 w) (shrink v) [t]
mNumTA wa va ta = do
  w <- wa
  t <- ta
  (v,vs) <- case t of
    Int -> do x <- fmap floor va; return (fromIntegral x, showInt x)
    Flt -> do x <- va;            return (             x, showFlt x)
    Exp -> do x <- va;            return (             x, showExp x)
  return $ NumTA t $ NumT (unW2 w) vs v

showExp x = showEFloat Nothing x ""
showFlt x = show (x :: Double)
showInt x = show (x :: Integer)

toNumTA n@(NumT _ s v) | s == showInt (floor v) = NumTA Int n
                       | s == showFlt        v  = NumTA Flt n
                       | s == showExp        v  = NumTA Exp n
                       | otherwise              = error $ "Invalid num string ["++s++"]"
unNumTA (NumTA _ n) = n

data BoolTA = BoolTA ExpToken deriving (Show)
instance Arbitrary BoolTA where
  arbitrary                   = mBoolTA arbitrary         arbitrary
  shrink (BoolTA (BoolT w v)) = mBoolTA (shrink $ toW2 w) (shrink v)
mBoolTA = liftM2 $ \w v -> BoolTA $ BoolT (unW2 w) v
toBoolTA = BoolTA
unBoolTA (BoolTA b) = b

data NullTA = NullTA ExpToken deriving (Show)
instance Arbitrary NullTA where
  arbitrary                 = mNullTA arbitrary
  shrink (NullTA (NullT w)) = mNullTA (shrink $ toW2 w)
mNullTA = liftM (NullTA .NullT .unW2)
toNullTA = NullTA
unNullTA (NullTA n) = n

data W = W String deriving (Show)
instance Arbitrary W where
  arbitrary    = mW $ sListOf $ elements " \t\n\v"
  shrink (W w) = mW $ sList .filter (`elem` " \t\n\v") <$> shrink w
mW = liftM W

toW  = W
unW (W a) = a

toW2 (a,b) = (W a, W b)
unW2 (W a, W b) = (a,b)

sizedListOf p = do Positive m <- arbitrary; ps <- replicateM m p; return $ sList ps

sized1 p = sized f where f i = p $ i `mod` 3
sListOf = liftM sList . listOf
sList = take 10

{-| Monomorphism restriction -}
mProgTA  :: Monad m => m [FormTA] -> m ProgTA
mFormTA  :: Monad m => m IdTA     -> m ExpTA    -> m FormTA
mPairTA  :: Monad m => m IdTA     -> m ExpTA    -> m PairTA
mIdTA    :: Monad m => m (W,W)    -> m String   -> m IdTA
mExpTA   :: Monad m => m ExpToken -> m ExpTA
mFuncTA  :: Monad m => m  W       -> m IdTA     -> m [ExpTA] -> m FuncTA
mArrayTA :: Monad m => m (W,W)    -> m [ExpTA]  -> m ArrayTA
mObjTA   :: Monad m => m (W,W)    -> m [PairTA] -> m ObjTA
mVarTA   :: Monad m =>               m IdTA     -> m VarTA
mStrTA   :: Monad m => m (W,W)    -> m String   -> m StrTA
mBoolTA  :: Monad m => m (W,W)    -> m Bool     -> m BoolTA
mNullTA  :: Monad m => m (W,W)    -> m NullTA
mW       :: Monad m => m String   -> m W

