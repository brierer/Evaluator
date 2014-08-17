{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module Marshall.MarshallPropUtils where

import Data.List hiding (any)
import Prelude   hiding (any)

import qualified Prelude as P

import Control.Applicative
import Control.Monad.State
import Data.Eval
import Data.ExpToken
import Data.ExpObj
import Data.Type
import Eval.MatchType
import Eval.MultiPass
import Test.Framework

import Parser.ParserPropUtils

class Is a where
  isTable :: a -> Bool
  isPlot  :: a -> Bool
  isArr   :: a -> Bool
  isObj   :: a -> Bool
  isStr   :: a -> Bool
  isNum   :: a -> Bool
  isBool  :: a -> Bool
  isNull  :: a -> Bool

instance Is ExpToken where
  isTable _ = error "MarshallPropUtils::isTable<ExpToken> [Should not be called]"
  isPlot  _ = error "MarshallPropUtils::isPlot <ExpToken> [Should not be called]"

  isArr (ArrT{})   = True ; isArr _   = False
  isObj (ObjT{})   = True;  isObj _   = False
  isStr (StrT{})   = True;  isStr _   = False
  isNum (NumT{})   = True;  isNum _   = False
  isBool (BoolT{}) = True;  isBool _  = False
  isNull (NullT{}) = True;  isNull _  = False

instance Is ExpObj where
  isTable (TableO{}) = True;  isTable _ = False
  isPlot  (PlotO{})  = True;  isPlot  _ = False
  isArr   (ArrO{})   = True;  isArr   _ = False
  isObj   (ObjO{})   = True;  isObj   _ = False
  isStr   (StrO{})   = True;  isStr   _ = False
  isNum   (NumO{})   = True;  isNum   _ = False
  isBool  (BoolO{})  = True;  isBool  _ = False
  isNull  (NullO{})  = True;  isNull  _ = False

class HasElems a where
  elemsOf :: a -> [a]
  filterElems :: (a -> Bool) -> a -> a

instance HasElems ExpToken where
  elemsOf (ArrT _ _ es) = es
  elemsOf (ObjT _ _ ps) = map pairVal ps

  filterElems pr (ArrT p w es) = ArrT p w $ filter pr es
  filterElems pr (ObjT p w ps) = ObjT p w $ filter (pr.pairVal) ps

instance HasElems ExpObj where
  elemsOf (ArrO _ es) = es
  elemsOf (ObjO _ ps) = map snd ps

  filterElems pr (ArrO p es) = ArrO p $ filter pr es
  filterElems pr (ObjO p ps) = ObjO p $ filter (pr.snd) ps

data ExpOA =  ExpOA ExpObj deriving (Eq,Show)
instance Unto ExpOA ExpObj where to = ExpOA; un (ExpOA e) = e
instance Arbitrary ExpOA where arbitrary = sized1 tall; shrink (ExpOA e) = mExpOA (shrinkO e)
instance Tall      ExpOA where                                    tall n = mExpOA (randomO n)
randomO n = join $ elements $ [mStrU, mNumU, mBoolU, mNullU] ++ if n > 0 then [mTableC n,mPlotC n, mArrC n, mObjC n] else []
mExpOA = liftM ExpOA

mStrU     = liftM un (arbitrary  :: Gen (StrPA   UOP))
mNumU     = liftM un (arbitrary  :: Gen (NumPA   UOP))
mBoolU    = liftM un (arbitrary  :: Gen (BoolPA  UOP))
mNullU    = liftM un (arbitrary  :: Gen (NullPA  UOP))

mTableC n = liftM un (tall (n-1) :: Gen (TablePA COP))
mPlotC  n = liftM un (tall (n-1) :: Gen (PlotPA  COP))
mArrC   n = liftM un (tall (n-1) :: Gen (ArrPA   COP))
mObjC   n = liftM un (tall (n-1) :: Gen (ObjPA   COP))
mStrC     = liftM un (arbitrary  :: Gen (StrPA   COP))
mNumC     = liftM un (arbitrary  :: Gen (NumPA   COP))
mBoolC    = liftM un (arbitrary  :: Gen (BoolPA  COP))
mNullC    = liftM un (arbitrary  :: Gen (NullPA  COP))

shrinkO x@(TableO (Upd{}) _ _)  = liftM un $ sShrink (to x :: TablePA UOP)
shrinkO x@(PlotO  (Upd{}) _ _)  = liftM un $ sShrink (to x :: PlotPA  UOP)
shrinkO x@(ArrO   (Upd{}) _)    = liftM un $ sShrink (to x :: ArrPA   UOP)
shrinkO x@(ObjO   (Upd{}) _)    = liftM un $ sShrink (to x :: ObjPA   UOP)
shrinkO x@(StrO   (Upd{}) _)    = liftM un $ sShrink (to x :: StrPA   UOP)
shrinkO x@(NumO   (Upd{}) _)    = liftM un $ sShrink (to x :: NumPA   UOP)
shrinkO x@(BoolO  (Upd{}) _)    = liftM un $ sShrink (to x :: BoolPA  UOP)
shrinkO x@(NullO  (Upd{}))      = liftM un $ sShrink (to x :: NullPA  UOP)
shrinkO x@(TableO (Calc{}) _ _) = liftM un $ sShrink (to x :: TablePA COP)
shrinkO x@(PlotO  (Calc{}) _ _) = liftM un $ sShrink (to x :: PlotPA  COP)
shrinkO x@(ArrO   (Calc{}) _)   = liftM un $ sShrink (to x :: ArrPA   COP)
shrinkO x@(ObjO   (Calc{}) _)   = liftM un $ sShrink (to x :: ObjPA   COP)
shrinkO x@(StrO   (Calc{}) _)   = liftM un $ sShrink (to x :: StrPA   COP)
shrinkO x@(NumO   (Calc{}) _)   = liftM un $ sShrink (to x :: NumPA   COP)
shrinkO x@(BoolO  (Calc{}) _)   = liftM un $ sShrink (to x :: BoolPA  COP)
shrinkO x@(NullO  (Calc{}))     = liftM un $ sShrink (to x :: NullPA  COP)

data TablePA a = TablePA    ExpObj deriving (Show)
instance Unto   (TablePA a) ExpObj where to = TablePA; un (TablePA t) = t
instance (Arbitrary a,Unto a ObjPos) => Arbitrary (TablePA a) where arbitrary = sized1 tall; shrink (TablePA (TableO p ess es)) = mTablePA (tShrink p) (shrink $ map (map to) ess)  (tShrinks es)
instance (Arbitrary a,Unto a ObjPos) => Tall      (TablePA a) where                                                      tall n = mTablePA  arbitrary  (sListOf $ talls n)          (talls n)
mTablePA = liftMF3 f un (equalize.map (map un)) (map un) where f x y = TablePA .TableO x y
equalize xss = let n = minimum $ map length xss in map (take n) xss

data PlotPA a = PlotPA    ExpObj deriving (Show)
instance Unto  (PlotPA a) ExpObj where to = PlotPA; un (PlotPA p) = p
instance (Arbitrary a,Unto a ObjPos) => Arbitrary (PlotPA a) where arbitrary = sized1 tall; shrink (PlotPA (PlotO p ps o)) = mPlotPA (tShrink p) (tShrinks ps) (tShrinks o)
instance (Arbitrary a,Unto a ObjPos) => Tall      (PlotPA a) where                                                  tall n = mPlotPA  arbitrary  (talls n)     (talls n)
mPlotPA = liftMF3 f un (map un) (map un) where f x y = PlotPA .PlotO x y
instance Unto (ExpOA,ExpOA) (ExpObj,ExpObj) where un (x,y) = (un x, un y); to (x,y) = (to x, to y)

data ArrPA a = ArrPA ExpObj deriving (Show)
instance Unto  (ArrPA a) ExpObj where to = ArrPA; un (ArrPA a) = a
instance (Arbitrary a,Unto a ObjPos) => Arbitrary (ArrPA a) where arbitrary = sized1 tall; shrink (ArrPA (ArrO p es)) = mArrPA (tShrink p) (tShrinks es)
instance (Arbitrary a,Unto a ObjPos) => Tall      (ArrPA a) where                                              tall n = mArrPA  arbitrary  (talls n)
mArrPA = liftMF2 f un (map un) where f x = ArrPA .ArrO x

data ObjPA a =  ObjPA ExpObj deriving (Show)
instance Unto (ObjPA a) ExpObj where to = ObjPA; un (ObjPA o) = o
instance (Arbitrary a,Unto a ObjPos) => Arbitrary (ObjPA a) where arbitrary = sized1 tall; shrink (ObjPA (ObjO p ps)) = mObjPA (tShrink p) (tShrinks ps)
instance (Arbitrary a,Unto a ObjPos) => Tall      (ObjPA a) where                                              tall n = mObjPA  arbitrary  (talls n)
mObjPA = liftMF2 f un (map un) where f x = ObjPA .ObjO x

data StrPA a =  StrPA ExpObj deriving (Show)
instance Unto (StrPA a) ExpObj where to = StrPA; un (StrPA s) = s
instance (Arbitrary a,Unto a ObjPos) => Arbitrary (StrPA a) where
  arbitrary                 = mStrPA  arbitrary   arbitrary
  shrink (StrPA (StrO p s)) = mStrPA (tShrink p) (tShrink s)
mStrPA = liftMF2 f un un where f x = StrPA .StrO x

data NumPA a = NumPA ExpObj deriving (Show)
instance Unto (NumPA a) ExpObj where to  = NumPA; un (NumPA n) = n
instance (Arbitrary a,Unto a ObjPos) => Arbitrary (NumPA a) where
  arbitrary                 = mNumPA  arbitrary   arbitrary
  shrink (NumPA (NumO p v)) = mNumPA (tShrink p) (sShrink v)
mNumPA = liftMF2 f un id where f x = NumPA .NumO x

data BoolPA a = BoolPA ExpObj deriving (Show)
instance Unto (BoolPA a) ExpObj where to = BoolPA; un (BoolPA b) = b
instance (Arbitrary a,Unto a ObjPos) => Arbitrary (BoolPA a) where
  arbitrary                   = mBoolPA  arbitrary   arbitrary
  shrink (BoolPA (BoolO p v)) = mBoolPA (tShrink p) (sShrink v)
mBoolPA = liftMF2 f un id where f x = BoolPA .BoolO x

data NullPA a = NullPA ExpObj deriving (Show)
instance Unto (NullPA a) ExpObj where to = NullPA; un (NullPA n) = n
instance (Arbitrary a,Unto a ObjPos) => Arbitrary (NullPA a) where
  arbitrary                 = mNullPA  arbitrary
  shrink (NullPA (NullO p)) = mNullPA (tShrink p)
mNullPA = liftM (NullPA .NullO .un)

data ExpUCA = ExpUCA ExpObj deriving (Eq,Show)
instance Unto ExpUCA ExpObj where to = ExpUCA; un (ExpUCA e) = e
instance Arbitrary ExpUCA where arbitrary = sized1 tall; shrink (ExpUCA e) = mExpUCA (shrinkO e)
instance Tall      ExpUCA where                                     tall n = mExpUCA (randomUC n)
randomUC n = join $ elements $ [mStrU,mStrC,mNumU,mNumC,mBoolU,mBoolC,mNullU,mNullC] ++ if n > 0 then [mTableUC n,mPlotUC n,mArrUC n,mObjUC n] else []
mExpUCA = liftM ExpUCA

mTableUC n = liftM un (tall (n-1) :: Gen TableUCA)
mPlotUC  n = liftM un (tall (n-1) :: Gen PlotUCA)
mArrUC   n = liftM un (tall (n-1) :: Gen ArrUCA)
mObjUC   n = liftM un (tall (n-1) :: Gen ObjUCA)

data TableUCA = TableUCA ExpObj deriving (Show)
instance Unto   TableUCA ExpObj where to = TableUCA; un (TableUCA t) = t
instance Arbitrary TableUCA where arbitrary = sized1 tall; shrink (TableUCA (TableO p ess es)) = mTableUCA (tShrink p) (shrink $ map (map to) ess)  (tShrinks es)
instance Tall      TableUCA where                                                       tall n = mTableUCA  arbitrary  (sListOf $ talls n)          (talls n)
mTableUCA = liftMF3 f un (equalize.map (map un)) (map un) where f x y = TableUCA .TableO x y
instance Unto (ExpUCA,ExpUCA) (ExpObj,ExpObj) where un (x,y) = (un x, un y); to (x,y) = (to x, to y)

data PlotUCA = PlotUCA ExpObj deriving (Show)
instance Unto  PlotUCA ExpObj where to = PlotUCA; un (PlotUCA p) = p
instance Arbitrary PlotUCA where arbitrary = sized1 tall; shrink (PlotUCA (PlotO p ps o)) = mPlotUCA (tShrink p) (tShrinks ps) (tShrinks o)
instance Tall      PlotUCA where                                                   tall n = mPlotUCA  arbitrary  (talls n)     (talls n)
mPlotUCA = liftMF3 f un (map un) (map un) where f x y = PlotUCA .PlotO x y

data ArrUCA = ArrUCA ExpObj deriving (Show)
instance Unto ArrUCA ExpObj where to = ArrUCA; un (ArrUCA a) = a
instance Arbitrary ArrUCA where arbitrary = sized1 tall; shrink (ArrUCA (ArrO p es)) = mArrUCA (tShrink p) (tShrinks es)
instance Tall      ArrUCA where                                               tall n = mArrUCA  arbitrary  (talls n)
mArrUCA = liftMF2 f un (map un) where f x = ArrUCA .ArrO x

data ObjUCA = ObjUCA ExpObj deriving (Show)
instance Unto ObjUCA ExpObj where to = ObjUCA; un (ObjUCA o) = o
instance Arbitrary ObjUCA where arbitrary = sized1 tall; shrink (ObjUCA (ObjO p ps)) = mObjUCA (tShrink p) (tShrinks ps)
instance Tall      ObjUCA where                                               tall n = mObjUCA  arbitrary  (talls n)
mObjUCA = liftMF2 f un (map un) where f x = ObjUCA .ObjO x

data ExpTS =  ExpTS ExpToken deriving (Show)
instance Unto ExpTS ExpToken where to = ExpTS; un (ExpTS e) = e
instance Arbitrary ExpTS where arbitrary = sized1 tall; shrink (ExpTS e) = mExpTS (tShrink e)
instance Tall      ExpTS where                                    tall n = mExpTS (tall n)
mExpTS = liftM (ExpTS .removeVarsAndFuncs.un)

data ArrTS =  ArrTS ExpToken deriving (Show)
instance Unto ArrTS ExpToken where to = ArrTS; un (ArrTS e) = e
instance Arbitrary ArrTS where arbitrary = sized1 tall; shrink (ArrTS e) = mArrTS (tShrink e)
instance Tall      ArrTS where                                    tall n = mArrTS (tall n)
mArrTS = liftM (ArrTS .removeVarsAndFuncs.un)

data ObjTS =  ObjTS ExpToken deriving (Show)
instance Unto ObjTS ExpToken where to = ObjTS; un (ObjTS e) = e
instance Arbitrary ObjTS where arbitrary = sized1 tall; shrink (ObjTS e) = mObjTS (tShrink e)
instance Tall      ObjTS where                                    tall n = mObjTS (tall n)
mObjTS = liftM (ObjTS .removeVarsAndFuncs.un)

data ObjPosA =     ObjPosA ObjPos deriving (Show)
instance Unto      ObjPosA ObjPos where to = ObjPosA; un (ObjPosA p) = p
instance Arbitrary ObjPosA where
  arbitrary                 = mObjPosA (elements [Upd,Calc])  arbitrary
  shrink (ObjPosA (Upd  p)) = mObjPosA (return Upd)          (tShrink p)
  shrink (ObjPosA (Calc p)) = mObjPosA (return Calc)         (tShrink p)
mObjPosA fa p = do f <- fa; liftM (ObjPosA .f .un) p

data UOP = UOP ObjPos deriving (Show)
instance Unto UOP ObjPos where to = UOP; un (UOP p) = p
instance Arbitrary UOP where
  arbitrary            = mUOP  arbitrary
  shrink (UOP (Upd p)) = mUOP (tShrink p)
mUOP = liftM (UOP .Upd .un)

data COP = COP ObjPos deriving (Show)
instance Unto COP ObjPos where to = COP; un (COP p) = p
instance Arbitrary COP where
  arbitrary             = mCOP  arbitrary
  shrink (COP (Calc p)) = mCOP (tShrink p)
mCOP = liftM (COP .Calc .un)

instance Unto b ExpObj    => Unto (FuncTA,b) (ExpToken,ExpObj) where un (x,y) = (un x,un y); to (x,y) = (to x,to y)
instance Unto b ExpObj    => Unto (AsciiStr,b) (String,ExpObj) where un (x,y) = (un x,un y); to (x,y) = (to x,to y)
instance (Tall a, Tall b) => Tall (a,b)                        where tall n = liftM2 (,) (tall n) (tall n)

data AsciiStr = AsciiStr String deriving (Show)
instance Unto AsciiStr String where to = AsciiStr .asciiStr; un (AsciiStr s) = s
instance Tall AsciiStr where tall _ = arbitrary
instance Arbitrary AsciiStr where
  arbitrary           = mAsciiStr  arbitrary
  shrink (AsciiStr s) = mAsciiStr (shrink s)
mAsciiStr = liftM (AsciiStr .sList.asciiStr)
asciiStr = filter (`elem` [' '..'~'])

chooseT f = f [(arr,isArr),(obj,isObj),(Str,isStr),(Num,isNum),(Bool,isBool),(Null,isNull)]

mkUtils ts = ( map fst ts
             , map (\(funcT,returnValue) -> (funcName funcT,[],Func $ \_ _ -> return returnValue)) ts)

validFuncs s ts = let ns = map (funcName.fst) ts in s `notElem` ns && ns == nub ns

funcName (FuncT _ (IdT _ _ s) _) = s
funcName x                       = error $ "MarshallPropUtils::funcName [Unexpected pattern ["++show x++"]]"

clearParams (FuncT a b _) = FuncT a b []
clearParams x             = error $ "MarshallPropUtils::clearParams [Unexpected pattern ["++show x++"]]"

removeVarsAndFuncs (FuncT _ (IdT p w _) _) = NullT p w
removeVarsAndFuncs (VarT    (IdT p w _))   = NullT p w
removeVarsAndFuncs (ArrT p w es)           = ArrT p w $ map removeVarsAndFuncs es
removeVarsAndFuncs (ObjT p w ps)           = ObjT p w $ map (mapPair removeVarsAndFuncs) ps
removeVarsAndFuncs x                       = x

{-| Mandatory type signatures -}
mExpOA   :: (Applicative m, Monad m)               => m ExpObj                                         -> m  ExpOA
mTablePA :: (Applicative m, Monad m,Unto a ObjPos) => m a -> m [[ExpOA]]         -> m [ExpOA]          -> m (TablePA a)
mPlotPA  :: (Applicative m, Monad m,Unto a ObjPos) => m a -> m [(ExpOA,ExpOA)] -> m [(AsciiStr,ExpOA)] -> m (PlotPA a)
mArrPA   :: (Applicative m, Monad m,Unto a ObjPos) => m a -> m [ExpOA]                                 -> m (ArrPA a)
mObjPA   :: (Applicative m, Monad m,Unto a ObjPos) => m a -> m [(AsciiStr,ExpOA)]                      -> m (ObjPA a)
mStrPA   :: (Applicative m, Monad m,Unto a ObjPos) => m a -> m AsciiStr                                -> m (StrPA a)
mNumPA   :: (Applicative m, Monad m,Unto a ObjPos) => m a -> m Double                                  -> m (NumPA a)
mBoolPA  :: (Applicative m, Monad m,Unto a ObjPos) => m a -> m Bool                                    -> m (BoolPA a)
mNullPA  :: (Applicative m, Monad m,Unto a ObjPos) => m a                                              -> m (NullPA a)

mExpUCA   :: (Applicative m, Monad m) => m ExpObj                                                  -> m ExpUCA
mTableUCA :: (Applicative m, Monad m) => m ObjPosA -> m [[ExpUCA]]        -> m [ExpUCA]            -> m TableUCA
mPlotUCA  :: (Applicative m, Monad m) => m ObjPosA -> m [(ExpUCA,ExpUCA)] -> m [(AsciiStr,ExpUCA)] -> m PlotUCA
mArrUCA   :: (Applicative m, Monad m) => m ObjPosA -> m [ExpUCA]                                   -> m ArrUCA
mObjUCA   :: (Applicative m, Monad m) => m ObjPosA -> m [(AsciiStr,ExpUCA)]                        -> m ObjUCA

mExpTS   :: (Applicative m, Monad m) => m ExpTA -> m ExpTS
mArrTS   :: (Applicative m, Monad m) => m ArrTA -> m ArrTS
mObjTS   :: (Applicative m, Monad m) => m ObjTA -> m ObjTS

mObjPosA :: (Applicative m, Monad m) => m (Pos -> ObjPos) -> m P -> m ObjPosA
mUOP     :: (Applicative m, Monad m) => m P -> m UOP
mCOP     :: (Applicative m, Monad m) => m P -> m COP

mAsciiStr :: (Applicative m, Monad m) => m String -> m AsciiStr
