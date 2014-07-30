{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
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
instance (Tall a, Tall b) => Tall (a,b)                        where tall n = liftM2 (,) (tall n) (tall n)

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
mExpTS :: (Applicative m, Monad m) => m ExpTA -> m ExpTS
mArrTS :: (Applicative m, Monad m) => m ArrTA -> m ArrTS
mObjTS :: (Applicative m, Monad m) => m ObjTA -> m ObjTS

mUOP   :: (Applicative m, Monad m) => m P -> m UOP
mCOP   :: (Applicative m, Monad m) => m P -> m COP
   