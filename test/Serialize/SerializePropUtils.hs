{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module Serialize.SerializePropUtils where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.ExpObj
import Data.List
import Test.Framework

import Marshall.MarshallPropUtils
import Parser.ParserPropUtils
import Serialize.SerializeUtils

data TableS = TableS String ExpObj String ExpObj deriving (Show)
instance Arbitrary TableS where arbitrary = sized1 tall; shrink (TableS _ a _ b) = mTableS (tShrink a) (tShrink b)
instance Tall      TableS where                                           tall n = mTableS (tall n)    (tall n)
mTableS ca ua = do
  c@(TableO _ essc hc) <- liftM un ca
  u@(TableO _ essu hu) <- liftM un ua
  return $ TableS ("{_type:TABLE,_data:"++showColumns essc ++",_head:"++showElems hc++"}") c ("{_type:UPD,_pos:"++getPosStr u++",_val:{_type:TABLE,_data:"++showColumns essu++",_head:"++showElems hu++"}}") u
showColumns ess = "[" ++ intercalate "," (map showColumn ess)            ++ "]"
showColumn es   = "[" ++ intercalate "," (map (serializeValid.Right) es) ++ "]"

data PlotS = PlotS String ExpObj String ExpObj deriving (Show)
instance Arbitrary PlotS where arbitrary = sized1 tall; shrink (PlotS _ a _ b) = mPlotS (tShrink a) (tShrink b)
instance Tall      PlotS where                                          tall n = mPlotS (tall n)    (tall n)
mPlotS ca ua = do
  c@(PlotO _ psc hc) <- liftM un ca
  u@(PlotO _ psu hu) <- liftM un ua
  return $ PlotS ("{_type:PLOT,_data:"++showPoints psc++",_head:"++showPairs hc++"}") c ("{_type:UPD,_pos:"++getPosStr u++",_val:{_type:PLOT,_data:"++showPoints psu++",_head:"++showPairs hu++"}}") u
showPoints ps   = "[" ++ intercalate "," (map showPoint ps)                   ++ "]"
showPoint (x,y) = "[" ++ serializeValid (Right x)++","++serializeValid (Right y)++ "]"

data ArrS = ArrS String ExpObj String ExpObj deriving (Show)
instance Arbitrary ArrS where arbitrary = sized1 tall; shrink (ArrS _ a _ b) = mArrS (tShrink a) (tShrink b)
instance Tall      ArrS where                                         tall n = mArrS (tall n)    (tall n)
mArrS ca ua = do
  c@(ArrO _ vc) <- liftM un ca
  u@(ArrO _ vu) <- liftM un ua
  return $ ArrS (showElems vc) c ("{_type:UPD,_pos:"++getPosStr u++",_val:"++showElems vu++"}") u
showElems es = "[" ++ intercalate "," (map (serializeValid.Right) es) ++ "]"

data ObjS = ObjS String ExpObj String ExpObj deriving (Show)
instance Arbitrary ObjS where arbitrary = sized1 tall; shrink (ObjS _ a _ b) = mObjS (tShrink a) (tShrink b)
instance Tall      ObjS where                                         tall n = mObjS (tall n)    (tall n)
mObjS ca ua = do
  c@(ObjO _ vc) <- liftM un ca
  u@(ObjO _ vu) <- liftM un ua
  return $ ObjS (showPairs vc) c ("{_type:UPD,_pos:"++getPosStr u++",_val:"++showPairs vu++"}") u
showPairs ps = "{" ++ intercalate "," (map showPair ps) ++ "}"
showPair (k,o) = show k ++ ":" ++ serializeValid (Right o)

data StrS = StrS String ExpObj String ExpObj deriving (Show)
instance Arbitrary StrS where
  arbitrary             = mStrS  arbitrary   arbitrary 
  shrink (StrS _ a _ b) = mStrS (tShrink a) (tShrink b)
mStrS ca ua = do
  c@(StrO _ vc) <- liftM un ca
  u@(StrO _ vu) <- liftM un ua
  return $ StrS (show vc) c ("{_type:UPD,_pos:"++getPosStr u++",_val:"++show vu++"}") u

data NumS = NumS String ExpObj String ExpObj deriving (Show)
instance Arbitrary NumS where
  arbitrary             = mNumS  arbitrary   arbitrary 
  shrink (NumS _ a _ b) = mNumS (tShrink a) (tShrink b)
mNumS ca ua = do
  c@(NumO _ vc) <- liftM un ca
  u@(NumO _ vu) <- liftM un ua
  return $ NumS (show vc) c ("{_type:UPD,_pos:"++getPosStr u++",_val:"++show vu++"}") u

data BoolS = BoolS String ExpObj String ExpObj deriving (Show)
instance Arbitrary BoolS where
  arbitrary              = mBoolS  arbitrary   arbitrary 
  shrink (BoolS _ a _ b) = mBoolS (tShrink a) (tShrink b)
mBoolS ca ua = do
  c@(BoolO _ vc) <- liftM un ca
  u@(BoolO _ vu) <- liftM un ua
  let toVal = map toLower . show
  return $ BoolS (toVal vc) c ("{_type:UPD,_pos:"++getPosStr u++",_val:"++toVal vu++"}") u
 
data NullS = NullS String ExpObj String ExpObj deriving (Show)
instance Arbitrary NullS where
  arbitrary              = mNullS  arbitrary   arbitrary 
  shrink (NullS _ a _ b) = mNullS (tShrink a) (tShrink b)
mNullS ca ua = do
  c <- liftM un ca
  u <- liftM un ua
  return $ NullS "null" c ("{_type:UPD,_pos:"++getPosStr u++",_val:null}") u

getPosStr (TableO p _ _) = posStr p
getPosStr (PlotO  p _ _) = posStr p
getPosStr (ArrO   p _)   = posStr p
getPosStr (ObjO   p _)   = posStr p
getPosStr (StrO   p _)   = posStr p
getPosStr (NumO   p _)   = posStr p
getPosStr (BoolO  p _)   = posStr p
getPosStr (NullO  p)     = posStr p

posStr (Upd (x,y)) = show [x,y]
posStr x           = "SerializeProp::posStr [Unexpected pattern ["++show x++"]]"

serializeCase expA inA expB inB f = expA == serializeValid (f inA) && expB == serializeValid (f inB)

{-| Mandatory type signatures -}
mTableS :: (Applicative m,Monad m) => m (TablePA COP) -> m (TablePA UOP) -> m TableS
mPlotS  :: (Applicative m,Monad m) => m (PlotPA  COP) -> m (PlotPA  UOP) -> m PlotS
mArrS   :: (Applicative m,Monad m) => m (ArrPA   COP) -> m (ArrPA   UOP) -> m ArrS
mObjS   :: (Applicative m,Monad m) => m (ObjPA   COP) -> m (ObjPA   UOP) -> m ObjS
mStrS   :: (Applicative m,Monad m) => m (StrPA   COP) -> m (StrPA   UOP) -> m StrS
mNumS   :: (Applicative m,Monad m) => m (NumPA   COP) -> m (NumPA   UOP) -> m NumS
mBoolS  :: (Applicative m,Monad m) => m (BoolPA  COP) -> m (BoolPA  UOP) -> m BoolS
mNullS  :: (Applicative m,Monad m) => m (NullPA  COP) -> m (NullPA  UOP) -> m NullS


