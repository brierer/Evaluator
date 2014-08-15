{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module Serialize.SerializePropUtils where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.ExpObj
import Test.Framework

import Marshall.MarshallPropUtils
import Parser.ParserPropUtils
import Serialize.SerializeUtils

data StrS = StrS String ExpObj String ExpObj deriving (Show)
instance Arbitrary StrS where
  arbitrary             = mStrS  arbitrary   arbitrary 
  shrink (StrS _ a _ b) = mStrS (tShrink a) (tShrink b)
mStrS ca ua = do
  c@(StrO _ vc) <- liftM (clampStr.un) ca
  u@(StrO _ vu) <- liftM (clampStr.un) ua
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

getPosStr (StrO  p _) = posStr p
getPosStr (NumO  p _) = posStr p
getPosStr (BoolO p _) = posStr p
getPosStr (NullO p)   = posStr p

posStr (Upd (x,y)) = show [x,y]
posStr x           = "SerializeProp::posStr [Unexpected pattern ["++show x++"]]"

clampStr (StrO p s) = StrO p $ filter (`elem` [' '..'~']) s
clampStr x          = error $ "SerializePropUtils::clampStr [Unexpected pattern ["++show x++"]]"

serializeCase expA inA expB inB f = expA == serializeValid (f inA) && expB == serializeValid (f inB)

{-| Mandatory type signatures -}
mStrS  :: (Applicative m,Monad m) => m (StrPA  COP) -> m (StrPA  UOP) -> m StrS
mNumS  :: (Applicative m,Monad m) => m (NumPA  COP) -> m (NumPA  UOP) -> m NumS
mBoolS :: (Applicative m,Monad m) => m (BoolPA COP) -> m (BoolPA UOP) -> m BoolS
mNullS :: (Applicative m,Monad m) => m (NullPA COP) -> m (NullPA UOP) -> m NullS


