{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric #-}


module Data.ExpObj
( ExpObj(..)
, ObjPos(..)
, getObjPos
, getPos
, fromObjPos
, Param(..)
) where

import Data.ExpToken
import Data.Aeson
import Data.Typeable
import GHC.Generics
import Data.Text (pack)

data ExpObj = TableO {_ObjPos :: ObjPos  , _TableData :: [[ExpObj]] , _TableParam :: Param }
            | PlotO  {_ObjPos :: ObjPos  , _PlotData :: [(ExpObj,ExpObj)] , _PlotParam :: Param }
            | ArrO   {_ObjPos :: ObjPos  , _ArrayData :: [ExpObj] }
            | ObjO   {_ObjPos :: ObjPos  , _ObjO :: [(String,ExpObj)] }
            | StrO   {_ObjPos :: ObjPos  , _StrO ::  String }
            | NumO   {_ObjPos :: ObjPos  , _NumO ::  Double }
            | BoolO  {_ObjPos :: ObjPos  , _BoolO ::  Bool }
            | WidgetO{_ObjPos :: ObjPos  , _Type :: String,  _Param :: Param}
            | NullO  {_ObjPos :: ObjPos }
              deriving (Eq,Ord,Show, Generic)

data ObjPos = Upd Pos | Calc Pos deriving (Eq,Ord,Show, Generic)

data Param = Param [(String,ExpObj)] deriving (Eq,Ord,Show, Generic)

instance ToJSON ExpObj
instance ToJSON ObjPos
instance ToJSON Param
	where toJSON (Param ps) = (object $ fmap toObject ps)
								where toObject (s,p) = ((pack s)  .= p) 

getObjPos :: ExpObj -> ObjPos
getObjPos (TableO p _ _) = p
getObjPos (PlotO  p _ _) = p
getObjPos (ArrO p _)     = p
getObjPos (ObjO p _)     = p
getObjPos (StrO p _)     = p
getObjPos (NumO p _)     = p
getObjPos (BoolO p _)    = p
getObjPos (NullO p)      = p

getPos :: ExpObj -> Pos
getPos = fromObjPos.getObjPos

fromObjPos :: ObjPos -> Pos
fromObjPos (Upd p) = p
fromObjPos (Calc p) = p