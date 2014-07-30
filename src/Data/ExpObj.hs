module Data.ExpObj
( ExpObj(..)
, ObjPos(..)
, getObjPos
, getPos
, fromObjPos
) where

import Data.ExpToken

data ExpObj = TableO ObjPos [[ExpObj]]         [ExpObj]
            | PlotO  ObjPos [(ExpObj,ExpObj)] [(String,ExpObj)]
            | ArrO   ObjPos  [ExpObj]
            | ObjO   ObjPos [(String,ExpObj)]
            | StrO   ObjPos   String
            | NumO   ObjPos   Double
            | BoolO  ObjPos   Bool
            | NullO  ObjPos
              deriving (Eq,Ord,Show)

data ObjPos = Upd Pos | Calc Pos deriving (Eq,Ord,Show)

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