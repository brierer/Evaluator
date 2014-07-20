module Data.ExpObj
( ExpObj(..)
) where

import Data.ExpToken (Pos)

data ExpObj = TableO Pos [[ExpObj]]         [ExpObj]
            | PlotO  Pos [(ExpObj,ExpObj)] [(String,ExpObj)]
            | ArrO   Pos  [ExpObj]
            | ObjO   Pos [(String,ExpObj)]
            | StrO   Pos   String
            | NumO   Pos   Double
            | BoolO  Pos   Bool
            | NullO  Pos  
              deriving (Eq,Ord,Show)