{-# OPTIONS_GHC -F -pgmF htfpp #-}
module MatchType.MatchTypeProp where

import Prelude   hiding (any)

import qualified Prelude as P

import Data.EvalError
import Data.ExpObj
import Data.Type
import Eval.MatchType
import Test.Framework

import Marshall.MarshallPropUtils
import MatchType.MatchTypePropUtils
import MatchType.MatchTypeUtils

prop_TableTypeFailure (TableTypeFailure  e) = Left (TypeMismatch $ TMLeaf (getPos e) LeafTable (getRoot e)) == simpleMatch e Table
prop_PlotTypeFailure  (PlotTypeFailure   e) = Left (TypeMismatch $ TMLeaf (getPos e) LeafPlot  (getRoot e)) == simpleMatch e Plot
prop_ArrTypeFailure   (ArrTypeFailure    e) = Left (TypeMismatch $ TMLeaf (getPos e) NodeArr   (getRoot e)) == simpleMatch e arr
prop_ObjTypeFailure   (ObjTypeFailure    e) = Left (TypeMismatch $ TMLeaf (getPos e) NodeObj   (getRoot e)) == simpleMatch e obj
prop_StrTypeFailure   (StrTypeFailure    e) = Left (TypeMismatch $ TMLeaf (getPos e) LeafStr   (getRoot e)) == simpleMatch e Str
prop_NumTypeFailure   (NumTypeFailure    e) = Left (TypeMismatch $ TMLeaf (getPos e) LeafNum   (getRoot e)) == simpleMatch e Num
prop_BoolTypeFailure  (BoolTypeFailure   e) = Left (TypeMismatch $ TMLeaf (getPos e) LeafBool  (getRoot e)) == simpleMatch e Bool
prop_NullTypeFailure  (NullTypeFailure   e) = Left (TypeMismatch $ TMLeaf (getPos e) LeafNull  (getRoot e)) == simpleMatch e Null

prop_TableTypeSuccess (TablePA  e) = Right e == simpleMatch e Table
prop_PlotTypeSuccess  (PlotPA   e) = Right e == simpleMatch e Plot
prop_ArrTypeSuccess   (ArrPA    e) = Right e == simpleMatch e arr
prop_ObjTypeSuccess   (ObjPA    e) = Right e == simpleMatch e obj
prop_StrTypeSuccess   (StrPA    e) = Right e == simpleMatch e Str
prop_NumTypeSuccess   (NumPA    e) = Right e == simpleMatch e Num
prop_BoolTypeSuccess  (BoolPA   e) = Right e == simpleMatch e Bool
prop_NullTypeSuccess  (NullPA   e) = Right e == simpleMatch e Null

prop_AnySuccess  (ExpOA e) = Right e                                                            == simpleMatch e any
prop_NoneFailure (ExpOA e) = Left (TypeMismatch $ TMLeaf (getPos e) (getRoot none) (getRoot e)) == simpleMatch e none

prop_ArrOfTypeFailure (ArrOfTypeFailure a e t _)  = simpleMatch e t == simpleMatch a (ArrOf t)
prop_ObjOfTypeFailure (ObjOfTypeFailure a e t _)  = simpleMatch e t == simpleMatch a (ObjOf t)
prop_OrTypeFailure    (OrTypeFailure      e _ ts) = matchOr e ts    == simpleMatch e (Or ts)

prop_ArrOfTypeSuccess (ArrOfTypeSuccess a t) = Right a == simpleMatch a (ArrOf t)
prop_ObjOfTypeSuccess (ObjOfTypeSuccess a t) = Right a == simpleMatch a (ObjOf t)
prop_OrTypeSuccess    (OrTypeSuccess e _ ts) = Right e == simpleMatch e (Or ts)

{-| Mandatory type signatures -}
prop_TableTypeSuccess :: TableOA -> Bool
prop_PlotTypeSuccess  :: PlotOA  -> Bool
prop_ArrTypeSuccess   :: ArrOA   -> Bool
prop_ObjTypeSuccess   :: ObjOA   -> Bool
prop_StrTypeSuccess   :: StrOA   -> Bool
prop_NumTypeSuccess   :: NumOA   -> Bool
prop_BoolTypeSuccess  :: BoolOA  -> Bool
prop_NullTypeSuccess  :: NullOA  -> Bool