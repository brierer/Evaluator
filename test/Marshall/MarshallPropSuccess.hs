{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Marshall.MarshallPropSuccess where

import Prelude hiding (any)

import qualified Prelude as P

import Data.Type
import Eval.MatchType
import Test.Framework

import Marshall.MarshallPropSuccessUtils

prop_ArrLit  s (ArrLit  es e) = caseLit arr  s es e
prop_ObjLit  s (ObjLit  es e) = caseLit obj  s es e
prop_StrLit  s (StrLit  es e) = caseLit Str  s es e    
prop_NumLit  s (NumLit  es e) = caseLit Num  s es e
prop_BoolLit s (BoolLit es e) = caseLit Bool s es e
prop_NullLit s (NullLit es e) = caseLit Null s es e

prop_ArrOfLit s (ArrOfLit es e t) = caseOfLit t s es e ArrOf
prop_ObjOfLit s (ObjOfLit es e t) = caseOfLit t s es e ObjOf
prop_OrLit    s (OrLit    es e t) = caseOrLit t s es e 

prop_TableFunc s (TableFunc ts e) = caseFunc Table s ts e
prop_PlotFunc  s (PlotFunc  ts e) = caseFunc Plot  s ts e
prop_ArrFunc   s (ArrFunc   ts e) = caseFunc arr   s ts e
prop_ObjFunc   s (ObjFunc   ts e) = caseFunc obj   s ts e
prop_StrFunc   s (StrFunc   ts e) = caseFunc Str   s ts e    
prop_NumFunc   s (NumFunc   ts e) = caseFunc Num   s ts e
prop_BoolFunc  s (BoolFunc  ts e) = caseFunc Bool  s ts e
prop_NullFunc  s (NullFunc  ts e) = caseFunc Null  s ts e

prop_ArrOfFunc s (ArrOfFunc ts e t) = caseOfFunc t s ts e ArrOf
prop_ObjOfFunc s (ObjOfFunc ts e t) = caseOfFunc t s ts e ObjOf
prop_OrFunc    s (OrFunc    ts e t) = caseOrFunc t s ts e 

