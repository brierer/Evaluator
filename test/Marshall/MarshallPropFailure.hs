{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Marshall.MarshallPropFailure where

import Data.List hiding (any)
import Prelude   hiding (any)

import qualified Prelude as P

import Data.EvalError
import Data.Type
import Eval.Marshall
import Eval.MatchType
import Test.Framework

import Marshall.MarshallPropFailureUtils
import Marshall.MarshallUtils
import Parser.ParserUnitUtils

prop_NbArgs (NbArgs p s n m) = n /= m ==> Left (ArgCountMismatch p s n m) == marshallWith (mkFunc' p s $ replicate m mockArg) (nbArgEntry s n)

prop_ArrLit  s (ArrLit  es e i) = caseLit NodeArr  arr  s es e i
prop_ObjLit  s (ObjLit  es e i) = caseLit NodeObj  obj  s es e i
prop_StrLit  s (StrLit  es e i) = caseLit LeafStr  Str  s es e i
prop_NumLit  s (NumLit  es e i) = caseLit LeafNum  Num  s es e i
prop_BoolLit s (BoolLit es e i) = caseLit LeafBool Bool s es e i
prop_NullLit s (NullLit es e i) = caseLit LeafNull Null s es e i

prop_ArrOfLit s (ArrOfLit es e i t) = caseOfLit t s es e i ArrOf
prop_ObjOfLit s (ObjOfLit es e i t) = caseOfLit t s es e i ObjOf
prop_OrLit    s (OrLit    es e i t) = caseOrLit t s es e i

prop_TableFunc s (TableFunc ts e i) = caseFunc LeafTable Table s ts e i
prop_PlotFunc  s (PlotFunc  ts e i) = caseFunc LeafPlot  Plot  s ts e i
prop_ArrFunc   s (ArrFunc   ts e i) = caseFunc NodeArr   arr   s ts e i
prop_ObjFunc   s (ObjFunc   ts e i) = caseFunc NodeObj   obj   s ts e i
prop_StrFunc   s (StrFunc   ts e i) = caseFunc LeafStr   Str   s ts e i
prop_NumFunc   s (NumFunc   ts e i) = caseFunc LeafNum   Num   s ts e i
prop_BoolFunc  s (BoolFunc  ts e i) = caseFunc LeafBool  Bool  s ts e i
prop_NullFunc  s (NullFunc  ts e i) = caseFunc LeafNull  Null  s ts e i

prop_ArrOfFunc s (ArrOfFunc ts e i t) = caseOfFunc t s ts e i ArrOf
prop_ObjOfFunc s (ObjOfFunc ts e i t) = caseOfFunc t s ts e i ObjOf
prop_OrFunc    s (OrFunc    ts e i t) = caseOrFunc t s ts e i

