{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Engine.EnginePropSuccess where

import Data.List hiding (any)
import Prelude   hiding (any)

import Control.Monad
import Control.Monad.State
import Data.Eval
import Data.EvalError
import Data.ExpToken
import Data.ExpObj
import Data.HasPos
import Data.Maybe
import Data.Type
import Eval.Engine
import Eval.Marshall
import Eval.MatchType
import Test.Framework

import Engine.EnginePropFailureConstraintUtils
import Engine.EnginePropFailureUtils
import Engine.EnginePropSuccessUtils
import Marshall.MarshallUtils
import Marshall.MarshallPropFailureUtils
import Marshall.MarshallPropUtils
import MatchType.MatchTypeUnitUtils
import MatchType.MatchTypePropUtils
import Parser.ParserPropUtils
import Parser.ParserUnitUtils

prop_Show1  (Show1 x r p fs)      = successCase     r p "show"        [x]     fs
prop_Show2  (Show2 x r p fs)      = successCase     r p "show"        [x]     fs
prop_Multi  (Multi x r p fs)      = successShowCase r p "multi"       [x]     fs
prop_Mean   (Mean  x r p fs)      = successShowCase r p "mean"        [x]     fs
prop_Desc   (Desc  x r p fs)      = successShowCase r p "descriptive" [x]     fs
                                                                              
prop_Table1 (Table1 x y r p fs)   = successCase     r p "table"       [x,y]   fs
prop_Table2 (Table2 x y r p fs)   = successCase     r p "table"       [x,y]   fs
prop_NTimes (NTimes x y r p fs)   = successCase     r p "nTimes"      [x,y]   fs
prop_Take1  (Take1  x y r p fs)   = successCase     r p "take"        [x,y]   fs
prop_Take2  (Take2  x y r p fs)   = successCase     r p "take"        [x,y]   fs
prop_Sort1  (Sort1  x y r p fs)   = successCase     r p "sort"        [x,y]   fs
prop_Sort2  (Sort2  x y r p fs)   = successCase     r p "sort"        [x,y]   fs
prop_Col1   (Col1   x y r p fs)   = successCase     r p "col"         [x,y]   fs
prop_Col2   (Col2   x y r p fs)   = successCase     r p "col"         [x,y]   fs

prop_Plot   (Plott  x y z r p fs) = successCase     r p "plot"        [x,y,z] fs



