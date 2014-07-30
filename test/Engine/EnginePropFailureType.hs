{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Engine.EnginePropFailureType where

import Prelude hiding (any)

import Data.EvalError
import Data.ExpObj
import Data.Type
import Eval.Marshall
import Eval.MatchType
import Test.Framework

import Engine.EnginePropFailureTypeUtils
import Engine.EnginePropFailureUtils
import Engine.EngineUtils
import Parser.ParserUnitUtils

prop_Show1   (Show1   x e fs)     = typeCase e  NodeArr            "show"        [x] fs
prop_Show2   (Show2   x e fs)     = typeCase e (NodeOr showable)   "show"        [x] fs

prop_Multi1  (Multi1  x e fs)     = typeCase e  NodeArr            "multi"       [x] fs
prop_Multi2  (Multi2  x e fs)     = typeCase e (NodeOr atom)       "multi"       [x] fs

prop_Mean1   (Mean1  x e fs)      = typeCase e  NodeArr            "mean"        [x] fs
prop_Mean2   (Mean2  x e fs)      = typeCase e (NodeOr atom)       "mean"        [x] fs

prop_Desc1   (Desc1  x e fs)      = typeCase e  NodeArr            "descriptive" [x] fs
prop_Desc2   (Desc2  x e fs)      = typeCase e (NodeOr atom)       "descriptive" [x] fs

prop_Table1  (Table1 x y e fs)    = typeCase e  NodeArr            "table"       [x,y] fs
prop_Table2  (Table2 x y e fs)    = typeCase e  NodeArr            "table"       [x,y] fs
prop_Table3  (Table3 x y e fs)    = typeCase e (NodeOr atom)       "table"       [x,y] fs
prop_Table4  (Table4 x y e fs)    = typeCase e  NodeObj            "table"       [x,y] fs
prop_Table5  (Table5 x y e fs)    = typeCase e  NodeArr            "table"       [x,y] fs
prop_Table6  (Table6 x y e fs)    = typeCase e  LeafStr            "table"       [x,y] fs

prop_NTimes1 (NTimes1 x y e fs)   = typeCase e  LeafNum            "nTimes"      [x,y] fs
prop_NTimes2 (NTimes2 x y e fs)   = typeCase e  LeafNum            "nTimes"      [x,y] fs

prop_Take1   (Take1   x y e fs)   = typeCase e  LeafNum            "take"        [x,y] fs
prop_Take2   (Take2   x y e fs)   = typeCase e (NodeOr tableOrArr) "take"        [x,y] fs

prop_Sort1   (Sort1   x y e fs)   = typeCase e  LeafNum            "sort"        [x,y] fs
prop_Sort2   (Sort2   x y e fs)   = typeCase e (NodeOr tableOrArr) "sort"        [x,y] fs

prop_Col1    (Col1    x y e fs)   = typeCase e  LeafNum            "col"         [x,y] fs
prop_Col2    (Col2    x y e fs)   = typeCase e (NodeOr tableOrArr) "col"         [x,y] fs
--
prop_Plot1   (Plot1   x y z e fs) = typeCase e  NodeArr            "plot"        [x,y,z] fs
prop_Plot2   (Plot2   x y z e fs) = typeCase e  LeafNum            "plot"        [x,y,z] fs
prop_Plot3   (Plot3   x y z e fs) = typeCase e  NodeArr            "plot"        [x,y,z] fs
prop_Plot4   (Plot4   x y z e fs) = typeCase e  LeafNum            "plot"        [x,y,z] fs
prop_Plot5   (Plot5   x y z e fs) = typeCase e  NodeObj            "plot"        [x,y,z] fs
prop_Plot6   (Plot6   x y z e fs) = typeCase e  LeafStr            "plot"        [x,y,z] fs

prop_Sort3   (Sort3   x y e fs)   = (Left $ TypeMismatch $ TMNode [TMLeaf (tokPos y) LeafTable NodeArr,TMLeaf (getPos e)  NodeArr      (getRoot e)]) == marshallWith (mkFunc "sort" [x,y]) (toFuncEntries fs)
prop_Sort4   (Sort4   x y e fs)   = (Left $ TypeMismatch $ TMNode [TMLeaf (tokPos y) LeafTable NodeArr,TMLeaf (getPos e) (NodeOr atom) (getRoot e)]) == marshallWith (mkFunc "sort" [x,y]) (toFuncEntries fs)

prop_Col3    (Col3    x y e fs)   = (Left $ TypeMismatch $ TMNode [TMLeaf (tokPos y) LeafTable NodeArr,TMLeaf (getPos e)  NodeArr      (getRoot e)]) == marshallWith (mkFunc "col"  [x,y]) (toFuncEntries fs)
prop_Col4    (Col4    x y e fs)   = (Left $ TypeMismatch $ TMNode [TMLeaf (tokPos y) LeafTable NodeArr,TMLeaf (getPos e) (NodeOr atom) (getRoot e)]) == marshallWith (mkFunc "col"  [x,y]) (toFuncEntries fs)
