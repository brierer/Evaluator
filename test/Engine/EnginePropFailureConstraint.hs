{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Engine.EnginePropFailureConstraint where

import Prelude hiding (any)

import Data.EvalError
import Test.Framework

import Engine.EnginePropFailureConstraintUtils

prop_MultiEmpty  (MultiEmpty  x p fs)    = constraintEmptyCase p "multi"        [x] fs
prop_MeanEmpty   (MeanEmpty   x p fs)    = constraintEmptyCase p "mean"         [x] fs
prop_DescEmpty   (DescEmpty   x p fs)    = constraintEmptyCase p "descriptive"  [x] fs

prop_TableEmtpy1 (TableEmpty1 x y p fs) = constraintEmptyCase p "table"         [x,y] fs
prop_TableEmtpy2 (TableEmpty2 x y p fs) = constraintEmptyCase p "table"         [x,y] fs

prop_SortEmtpy1 (SortEmpty1   x y p fs) = constraintEmptyCase p "sort"          [x,y] fs
prop_SortEmtpy2 (SortEmpty2   x y p fs) = constraintEmptyCase p "sort"          [x,y] fs

prop_ColEmtpy1 (ColEmpty1     x y p fs) = constraintEmptyCase p "col"           [x,y] fs
prop_ColEmtpy2 (ColEmpty2     x y p fs) = constraintEmptyCase p "col"           [x,y] fs

prop_TableColumnLength (TableColumnLength x y p n m fs)   = constraintCase (TableColumnLengthMismatch p n m)   "table" [x,y] fs
prop_TableHeaderLength (TableHeaderLength x y p n m fs)   = constraintCase (TableHeaderLengthMismatch p n m)   "table" [x,y] fs

prop_TableTakeMin      (TableTakeMin      x y p i fs)     = constraintCase (IllegalTakeTableLength   p 1 i)    "take"  [x,y] fs

prop_SortIndexOutOfBounds1 (SortIndexOutOfBounds1 x y p n m i fs) = constraintCase (IndexOutOfBounds    p n m i) "sort"  [x,y] fs
prop_SortIndexOutOfBounds2 (SortIndexOutOfBounds2 x y p n m i fs) = constraintCase (IndexOutOfBounds    p n m i) "sort"  [x,y] fs
prop_ColIndexOutOfBounds1  (ColIndexOutOfBounds1  x y p n m i fs) = constraintCase (IndexOutOfBounds    p n m i) "col"   [x,y] fs
prop_ColIndexOutOfBounds2  (ColIndexOutOfBounds2  x y p n m i fs) = constraintCase (IndexOutOfBounds    p n m i) "col"   [x,y] fs

  