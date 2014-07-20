module Data.EvalError
( EvalError(..)
) where

import Data.ExpToken (Pos)
import Data.Type     (TypeTree)

data EvalError = MultipleDefinitions Pos String
               | UndefinedVariable Pos String
               | CycleInDefinitions [(Pos,String)]
               | UndefinedFunction Pos String
               | NonTopLevelShow Pos
               | NoShow
               | ArgCountMismatch Pos String Int Int
               | ArgError Int String EvalError
               | TypeMismatch Pos TypeTree TypeTree
               | IllegalEmpty Pos
               | TableColumnLengthMismatch Pos Int Int
               | TableHeaderLengthMismatch Pos Int Int
               | IndexOutOfBounds Pos Int Int Int
                 deriving (Eq,Show)

