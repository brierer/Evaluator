module Data.EvalError
( EvalError(..)
, TMTree(..)
) where

import Data.ExpToken
import Data.Type

data EvalError = InvalidParse Pos [String]
               | MultipleDefinitions Pos String
               | UndefinedVariable Pos String
               | CycleInDefinitions [(Pos,String)]
               | UndefinedFunction Pos String
               | NonTopLevelShow Pos
               | NoShow
               | ArgCountMismatch Pos String Int Int
               | ArgError Int String EvalError
               | TypeMismatch TMTree
               | IllegalEmpty Pos
               | TableColumnLengthMismatch Pos Int Int
               | TableHeaderLengthMismatch Pos Int Int
               | IllegalTakeTableLength Pos Int Int
               | IndexOutOfBounds Pos Int Int Int
                 deriving (Eq,Show)

data TMTree = TMLeaf Pos TypeTree TypeTree
            | TMNode [TMTree]
              deriving (Eq,Show)
