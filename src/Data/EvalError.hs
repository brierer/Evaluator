module Data.EvalError
( EvalError(..)
) where

import Data.ExpToken (Pos)
import Data.ExpObj   (Type)

data EvalError = MultipleDefinitions Pos String
               | UndefinedVariable Pos String
               | CycleInDefinitions [(Pos,String)]
               | UndefinedFunction Pos String
               | NonTopLevelShow Pos
               | NoShow
               | InvalidNbOfArgs Pos String Int Int
               | ArgError Int String EvalError
               | TypeMismatch Pos Type Type
               | IllegalEmpty Pos
               | TableColumnLengthMismatch Pos Int Int
               | TableHeaderLengthMismatch Pos Int Int
               | IndexOutOfBounds Pos Int Int Int
                 deriving (Eq,Show)

