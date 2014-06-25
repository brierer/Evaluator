module Data.Eval where

import qualified Data.Map as M (Map)

import Data.Token              (ExpToken,Pos)

data EvalError = MultipleDefinitions String
               | UndefinedVariable String
               | CycleInVariables [String]
               | UndefinedFunction String String
               | NonTopLevelShow String
               | NoShow
               | InvalidNbOfArgs Pos String Int Int
               | TypeMismatch Pos String String
                 deriving (Eq,Show)

type State = (Table,Table)
type Table = M.Map String ExpToken
type Eval = Either EvalError

data Obj = ArrayObj [Obj]
         | ObjObj [(String,Obj)]
         | StrObj String
         | NumObj Double
         | BoolObj Bool
         | NullObj
           deriving (Eq,Show)

type TypeValidator = ExpToken -> Eval Obj
type Func = [Obj] -> Eval Obj