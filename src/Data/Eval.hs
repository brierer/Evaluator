module Data.Eval where

import qualified Data.Map as M (Map)

import Data.Token              (ExpToken,Pos)

data EvalError = MultipleDefinitions Pos String
               | UndefinedVariable String
               | CycleInDefinitions [(Pos,String)]
               | UndefinedFunction Pos String
               | NonTopLevelShow Pos
               | NoShow
               | InvalidNbOfArgs Pos String Int Int
               | TypeMismatch Pos String String
                 deriving (Eq,Show)

type State = (Table,Table)
type Table = M.Map String (ExpToken,Pos)
type Eval = Either EvalError

data ExpObj = ArrayO Pos [ExpObj]
            | ObjO   Pos [(String,ExpObj)]
            | StrO   Pos String
            | NumO   Pos Double
            | BoolO  Pos Bool
            | NullO  Pos
              deriving (Eq,Show)

type TypeValidator a = a -> Eval ExpObj
type Func = [ExpObj] -> Eval ExpObj