module Data.Eval
( DerefState
, Table
, Eval
, EvalFunc
, FuncEntry
, Func(..)
) where

import qualified Data.Map as M (Map)

import Control.Monad.State     (StateT)
import Data.EvalError          (EvalError)
import Data.ExpToken           (ExpToken,Pos)
import Data.ExpObj             (ExpObj)
import Data.Type               (Type)

type Table = M.Map String (ExpToken,Pos)
type DerefState = (Table,Table)
type Eval = Either EvalError

type EvalFunc = StateT [FuncEntry] Eval
type FuncEntry = (String,[Type],Func)

newtype Func = Func { call :: Pos -> [ExpObj] -> EvalFunc ExpObj }








