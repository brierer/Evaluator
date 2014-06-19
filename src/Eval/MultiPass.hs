module Eval.MultiPass where

import qualified Data.Map as M (Map,empty,lookup,insert,null,toList,keys,delete)
--
import Data.Token              (ProgToken(..),FormToken(..),PairToken(..),ExpToken(..))
import Control.Monad           (foldM,liftM,liftM2)

data EvalError = MultipleDefinitions String
               | UndefinedVariable String
               | CycleInVariables [String]
                 deriving (Eq,Show)

type State = (Table,Table)
type Table = M.Map String ExpToken
type Eval = Either EvalError

initTable :: ProgToken -> Eval Table
initTable (ProgT fs) = foldM f M.empty fs where 
  f m (FormT name value) = case M.lookup name m of
    Nothing -> Right $ M.insert name value m
    Just _  -> Left $ MultipleDefinitions name

derefVars :: Table -> Eval Table
derefVars = flip f M.empty where
 f pending finished = do
  (newPending, newFinished) <- foldM derefVar (pending,finished) $ M.toList pending
  if M.null newPending      then return newFinished       else 
   if pending /= newPending then f newPending newFinished else Left $ CycleInVariables $ M.keys pending

derefVar :: State -> (String,ExpToken) -> Eval State
derefVar (pending,finished) (n,v) = do
  v' <- deref pending finished v
  return $ uncurry (,) $ if hasAnyVar v' then (M.insert n v' pending,finished) else (M.delete n pending,M.insert n v' finished)

deref :: Table -> Table -> ExpToken -> Eval ExpToken
deref ps fs (FuncT w n es) = liftM (FuncT w n) $ mapM (deref ps fs) es
deref ps fs (ArrayT w es)  = liftM (ArrayT w)  $ mapM (deref ps fs) es             
deref ps fs (ObjT w ts)    = liftM (ObjT w)    $ mapM (\(PairT x y) -> liftM2 PairT (return x) $ deref ps fs y) ts
deref ps fs (VarT w n)     = case M.lookup n fs of Just x -> return x; Nothing -> case M.lookup n ps of Just _ -> return $ VarT w n; Nothing -> Left $ UndefinedVariable n
deref _  _  e            = return e

hasAnyVar :: ExpToken -> Bool
hasAnyVar (FuncT _ _ es) = any hasAnyVar es
hasAnyVar (ArrayT _ es)  = any hasAnyVar es             
hasAnyVar (ObjT _ ps)    = any (\(PairT _ e) -> hasAnyVar e) ps
hasAnyVar (VarT _ _)     = True
hasAnyVar _            = False