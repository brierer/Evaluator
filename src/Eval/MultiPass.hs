module Eval.MultiPass where

import Control.Monad
import qualified Data.Map as M

import Data.Token

data EvalError = MultipleDefinitions String
               | UndefinedVariable String
               | CycleInVariables [String]
                 deriving (Eq,Show)

data State = State Table Table              deriving (Eq,Show)

type Table = M.Map String ExpToken
type Eval = Either EvalError

initState :: ProgToken -> Eval State
initState (ProgT fs) = liftM (flip State M.empty) $ foldM f M.empty fs
  where f :: Table -> FormToken -> Eval Table
        f m (FormT name value) = case M.lookup name m of
                                  Nothing -> Right $ M.insert name value m
                                  Just _  -> Left $ MultipleDefinitions name

derefVars :: State -> Eval State
derefVars s@(State pending _) = do
  s'@(State newPending _) <- foldM derefVar s $ M.toList pending
  if M.null newPending 
    then return s'
    else if pending == newPending
      then Left $ CycleInVariables $ M.keys pending
      else derefVars s' 

derefVar :: State -> (String,ExpToken) -> Eval State
derefVar (State pending finished) (n,v) = do
   v' <- deref pending finished v
   return $ uncurry State $ if hasAnyVar v' then (M.insert n v' pending,finished) else (M.delete n pending,M.insert n v' finished)

deref :: Table -> Table -> ExpToken -> Eval ExpToken
deref ps fs (FuncT n es) = liftM (FuncT n) $ mapM (deref ps fs) es
deref ps fs (ArrayT es)  = liftM ArrayT    $ mapM (deref ps fs) es             
deref ps fs (ObjT ts)    = liftM ObjT      $ mapM (\(PairT x y) -> liftM2 PairT (return x) $ deref ps fs y) ts
deref ps fs (VarT n)     = case M.lookup n fs of Just x -> return x; Nothing -> case M.lookup n ps of Just _ -> return $ VarT n; Nothing -> Left $ UndefinedVariable n
deref _  _  e            = return e

hasAnyVar :: ExpToken -> Bool
hasAnyVar (FuncT _ es) = any hasAnyVar es
hasAnyVar (ArrayT es)  = any hasAnyVar es             
hasAnyVar (ObjT ps)    = any (\(PairT _ e) -> hasAnyVar e) ps
hasAnyVar (VarT _)     = True
hasAnyVar _            = False