module Eval.MultiPass where

import qualified Data.Map as M (empty,lookup,insert,null,toList,keys,delete)

import Control.Monad           (foldM,liftM,liftM3,when,zipWithM)
import Data.Token              (ProgToken(..),FormToken(..),PairToken(..),IdToken(..),ExpToken(..),Pos)
import Data.Eval               (EvalError(..),Eval,State,Table,Obj,TypeValidator,Func)

initTable :: ProgToken -> Eval Table
initTable (ProgT _ fs) = foldM f M.empty fs where 
  f m (FormT _ (IdT _ _ name) value) = case M.lookup name m of
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
deref ps fs (FuncT p w n es)   = liftM (FuncT p w n) $ mapM (deref ps fs) es
deref ps fs (ArrayT p w es)    = liftM (ArrayT p w)  $ mapM (deref ps fs) es             
deref ps fs (ObjT p w ts)      = liftM (ObjT p w)    $ mapM (mapMPair $ deref ps fs) ts
deref ps fs (VarT p i)         = let IdT _ _ n = i in case M.lookup n fs of Just x -> return x; Nothing -> case M.lookup n ps of Just _ -> return $ VarT p i; Nothing -> Left $ UndefinedVariable n
deref _  _  e                  = return e

hasAnyVar :: ExpToken -> Bool
hasAnyVar (FuncT _ _ _ es) = any hasAnyVar es
hasAnyVar (ArrayT _ _ es)  = any hasAnyVar es             
hasAnyVar (ObjT _ _ ps)    = any (hasAnyVar.pairVal) ps
hasAnyVar (VarT _ _)       = True
hasAnyVar _                = False

validateFunctions :: [String] -> Table -> Eval ()
validateFunctions fns t = let table = M.toList t in do
  validateNames fns table
  validateNonTopShows table
  validateTopShow table

validateNames :: [String] -> [(String,ExpToken)] -> Eval ()
validateNames fns = mapM_ (uncurry f)   where
  f v (FuncT _ _ (IdT _ _ fn) es) | isValidShow v fn fns = mapM_ (f v) es | otherwise = Left $ UndefinedFunction v fn
  f v (ArrayT _ _ es)                                    = mapM_ (f v) es
  f v (ObjT _ _ ps)                                      = mapM_ (f v.pairVal) ps
  f _ _                                                  = return ()

isValidShow :: String -> String -> [String] -> Bool
isValidShow v fn fns = (v == "show" && fn == "show" ) || fn == "show" || fn `elem` fns

validateNonTopShows :: [(String,ExpToken)] -> Eval ()
validateNonTopShows = mapM_ (uncurry top) where
  top v (FuncT _ _ _ es) = mapM_ (f v) es
  top v e              = f v e
  f v (FuncT _ _ (IdT _ _ fn) es) | fn /= "show" = mapM_ (f v) es | otherwise = Left $ NonTopLevelShow v
  f v (ArrayT _ _ es)                          = mapM_ (f v) es
  f v (ObjT _ _ ps)                            = mapM_ (f v.pairVal) ps
  f _ _                                        = return ()

validateTopShow :: [(String,ExpToken)] -> Eval ()
validateTopShow []                                        = Left NoShow
validateTopShow (("show",FuncT _ _ (IdT _ _ "show") _):_) = return ()
validateTopShow (_:fs)                                    = validateTopShow fs

applyFunc :: [(String,([TypeValidator],Func))] -> ExpToken -> Eval Obj
applyFunc funcs (FuncT p _ (IdT _ _ i) es) = case lookup i funcs of 
  Nothing -> error $ "Couldn't lookup func ["++i++"]"
  Just (validators,func) -> do
    validateArgsLength p i (length validators) (length es)
    args <- zipWithM ($) validators es
    func args
    
applyFunc _ e = error $ "MultiPass::applyFunc [Unexpected expression in pattern matching ["++show e++"]]"
  
validateArgsLength :: Pos -> String -> Int -> Int -> Eval ()
validateArgsLength p i lv le = when (lv /= le) $ Left $ InvalidNbOfArgs p i lv le 

formVal :: FormToken -> ExpToken
formVal (FormT _ _ x) = x

pairVal :: PairToken -> ExpToken
pairVal (PairT _ _ x) = x

mapPair :: (ExpToken -> ExpToken) -> PairToken -> PairToken
mapPair f (PairT p x y) = PairT p x (f y)

mapMPair :: Monad m => (ExpToken -> m ExpToken) -> PairToken -> m PairToken
mapMPair f (PairT p x y) = liftM3 PairT (return p) (return x) (f y)

