module Eval.MultiPass
( initTable
, derefVars
, validateFunctions
, formVal
, pairVal
, mapPair
, mapMPair
) where

import qualified Data.Map as M (empty,lookup,insert,null,toList,keys,delete,elems)

import Control.Monad           (foldM,liftM,liftM2)
import Data.Eval               (Eval,State,Table)
import Data.EvalError          (EvalError(..))
import Data.ExpToken           (ProgToken(..),FormToken(..),PairToken(..),IdToken(..),ExpToken(..),Pos)

initTable :: ProgToken -> Eval Table
initTable (ProgT _ fs) = foldM f M.empty fs where
  f m (FormT (IdT p _ name) value) = case M.lookup name m of
    Nothing -> Right $ M.insert name (value,p) m
    Just _  -> Left $ MultipleDefinitions p name

derefVars :: Table -> Eval Table
derefVars = flip f M.empty where
 f pending finished = do
  (newPending, newFinished) <- foldM derefVar (pending,finished) $ M.toList pending
  if M.null newPending      then return newFinished       else
   if pending /= newPending then f newPending newFinished else Left $ CycleInDefinitions $ zip (map snd $ M.elems pending) (M.keys pending)

derefVar :: State -> (String,(ExpToken,Pos)) -> Eval State
derefVar (pending,finished) (n,(v,p)) = do
  v' <- deref pending finished v
  return $ uncurry (,) $ if hasAnyVar v' then (M.insert n (v',p) pending,finished) else (M.delete n pending,M.insert n (v',p) finished)

deref :: Table -> Table -> ExpToken -> Eval ExpToken
deref ps fs (FuncT w n es)  = liftM (FuncT w n) $ mapM (deref ps fs) es
deref ps fs (ArrayT p w es) = liftM (ArrayT p w)  $ mapM (deref ps fs) es
deref ps fs (ObjT p w ts)   = liftM (ObjT p w)    $ mapM (mapMPair $ deref ps fs) ts
deref ps fs (VarT i)        = let IdT q _ n = i in case M.lookup n fs of Just (x,_) -> return x; Nothing -> case M.lookup n ps of Just _ -> return $ VarT i; Nothing -> Left $ UndefinedVariable q n
deref _  _  e               = return e

hasAnyVar :: ExpToken -> Bool
hasAnyVar (FuncT _ _ es)  = any hasAnyVar es
hasAnyVar (ArrayT _ _ es) = any hasAnyVar es
hasAnyVar (ObjT _ _ ps)   = any (hasAnyVar.pairVal) ps
hasAnyVar (VarT _)        = True
hasAnyVar _               = False

validateFunctions :: [String] -> Table -> Eval ()
validateFunctions fns t = let table = map (\(a,(b,_))->(a,b)) $ M.toList t in do
  validateNames fns table
  validateNonTopShows table
  validateTopShow table

validateNames :: [String] -> [(String,ExpToken)] -> Eval ()
validateNames fns = mapM_ (uncurry f)   where
  f v (FuncT _ (IdT p _ fn) es) | isValidShow v fn fns = mapM_ (f v) es | otherwise = Left $ UndefinedFunction p fn
  f v (ArrayT _ _ es)                                  = mapM_ (f v) es
  f v (ObjT _ _ ps)                                    = mapM_ (f v.pairVal) ps
  f _ _                                                = return ()

isValidShow :: String -> String -> [String] -> Bool
isValidShow v fn fns = (v == "show" && fn == "show" ) || fn == "show" || fn `elem` fns

validateNonTopShows :: [(String,ExpToken)] -> Eval ()
validateNonTopShows = mapM_ (uncurry top) where
  top v (FuncT _ _ es) = mapM_ (f v) es
  top v e              = f v e
  f v (FuncT _ (IdT p _ fn) es) | fn /= "show" = mapM_ (f v) es | otherwise = Left $ NonTopLevelShow p
  f v (ArrayT _ _ es)                          = mapM_ (f v) es
  f v (ObjT _ _ ps)                            = mapM_ (f v.pairVal) ps
  f _ _                                        = return ()

validateTopShow :: [(String,ExpToken)] -> Eval ()
validateTopShow []                                      = Left NoShow
validateTopShow (("show",FuncT _ (IdT _ _ "show") _):_) = return ()
validateTopShow (_:fs)                                  = validateTopShow fs

formVal :: FormToken -> ExpToken
formVal (FormT _ x) = x

pairVal :: PairToken -> ExpToken
pairVal (PairT _ x) = x

mapPair :: (ExpToken -> ExpToken) -> PairToken -> PairToken
mapPair f (PairT x y) = PairT x (f y)

mapMPair :: Monad m => (ExpToken -> m ExpToken) -> PairToken -> m PairToken
mapMPair f (PairT x y) = liftM2 PairT (return x) (f y)

