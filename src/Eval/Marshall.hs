module Eval.Marshall
( marshallWith
, marshall
) where

import Prelude hiding (any)

import qualified Prelude  as P

import Control.Monad.State
import Data.Eval
import Data.EvalError
import Data.ExpObj
import Data.ExpToken
import Eval.MatchType
import Data.Map as M
import Data.Maybe

marshallWith :: Table -> ExpToken -> [FuncEntry] -> Eval ExpObj
marshallWith d x = evalStateT $ marshall d x

marshall :: Table -> ExpToken -> EvalFunc ExpObj
marshall d (FuncT _ (IdT p _ i) es) = applyFunc d p i es
marshall d (ArrT p _ es)            = liftM (ArrO $ Calc p) $ mapM (marshall d) es
marshall d (ObjT p _ ps)            = liftM (ObjO $ Calc p) $ mapM f        ps where f (PairT (IdT _ _ x) y) = liftM2 (,) (return x) (marshall d y)
marshall d (StrT p _ s)             = return $ StrO  (Upd p) s
marshall d (NumT p _ _ n)           = return $ NumO  (Upd p) n
marshall d (BoolT p _ b)            = return $ BoolO (Upd p) b
marshall d (NullT p _)              = return $ NullO (Upd p)
marshall d (VarT (IdT p w s))       = marshall  d  ( fromJust $ ( fmap fst  $ M.lookup s d))
marshall d e                        = error $ "Eval.Marshall::marshall [Unexpected pattern ["++show e++"]]"

applyFunc :: Table -> Pos -> String -> [ExpToken] -> EvalFunc ExpObj
applyFunc d p i es = do fs <- get; case lookup' fs of {
  Nothing -> error $ "Eval.Marshall::applyFunc [Could not lookup function ["++i++"]]";
  Just (ts,func) -> validateArgCount i (length ts) (length es) >> mapM (marshall d) es >>= zipWithM ($) (P.map matchType ts) >>= invoke func (Calc p) }
--
  where lookup' = P.lookup i .(P.map) (\(x,y,z)->(x,(y,z)))
        validateArgCount s l1 l2 = when (l1 /= l2) $ lift $ Left $ ArgCountMismatch p s l1 l2

