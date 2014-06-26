module Eval.Function where

import Prelude hiding (exp,null)

import Control.Monad       (liftM,liftM2,zipWithM,when)
import Data.Eval           (ExpObj(..),EvalError(..),Eval,FuncEntry,TypeValidator,Func)
import Data.Token          (PairToken(..),IdToken(..),ExpToken(..),Pos)

{-| Function validation and application -}
applyFunc :: [(String,([TypeValidator ExpToken],Func))] -> ExpToken -> Eval ExpObj
applyFunc fs (FuncT p _ (IdT _ _ i) es) = case lookup i fs of 
  Nothing -> error $ "Couldn't lookup func ["++i++"]"
  Just (validators,func) -> validateArgsLength p i (length validators) (length es) >> zipWithM ($) validators es >>= func
applyFunc _ e = error $ "MultiPass::applyFunc [Unexpected expression in pattern matching ["++show e++"]]"

validateArgsLength :: Pos -> String -> Int -> Int -> Eval ()
validateArgsLength p i lv le = when (lv /= le) $ Left $ InvalidNbOfArgs p i lv le 

{-| Validation functions and combinators -}
class Marshallable a where
  exp :: [FuncEntry a] -> TypeValidator a
  exp fs = array fs <|> obj fs <|> str <|> num <|> bool <|> null <|> typeMismatch "Expression"
  
  showable :: TypeValidator a
  table    :: TypeValidator a
  plot     :: TypeValidator a
  matrix   :: TypeValidator a
  array    :: [FuncEntry a] ->TypeValidator a
  obj      :: [FuncEntry a] ->TypeValidator a
  str      :: TypeValidator a
  num      :: TypeValidator a
  bool     :: TypeValidator a
  null     :: TypeValidator a
  
  getP :: a -> Pos
  getT :: a -> String

(<|>) :: TypeValidator a -> TypeValidator a -> TypeValidator a
infixl 3 <|>
(a <|> b) x = case a x of r@(Right _) -> r; Left _ -> b x

instance Marshallable ExpToken where
  showable _ = error "Eval.Function::showable<ExpToken> [Not Implemented]"
  table _    = error "Eval.Function::table<ExpToken> [Not Implemented]"
  plot _     = error "Eval.Function::plot<ExpToken> [Not Implemented]"
  matrix _   = error "Eval.Function::matrix<ExpToken> [Not Implemented]"

  array fs (ArrayT p _ es)  = liftM (ArrayO p) $ mapM (exp fs) es;   array _ e = typeMismatch (head types) e
  obj fs   (ObjT p _ ps)    = liftM (ObjO p) $ mapM (toTuple fs) ps; obj _ e   = typeMismatch (types !! 1) e
  str      (StrT p _ s)     = return $ StrO p s;                     str e     = typeMismatch (types !! 2) e
  num      (NumT p _ _ n)   = return $ NumO p n;                     num e     = typeMismatch (types !! 3) e
  bool     (BoolT p _ b)    = return $ BoolO p b;                    bool e    = typeMismatch (types !! 4) e
  null     (NullT p _)      = return $ NullO p;                      null e    = typeMismatch (types !! 5) e
  
  getP (ArrayT p _ _) = p
  getP (ObjT p _ _)   = p
  getP (StrT p _ _)   = p
  getP (NumT p _ _ _) = p
  getP (BoolT p _ _)  = p
  getP (NullT p _)    = p
  getP e              = error $ "Eval.Function::getP<ExpToken> [Failed pattern match ["++show e++"]]"
  
  getT (ArrayT{}) = head types
  getT (ObjT{})   = types !! 1
  getT (StrT{})   = types !! 2
  getT (NumT{})   = types !! 3
  getT (BoolT{})  = types !! 4
  getT (NullT{})  = types !! 5
  getT e          = error $ "Eval.Function::getT<ExpToken> [Failed pattern match ["++show e++"]]"

instance Marshallable ExpObj where
  showable _ = error "Eval.Function::showable<Obj> [Not Implemented]"
  table _    = error "Eval.Function::table<Obj> [Not Implemented]"
  plot _     = error "Eval.Function::plot<Obj> [Not Implemented]"
  matrix _   = error "Eval.Function::matrix<Obj> [Not Implemented]"

  array _ x@(ArrayO{}) = return x; array _ e = typeMismatch (head types) e
  obj _   x@(ObjO{})   = return x; obj _ e   = typeMismatch (types !! 1) e  
  str     x@(StrO{})   = return x; str e     = typeMismatch (types !! 2) e
  num     x@(NumO{})   = return x; num e     = typeMismatch (types !! 3) e
  bool    x@(BoolO{})  = return x; bool e    = typeMismatch (types !! 4) e
  null    x@(NullO{})  = return x; null e    = typeMismatch (types !! 5) e
  
  getP (ArrayO p _) = p
  getP (ObjO p _)   = p
  getP (StrO p _)   = p
  getP (NumO p _)   = p
  getP (BoolO p _)  = p
  getP (NullO p)    = p
--  getP e              = error $ "Eval.Function::getP<Obj> [Failed pattern match ["++show e++"]]"
  
  getT (ArrayO{}) = head types
  getT (ObjO{})   = types !! 1
  getT (StrO{})   = types !! 2
  getT (NumO{})   = types !! 3
  getT (BoolO{})  = types !! 4
  getT (NullO{})  = types !! 5
--  getT e          = error $ "Eval.Function::getT<Obj> [Failed pattern match ["++show e++"]]"


typeMismatch :: Marshallable a => String -> TypeValidator a
typeMismatch expected e = Left $ TypeMismatch (getP e) expected (getT e)

types :: [String]
types = ["Array","Object","String","Number","Boolean","Null"]

toTuple :: [FuncEntry ExpToken] -> PairToken -> Eval (String,ExpObj)
toTuple fs (PairT _ (IdT _ _ x) y) = liftM2 (,) (return x) (exp fs y)
