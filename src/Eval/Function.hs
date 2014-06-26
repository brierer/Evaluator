module Eval.Function
( Marshallable(..)
, applyFunc
, types
) where

import Prelude hiding      (any,null)

import Control.Monad       (liftM,liftM2,zipWithM,when)
import Data.Eval           (Type(..),ExpObj(..),EvalError(..),Eval,FuncEntry,TypeValidator,Func)
import Data.Token          (PairToken(..),IdToken(..),ExpToken(..),Pos)

class Marshallable a where
  any :: [FuncEntry a] -> TypeValidator a
  any fs = array fs <|> obj fs <|> str <|> num <|> bool <|> null
  
  table    :: [FuncEntry a] -> TypeValidator a
  plot     :: [FuncEntry a] -> TypeValidator a
  array    :: [FuncEntry a] -> TypeValidator a
  obj      :: [FuncEntry a] -> TypeValidator a
  str      :: TypeValidator a
  num      :: TypeValidator a
  bool     :: TypeValidator a
  null     :: TypeValidator a
  
  getP :: a -> Pos
  getT :: a -> Type

instance Marshallable ExpToken where
  table _    = error "Eval.Function::table<ExpToken> [Should not be called]"
  plot _     = error "Eval.Function::plot<ExpToken>  [Should not be called]"

  array fs (ArrayT p _ es)  = liftM (ArrayO p) $ mapM (any fs)     es; array _ e = typeMismatch Array   e
  obj fs   (ObjT p _ ps)    = liftM (ObjO p)   $ mapM (toTuple fs) ps; obj _ e   = typeMismatch Object  e
  str      (StrT p _ s)     = return $ StrO p s;                       str e     = typeMismatch String  e
  num      (NumT p _ _ n)   = return $ NumO p n;                       num e     = typeMismatch Number  e
  bool     (BoolT p _ b)    = return $ BoolO p b;                      bool e    = typeMismatch Boolean e
  null     (NullT p _)      = return $ NullO p;                        null e    = typeMismatch Null    e
  
  getP (ArrayT p _ _) = p
  getP (ObjT p _ _)   = p
  getP (StrT p _ _)   = p
  getP (NumT p _ _ _) = p
  getP (BoolT p _ _)  = p
  getP (NullT p _)    = p
  getP e              = error $ "Eval.Function::getP<ExpToken> [Failed pattern match ["++show e++"]]"
  
  getT (ArrayT{})     = Array  
  getT (ObjT{})       = Object 
  getT (StrT{})       = String 
  getT (NumT{})       = Number 
  getT (BoolT{})      = Boolean
  getT (NullT{})      = Null   
  getT e              = error $ "8val.Function::getT<ExpToken> [Failed pattern match ["++show e++"]]"

instance Marshallable ExpObj where
  table _ x@(TableO{}) = return x; table _ e = typeMismatch Table   e
  plot _  x@(PlotO{})  = return x; plot _ e  = typeMismatch Plot    e
  array _ x@(ArrayO{}) = return x; array _ e = typeMismatch Array   e
  obj _   x@(ObjO{})   = return x; obj _ e   = typeMismatch Object  e
  str     x@(StrO{})   = return x; str e     = typeMismatch String  e
  num     x@(NumO{})   = return x; num e     = typeMismatch Number  e
  bool    x@(BoolO{})  = return x; bool e    = typeMismatch Boolean e
  null    x@(NullO{})  = return x; null e    = typeMismatch Null    e
  
  getP (TableO p _ _)  = p
  getP (PlotO p _ _ _) = p
  getP (ArrayO p _)    = p
  getP (ObjO p _)      = p
  getP (StrO p _)      = p
  getP (NumO p _)      = p
  getP (BoolO p _)     = p
  getP (NullO p)       = p
  
  getT (TableO{})  = Table
  getT (PlotO{})   = Plot
  getT (ArrayO{})  = Array
  getT (ObjO{})    = Object
  getT (StrO{})    = String
  getT (NumO{})    = Number
  getT (BoolO{})   = Boolean
  getT (NullO{})   = Null

(<|>) :: TypeValidator a -> TypeValidator a -> TypeValidator a
infixl 3 <|>
(a <|> b) x = case a x of r@(Right _) -> r; Left _ -> b x

{-| Function validation and application -}
applyFunc :: [(String,([TypeValidator ExpToken],Func))] -> ExpToken -> Eval ExpObj
applyFunc fs (FuncT p _ (IdT _ _ i) es) = apply fs p i es
applyFunc _ e                           = error $ "MultiPass::applyFunc [Failed pattern match ["++show e++"]]"

apply :: [(String,([TypeValidator ExpToken],Func))] -> Pos -> String -> [ExpToken] -> Eval ExpObj
apply fs p i es = case lookup i fs of 
  Nothing -> error $ "Could not lookup func ["++i++"]"
  Just (validators,func) -> validateArgsLength p i (length validators) (length es) >> zipWithM ($) validators es >>= func

validateArgsLength :: Pos -> String -> Int -> Int -> Eval ()
validateArgsLength p i lv le = when (lv /= le) $ Left $ InvalidNbOfArgs p i lv le 

typeMismatch :: Marshallable a => Type -> TypeValidator a
typeMismatch expected e = Left $ TypeMismatch (getP e) expected (getT e)

types :: [Type]
types = [Table,Plot,Array,Object,String,Number,Boolean,Null]

toTuple :: [FuncEntry ExpToken] -> PairToken -> Eval (String,ExpObj)
toTuple fs (PairT _ (IdT _ _ x) y) = liftM2 (,) (return x) (any fs y)
