module Eval.Function
( Marshallable(..)
, table, plot, array, obj, str, num, bool, null
, any,noLit,lit
, anyType, noLitType,litType
, arrayOf, objOf, nonEmpty, args
, (<|>), (<!>)
, withFuncs
, evalError
) where

import Prelude hiding         (any,null)

import qualified Prelude as P (any,null)

import Control.Monad.State    (evalStateT,get,lift)
import Control.Monad          (liftM,liftM2,zipWithM,when,(>=>))
import Data.Eval              (EvalError(..),Type(..),ExpObj(..),EvalError(..),Eval,FuncEntry,EvalFunc,TypeValidator(..),Func(..))
import Data.Token             (PairToken(..),IdToken(..),ExpToken(..),Pos)

class Marshallable a where
  marshall :: a -> EvalFunc ExpObj
  getPos   :: a -> Pos
  getType  :: a -> Type

instance Marshallable ExpToken where
  marshall (FuncT _ (IdT p _ i) es) = applyFunc p i es
  marshall (ArrayT p _ es)          = liftM (ArrayO p) $ mapM marshall es
  marshall (ObjT p _ ps)            = liftM (ObjO p)   $ mapM f        ps where f (PairT (IdT _ _ x) y) = liftM2 (,) (return x) (marshall y)
  marshall (StrT p _ s)             = return $ StrO p s
  marshall (NumT p _ _ n)           = return $ NumO p n
  marshall (BoolT p _ b)            = return $ BoolO p b
  marshall (NullT p _)              = return $ NullO p
  marshall e                        = error $ "Eval.Function::marshall<ExpToken> [Unexpected pattern ["++show e++"]]"

  getPos (ArrayT p _ _)          = p
  getPos (ObjT p _ _)            = p
  getPos (StrT p _ _)            = p
  getPos (NumT p _ _ _)          = p
  getPos (BoolT p _ _)           = p
  getPos (NullT p _)             = p
  getPos e                       = error $ "Eval.Function::getPos<ExpToken> [Unexpected pattern ["++show e++"]]"

  getType (ArrayT{}) = Arr
  getType (ObjT{})   = Obj
  getType (StrT{})   = Str
  getType (NumT{})   = Num
  getType (BoolT{})  = Bool
  getType (NullT{})  = Null
  getType e          = error $ "Eval.Function::getType<ExpToken> [Unexpected pattern ["++show e++"]]"

instance Marshallable ExpObj where
  marshall = return

  getPos (TableO p _ _) = p
  getPos (PlotO  p _ _) = p
  getPos (ArrayO p _)   = p
  getPos (ObjO p _)     = p
  getPos (StrO p _)     = p
  getPos (NumO p _)     = p
  getPos (BoolO p _)    = p
  getPos (NullO p)      = p

  getType (TableO{}) = Table
  getType (PlotO{})  = Plot
  getType (ArrayO{}) = Arr
  getType (ObjO{})   = Obj
  getType (StrO{})   = Str
  getType (NumO{})   = Num
  getType (BoolO{})  = Bool
  getType (NullO{})  = Null

{-| Type validators -}
match :: Type -> TypeValidator
match t = TypeVal $ \e -> if t == getType e  then return e else runValidation (mismatch t) e

mismatch :: Type -> TypeValidator
mismatch t = TypeVal $ \e -> evalError $ TypeMismatch (getPos e) t (getType e)


table :: TypeValidator
plot  :: TypeValidator
array :: TypeValidator
obj   :: TypeValidator
str   :: TypeValidator
num   :: TypeValidator
bool  :: TypeValidator
null  :: TypeValidator

table = match Table
plot  = match Plot
array = arrayOf any
obj   = objOf   any
str   = match Str
num   = match Num
bool  = match Bool
null  = match Null

any   :: TypeValidator
lit   :: TypeValidator
noLit :: TypeValidator

any   = table <|> plot <|> array <|> obj <|> str <|> num <|> bool <|> null <!> anyType
noLit = table <|> plot                                                     <!> noLitType
lit   =                    array <|> obj <|> str <|> num <|> bool <|> null <!> litType

anyType   :: Type
noLitType :: Type
litType   :: Type

anyType   = foldl1 Or [noLitType,litType]
noLitType = foldl1 Or [Table,Plot]
litType   = foldl1 Or [Arr,Obj,Str,Num,Bool,Null]

arrayOf :: TypeValidator -> TypeValidator
arrayOf v = TypeVal (runValidation (match Arr) >=> \(ArrayO p es) -> liftM (ArrayO p) $ mapM (runValidation v) es)

objOf :: TypeValidator -> TypeValidator
objOf v = TypeVal (runValidation (match Obj) >=> \(ObjO p ps) -> liftM (ObjO p) $ mapM (\(x,y) -> liftM2 (,) (return x) $ runValidation v y) ps)

nonEmpty :: TypeValidator -> TypeValidator
nonEmpty v = let raise = evalError .IllegalEmpty in TypeVal $ runValidation v >=> \ x -> case x of
  TableO p [] _ -> raise p; TableO p xs _ -> if not (P.any (not . P.null) xs) then raise p else return x
  PlotO  p [] _ -> raise p;
  ArrayO p [] -> raise p;
  ObjO   p [] -> raise p;
  StrO   p [] -> raise p;
  _ -> return x

args :: FuncEntry -> FuncEntry
args (n,(vs,f)) = (n,(zipWith arg [0..] vs,f)) where
  arg i (TypeVal v) = TypeVal $ \x -> do st <- get; case evalStateT (v x) st of Left e -> evalError $ ArgError i n e; r -> lift r

(<|>) :: TypeValidator -> TypeValidator -> TypeValidator
(<|>) (TypeVal a) (TypeVal b) = TypeVal $ \x -> do st <- get; case evalStateT (a x) st of Right r -> return r; Left _ -> b x
infixl 3 <|>

infixl 3 <!>
(<!>) :: TypeValidator -> Type -> TypeValidator
(<!>) v = (<|>) v . mismatch

withFuncs :: Marshallable a => [FuncEntry] -> TypeValidator -> a -> Eval ExpObj
withFuncs fs v e = evalStateT (marshall e >>= runValidation v) fs

{-| Privates -}
applyFunc :: Pos -> String -> [ExpToken] -> EvalFunc ExpObj
applyFunc p i es = get >>= \fs -> case lookup i fs of
    Nothing -> error $ "Eval.Function::applyFunc [Could not lookup func ["++i++"]]"
    Just (vs,Func f) -> validArgCount p i (length vs) (length es) >>
                        mapM marshall es >>= zipWithM runValidation vs >>= f p

validArgCount :: Pos -> String -> Int -> Int -> EvalFunc ()
validArgCount p i lv le = when (lv /= le) $ evalError $ InvalidNbOfArgs p i lv le

evalError :: EvalError -> EvalFunc a
evalError = lift.Left


