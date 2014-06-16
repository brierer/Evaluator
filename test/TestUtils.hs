{-# OPTIONS_GHC -fno-warn-orphans #-}
module TestUtils where

import Test.Framework
import Control.Arrow
import Control.Monad
import Data.List
import Data.Char
import Data.Maybe
import Data.Function
import qualified Data.Map as M
import qualified Data.Set as S

import Text.ParserCombinators.Parsec hiding (alphaNum)
import Text.ParserCombinators.Parsec.Error

import Data.Token

{-# ANN module "HLint: ignore Use camelCase" #-}
{-# ANN module "HLint: ignore Redundant do" #-}

{-| Types -}
newtype StrTA = StrTA String      deriving Show
newtype IdA   = IdA   String      deriving Show
data    Pair  = Pair IdA ExpToken deriving Show

data UniqueDefs = UniqueDefs ProgToken       deriving (Eq,Show)
data NonEmptyUniqueDefs = NonEmptyUniqueDefs UniqueDefs deriving (Show)
data MultiDefs  = MultiDefs ProgToken String deriving (Eq,Show)

data ValidProg = ValidProg UniqueDefs         deriving (Eq,Show)
data UndefProg = UndefProg ValidProg String   deriving (Eq,Show)
data CycleProg = CycleProg ValidProg [String] deriving (Eq,Show)

{-| Instances -}
instance Eq ParseError where (==) a b = errorMessages a == errorMessages b

instance Arbitrary StrTA where arbitrary = liftM StrTA $ mListOf $ elements $ [' '..'~'] \\ "\"\\"
                               shrink (StrTA s) = map StrTA $ filter validStr $ shrink s

instance Arbitrary IdA   where arbitrary = liftM IdA   $ liftM2 (:) (elements alpha) $ mListOf $ elements alphaNum
                               shrink (IdA s) = map IdA $ filter validId $ shrink s

instance Arbitrary Pair  where arbitrary = liftM2 Pair arbitrary (arb 1)
                               shrink (Pair idA tok) = [Pair idA' tok' | idA' <- shrink idA, tok' <- shrink tok]

instance Arbitrary ProgToken where arbitrary = liftM ProgT (sListOf arbitrary)
                                   shrink (ProgT fs) = map ProgT $ shrink fs

instance Arbitrary FormToken where arbitrary = liftM2 FormT (liftM unId arbitrary) (arb 1)
                                   shrink (FormT idA e) = [FormT (unId idA') e | idA' <- shrink (IdA idA)] ++ [FormT idA e' | e' <- shrink e]

instance Arbitrary ExpToken where arbitrary = arb 1; shrink = shrinkTok

instance Arbitrary UniqueDefs where
  arbitrary = liftM (fromForms.uniqueForms) arbitrary
  shrink (UniqueDefs prog) = map (fromForms.uniqueForms.forms) $ shrink prog

instance Arbitrary NonEmptyUniqueDefs where
  arbitrary = do NonEmpty fs <- arbitrary; return $ fromForms $ uniqueForms fs
  shrink (NonEmptyUniqueDefs prog) = map fromForms $ filter (not.null) $ map (uniqueForms.forms) $ shrink prog

instance Arbitrary MultiDefs where
  arbitrary = do NonEmptyUniqueDefs prog <- arbitrary; let fs = forms prog in return $ MultiDefs (fromForms $ concat $ replicate 5 fs) $ formName $ head fs

instance Arbitrary ValidProg where
  arbitrary = do UniqueDefs prog <- arbitrary; liftM fromForms $ replaceAllVariables elements $ forms prog
  shrink (ValidProg prog) = concatMap (map fromForms . replaceAllVariables id . forms) $ shrink prog

instance Arbitrary UndefProg where
  arbitrary = mUndefProg arbitrary elements     
  shrink (UndefProg p _) = mUndefProg (shrink p) id

instance Arbitrary CycleProg where
  arbitrary = mCycleProg arbitrary elements
  shrink (CycleProg p _) = mCycleProg (shrink p) id

class Parseable a where parseable :: a -> [String]
instance Parseable IdA where parseable (IdA s) = [s]
instance Parseable Pair where parseable (Pair (IdA s) tok) = parseable (IdA s) ++ [":"] ++ parseable tok
instance Parseable ProgToken where parseable (ProgT fs)    = parseables' "\n" fs
instance Parseable FormToken where parseable (FormT s e)   = parseable (IdA s) ++ ["="] ++ parseable e
instance Parseable ExpToken where parseable = parseableTok

parseables :: Parseable a => [a] -> [String]
parseables  = parseables' ","
parseables' sep xs = intercalate [sep] $ map parseable xs 

arb :: Integer -> Gen ExpToken
arb 0 = join $ elements [                                 arbVar, arbString, arbNum, arbBool, arbNull]
arb d = join $ elements [arbFunc d, arbArray d, arbObj d, arbVar, arbString, arbNum, arbBool, arbNull]

arbFunc  d = liftM2 FuncT (liftM unId arbitrary) $ sListOf $ arb $ d-1
arbArray d = liftM ArrayT $ sListOf $ arb $ d-1
arbObj   d = do
  ss <- sListOf arbitrary
  es <- sListOf (arb $ d-1)
  return $ ObjT .map unPair $ zipWith Pair ss es

arbVar     = liftM (VarT . unId) arbitrary
arbString  = liftM (StrT . unStr) arbitrary
arbNum     = liftM NumT arbitrary
arbBool    = liftM BoolT $ elements [True, False]
arbNull    = return NullT

shrinkTok (FuncT s es)  = [FuncT s' es' | (IdA s') <- shrink (IdA s), es' <- shrink es] ++ [ArrayT es]
shrinkTok (ArrayT es)   = map ArrayT (shrink es) 
shrinkTok (ObjT ps)     = map (ObjT . map unPair) (shrink $ map toPair ps)
shrinkTok (VarT v)      = map (VarT  .unId) (shrink (IdA v))
shrinkTok (StrT s)      = map StrT $ shrink s
shrinkTok (NumT x)      = map NumT $ shrink x
shrinkTok (BoolT b)     = map BoolT $ shrink b
shrinkTok NullT         = []

parseableTok (FuncT i es) = parseable (IdA i) ++ ["("] ++ parseables es ++ [")"]
parseableTok (ArrayT es)  = ["["] ++ parseables es ++ ["]"]
parseableTok (ObjT ps)    = ["{"] ++ parseables (map toPair ps) ++ ["}"]
parseableTok (VarT v)     = parseable (IdA v)
parseableTok (StrT s)     = [show s]
parseableTok (NumT x)     = [show x]
parseableTok (BoolT b)    = [map toLower $ show b]
parseableTok NullT        = ["null"]

class HasProg a where
  forms :: a -> [FormToken]
  fromForms :: [FormToken] -> a
  
  toToken :: a -> ProgToken
  toToken = fromForms.forms

instance HasProg ProgToken where
  forms (ProgT fs) = fs
  fromForms = ProgT

instance HasProg UniqueDefs where
  forms (UniqueDefs prog) = forms prog
  fromForms = UniqueDefs .fromForms
  
instance HasProg NonEmptyUniqueDefs where
  forms (NonEmptyUniqueDefs prog) = forms prog
  fromForms = NonEmptyUniqueDefs .fromForms
  
instance HasProg ValidProg where
  forms (ValidProg prog) = forms prog
  fromForms = ValidProg .fromForms

{-| Functions -}
parseCase x p ss = all ((Right x ==). parse p "") $ cases ss
cases ss = [concat ss, unwords ss, " " ++ concat ss ++ " ", " " ++ unwords ss ++ " "]

alpha = ['a'..'z']++['A'..'Z']
alphaNum = alpha ++ ['0'..'9']

toPair (s,e) = Pair (IdA s) e
unPair (Pair (IdA s) e) = (s,e)

unId (IdA s) = s
unStr (StrTA s) = s

validId s = not (null s) && head s `elem` alpha && null (s \\ alphaNum)
validStr s =   null $ s \\ ([' '..'~'] \\ "\"\\")

mListOf = liftM (take 10) . listOf
sListOf = liftM (take 5) . listOf

toTuple (FormT a b) = (a,b)

fromProgForms :: HasProg a => a -> M.Map String ExpToken
fromProgForms = M.fromList.map toTuple.forms
formName (FormT a _) = a

derefValidProg :: HasProg a => a -> a
derefValidProg = fromForms.derefAll.forms

derefAll :: [FormToken] -> [FormToken]
derefAll []             = []
derefAll (FormT n v:fs) = let moo1 = FormT n (moo fs v):derefAll fs
                          in  if any hasVarF moo1 then derefAll moo1 else moo1

hasVarF (FormT _ e) = hasVar e
hasVar (FuncT _ es) = any hasVar es
hasVar (ArrayT es)  = any hasVar es             
hasVar (ObjT ps)    = any (hasVar.snd) ps
hasVar (VarT _)     = True
hasVar _            = False

moo fs (FuncT m es) = FuncT  m $ map (moo fs) es
moo fs (ArrayT es)  = ArrayT   $ map (moo fs) es
moo fs (ObjT ps)    = ObjT     $ map (second $ moo fs) ps
moo fs (VarT m)     = fromMaybe (error $ "Couldn't find var ["++m++"] in prog ["++show fs++"]") $ lookup m $ map toTuple fs
moo _  e            = e

uniqueForms = nubBy ((==) `on` formName)



mCycleProg a f = do
 ValidProg prog <- a
 let fs = filter hasVarF $ forms prog
     ns = map formName fs
 if null fs then return $ CycleProg (fromForms []) [] else do
 fs' <- zipWithM (makeCycle f) fs $ tail $ cycle ns
 return $ CycleProg (fromForms fs') (sort ns)

makeCycle f (FormT n e) m = liftM (FormT n) $ replaceVariables [m] f e

replaceAllVariables :: Monad m => ([String] -> m String) -> [FormToken] -> m [FormToken]
replaceAllVariables _ []  = return []
replaceAllVariables f (FormT n v:fs) = do
  v' <- replaceVariables (map formName fs) f v
  rest <- replaceAllVariables f fs
  return $ FormT n v': rest

replaceVariables :: Monad m => [String] -> ([String] -> m String) -> ExpToken -> m ExpToken
replaceVariables ns f (FuncT n es) = liftM (FuncT n) $ mapM (replaceVariables ns f) es
replaceVariables ns f (ArrayT es)  = liftM ArrayT    $ mapM (replaceVariables ns f) es
replaceVariables ns f (ObjT ps)    = liftM ObjT      $ mapM (\(x,y) -> liftM2 (,) (return x) $ replaceVariables ns f y) ps
replaceVariables [] _ (VarT _)     = return NullT
replaceVariables ns f (VarT _)     = liftM VarT $ f ns
replaceVariables _  _ e            = return e

mUndefProg a1 a2 = do
  ValidProg prog <- a1
  let fs = forms prog
      ns = getRefed fs
  if null ns then return $ UndefProg (fromForms []) "" else do
  toRemove <- a2 ns
  let fs' = filter ((/=toRemove).formName) fs
  return $ UndefProg (fromForms fs') toRemove

getRefed = S.toList . f S.empty
  where f acc []        = acc
        f acc (FormT _ e:fs) = f (g acc e) fs
        g acc (FuncT _ es) = foldl g acc es
        g acc (ArrayT es)  = foldl g acc es
        g acc (ObjT ps)    = foldl g acc $ map snd ps
        g acc (VarT n)     = S.insert n acc
        g acc _            = acc
