{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-orphans #-}
module Parser.ParserTest where

import Test.Framework
import Control.Monad
import Data.List
import Data.Char
import Numeric

import Text.ParserCombinators.Parsec hiding (alphaNum)
import Text.ParserCombinators.Parsec.Error

import Parser.Parser

{-# ANN module "HLint: ignore Use camelCase" #-}
{-# ANN module "HLint: ignore Redundant do" #-}

-- Program
prop_prog fs                 = eqCase (ProgT fs)              progT  (parseable (ProgT fs))
prop_form (IdA s) e          = eqCase (FormT s e)             formT  (parseable (FormT s e))
                                                                      
-- Composite expressions                                              
prop_func (IdA s) es         = eqCase (FuncT s es)            funcT  (parseable (FuncT s es))
prop_array es                = eqCase (ArrayT es)             arrayT (parseable (ArrayT es))
prop_obj ps                  = eqCase (ObjT (map unPair ps))  objT   (parseable (ObjT (map unPair ps)))
                                                                      
prop_exp e                   = eqCase e                       expT   (parseable (e :: ExpToken))
prop_pair p@(Pair (IdA s) e) = eqCase (s,e)                   pairT  (parseable p)
                                                                      
-- Atomic expressions                                                 
prop_var    (IdA s)          = eqCase (VarT s)                varT   (parseable (VarT s))
prop_string (StrTA s)        = eqCase (StrT s)                strT   (parseable (StrT s))
                                                                      
prop_number_int i            = eqCase (NumT (fromIntegral i)) numT   [show (i :: Integer)]
prop_number_flt f            = eqCase (NumT f)                numT   [show (f :: Double)]
prop_number_exp f            = eqCase (NumT f)                numT   [showEFloat Nothing f ""]
                                                                     
prop_bool b                  = eqCase (BoolT b)               boolT  (parseable (BoolT b))
prop_null                    = eqCase NullT                   nullT  (parseable NullT)

eqCase x p ss = all ((Right x ==). parse p "") $ cases ss
cases ss = [concat ss, unwords ss, " " ++ concat ss ++ " ", " " ++ unwords ss ++ " "]

instance Eq ParseError where (==) a b = errorMessages a == errorMessages b

newtype StrTA = StrTA String      deriving Show
newtype IdA   = IdA   String      deriving Show
data    Pair  = Pair IdA ExpToken deriving Show

instance Arbitrary StrTA where arbitrary = liftM StrTA $ mListOf $ elements $ [' '..'~'] \\ "\"\\"
                               shrink (StrTA s) = map StrTA $ filter validStr $ shrink s

instance Arbitrary IdA   where arbitrary = liftM IdA   $ liftM2 (:) (elements alpha) $ mListOf $ elements alphaNum
                               shrink (IdA s) = map IdA $ filter validId $ shrink s

instance Arbitrary Pair  where arbitrary = liftM2 Pair arbitrary (arb 1)
                               shrink (Pair idA tok) = [Pair idA' tok' | idA' <- shrink idA, tok' <- shrink tok]

instance Arbitrary FormToken where arbitrary = liftM2 FormT (liftM unId arbitrary) (arb 1)
                                   shrink (FormT idA e) = [FormT (unId idA') e' | idA' <- shrink (IdA idA), e' <- shrink e]

instance Arbitrary ExpToken where arbitrary = arb 1
                                  shrink = shrinkTok

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

shrinkTok (FuncT s es)  = [FuncT s' es' | (IdA s') <- shrink (IdA s), es' <- shrink es]
shrinkTok (ArrayT es)   = map ArrayT $ shrink es
shrinkTok (ObjT ps)     = map (ObjT . map unPair) (shrink $ map toPair ps)
shrinkTok (VarT v)      = map (VarT  .unId) $ shrink (IdA v)
shrinkTok (StrT s)      = map StrT $ shrink s
shrinkTok (NumT x)      = map NumT $ shrink x
shrinkTok (BoolT True)  = [BoolT False]
shrinkTok (BoolT False) = [NullT]
shrinkTok NullT         = []

parseableTok (FuncT i es) = parseable (IdA i) ++ ["("] ++ parseables es ++ [")"]
parseableTok (ArrayT es)  = ["["] ++ parseables es ++ ["]"]
parseableTok (ObjT ps)    = ["{"] ++ parseables (map toPair ps) ++ ["}"]
parseableTok (VarT v)     = parseable (IdA v)
parseableTok (StrT s)     = [show s]
parseableTok (NumT x)     = [show x]
parseableTok (BoolT b)    = [map toLower $ show b]
parseableTok NullT        = ["null"]

class Parseable a where parseable :: a -> [String]
instance Parseable IdA where parseable (IdA s) = [s]
instance Parseable Pair where parseable (Pair (IdA s) tok) = parseable (IdA s) ++ [":"] ++ parseable tok
instance Parseable ProgToken where parseable (ProgT fs)    = parseables' "\n" fs
instance Parseable FormToken where parseable (FormT s e)   = parseable (IdA s) ++ ["="] ++ parseable e
instance Parseable ExpToken where parseable = parseableTok

parseables :: Parseable a => [a] -> [String]
parseables  = parseables' ","
parseables' sep xs = intercalate [sep] $ map parseable xs 

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




