module Eval.FunctionEvalTestUtils where

import Test.Framework
import Control.Monad
import Data.Token
import Data.Eval
import Parser.ParserTestUtils
import Eval.MultiPass

import Prelude hiding (null)

applyFunc' fs p n es = applyFunc fs (mkFunc p n es)
mkFunc p n = FuncT p w1 (IdT p w2 n)

w1 = ""
w2 = ("","")
p0 = (0 :: Int,0 :: Int)

funcNames = ["arrayTestF", "objTestF","strTestF","numTestF","boolTestF","nullTestF"]
mkFuncs = zipWith f funcNames where f name e = (name,([],testF e))

data TestFuncs = TF [ExpToken] deriving (Show)
instance Arbitrary TestFuncs where
  arbitrary                  = mTestFuncs (sizedArrayTA zero)    (sizedObjTA zero)    arbitrary            arbitrary            arbitrary             arbitrary
  shrink (TF [a,o,s,n,b,n']) = mTestFuncs (shrink $ toArrayTA a) (shrink $ toObjTA o) (shrink $ toStrTA s) (shrink $ toNumTA n) (shrink $ toBoolTA b) (shrink $ toNullTA n')
  shrink _                   = error "FunctionEvalTest::shrink [Pattern mismatch in TestFuncs instance]"
mTestFuncs a o s n b n' = liftM TF $ sequence [liftM unArrayTA a, liftM unObjTA o, liftM unStrTA s, liftM unNumTA n, liftM unBoolTA b, liftM unNullTA n']
zero = 0 :: Integer

testF :: ExpToken -> [Obj] -> Eval Obj
testF (ArrayT _ _ es) [] = liftM ArrayObj $ mapM (`testF` []) es
testF (ObjT _ _ ps)   [] = liftM ObjObj   $ mapM (\(PairT _ (IdT _ _ x) y) -> liftM2 (,) (return x) (testF y [])) ps
testF (StrT _ _ s)    [] = return $ StrObj s
testF (NumT _ _ _ n)  [] = return $ NumObj n
testF (BoolT _ _ b)   [] = return $ BoolObj b 
testF (NullT _ _)     [] = return NullObj
testF (VarT{})        [] = return NullObj
testF e               os = error $ "FunctionEvalTest::mooTestF [] Failed pattern match ["++show e++"] and ["++show os++"]]"

isFunc (FuncT{}) = True
isFunc _         = False

isVar (VarT _ _) = True
isVar _          = False

isNull (NullT _ _) = True
isNull _           = False





