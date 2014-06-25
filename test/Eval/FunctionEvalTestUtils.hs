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
isFunc (FuncT{}) = True
isFunc _         = False

w1 = ""
w2 = ("","")
p0 = (0 :: Int,0 :: Int)

getP (ArrayT p _ _) = p
getP (ObjT p _ _)   = p
getP (StrT p _ _)   = p
getP (NumT p _ _ _) = p
getP (BoolT p _ _)  = p
getP (NullT p _)    = p
getP e              = error $ "FunctionEvalTest::getP [Failed pattern match ["++show e++"]]"

getT (ArrayT{}) = head types
getT (ObjT{})   = types !! 1
getT (StrT{})   = types !! 2
getT (NumT{})   = types !! 3
getT (BoolT{})  = types !! 4
getT (NullT{})  = types !! 5
getT e          = error $ "FunctionEvalTest::getT [Failed pattern match ["++show e++"]]"

funcNames = ["arrayTestF", "objTestF","strTestF","numTestF","boolTestF","nullTestF"]
types     = ["Array","Object","String","Number","Boolean","Null"]

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

isNull (NullT _ _) = True
isNull _           = False







