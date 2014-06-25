{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Eval.FunctionEvalTest where

import Test.Framework
import Data.Eval
import Parser.ParserTestUtils
import Eval.Function

import Prelude hiding (null)

import Eval.FunctionEvalTestUtils

prop_NbArgs1 (P p) esTA = length esTA > 1 ==> let es = map unExpTA esTA in
  all (\name -> Left (InvalidNbOfArgs p name 1 0)           == applyFunc' funcs p name []
             && Left (InvalidNbOfArgs p name 1 (length es)) == applyFunc' funcs p name es)
    ["show","multi","mean","descriptive"]

prop_NbArgs2 (P p) esTA = length esTA > 2 ==> let es = map unExpTA esTA in
  all (\name -> Left (InvalidNbOfArgs p name 2 0)           == applyFunc' funcs p name []
             && Left (InvalidNbOfArgs p name 2 1)           == applyFunc' funcs p name (take 1 es)
             && Left (InvalidNbOfArgs p name 2 (length es)) == applyFunc' funcs p name es)
    ["table","nTimes","take","sortTable"]

prop_NbArgs3 (P p) esTA = length esTA > 3 ==> let es = map unExpTA esTA in
  all (\name -> Left (InvalidNbOfArgs p name 3 0)           == applyFunc' funcs p name []
             && Left (InvalidNbOfArgs p name 3 1)           == applyFunc' funcs p name (take 1 es)
             && Left (InvalidNbOfArgs p name 3 2)           == applyFunc' funcs p name (take 2 es)
             && Left (InvalidNbOfArgs p name 3 (length es)) == applyFunc' funcs p name es)
    ["plotLine"]

prop_ErrorMarshallNull (ExpTA e) = not (isNull e || isVar e || isFunc e) ==> Left (TypeMismatch (getP e) (getT e) "Null") == null e
prop_MarshallNullLit  (NullTA n) = Right NullObj == null n
prop_MarshallNullFunc (TF es)    = Right NullObj == applyFunc' (mkFuncs es) p0 "nullTestF" []
