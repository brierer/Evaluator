{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where
import Test.Framework (TestSuite,makeTestSuite,htfMain)

import {-@ HTF_TESTS @-} Unit.Parser.MonolithicParserTest
import {-@ HTF_TESTS @-} Unit.Eval.MultiPassEvalTest
import {-@ HTF_TESTS @-} Unit.Eval.TypeEvalTest
import {-@ HTF_TESTS @-} Unit.Eval.FunctionEvalTest

import {-@ HTF_TESTS @-} Prop.Parser.MonolithicParserTest
import {-@ HTF_TESTS @-} Prop.Eval.MultiPassEvalTest
import {-@ HTF_TESTS @-} Prop.Eval.TypeEvalTest
import {-@ HTF_TESTS @-} Prop.Eval.FunctionEvalTest

main :: IO ()
main = htfMain htf_importedTests
