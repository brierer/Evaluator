{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where
import Test.Framework (TestSuite,makeTestSuite,htfMain)

import {-@ HTF_TESTS @-} Parser.MonolithicParserTestUnit
import {-@ HTF_TESTS @-} Eval.MultiPass.MultiPassEvalTestUnit
import {-@ HTF_TESTS @-} Eval.Function.FunctionEvalTest
import {-@ HTF_TESTS @-} Eval.Engine.EngineErrorTest
import {-@ HTF_TESTS @-} Eval.Engine.EngineReturnValueTest

import {-@ HTF_TESTS @-} Parser.MonolithicParserTestProp
import {-@ HTF_TESTS @-} Eval.MultiPass.MultiPassEvalTestProp

main :: IO ()
main = htfMain htf_importedTests
