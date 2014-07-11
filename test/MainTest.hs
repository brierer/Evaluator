{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where
import Test.Framework (TestSuite,makeTestSuite,htfMain)

import {-@ HTF_TESTS @-} Parser.MonolithicParserTestUnit
import {-@ HTF_TESTS @-} Eval.MultiPassEvalTestUnit
--import {-@ HTF_TESTS @-} Eval.FunctionEvalTest
--import {-@ HTF_TESTS @-} Eval.EngineErrorTest
--import {-@ HTF_TESTS @-} Eval.EngineReturnValueTest

import {-@ HTF_TESTS @-} Parser.MonolithicParserTestProp
import {-@ HTF_TESTS @-} Eval.MultiPassEvalTestProp

main :: IO ()
main = htfMain htf_importedTests
