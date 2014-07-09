{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where
import Test.Framework (TestSuite,makeTestSuite,htfMain)

import {-@ HTF_TESTS @-} Parser.MonolithicParserTest
import {-@ HTF_TESTS @-} Eval.MultiPassEvalTest
import {-@ HTF_TESTS @-} Eval.FunctionEvalTest
import {-@ HTF_TESTS @-} Eval.EngineErrorTest
import {-@ HTF_TESTS @-} Eval.EngineReturnValueTest

main :: IO ()
main = htfMain htf_importedTests
