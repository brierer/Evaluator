{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where
import Test.Framework (TestSuite,makeTestSuite,htfMain)

import {-@ HTF_TESTS @-} Parser.MonolithicTest
import {-@ HTF_TESTS @-} Eval.MultiPassTest
import {-@ HTF_TESTS @-} Eval.FunctionEvalTest

main :: IO ()
main = htfMain htf_importedTests
