{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where
import Test.Framework

import {-@ HTF_TESTS @-} TestEqParser
import {-@ HTF_TESTS @-} Parser.MonolithicTest

main :: IO ()
main = htfMain htf_importedTests
