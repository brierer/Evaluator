{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where
import Test.Framework

import {-@ HTF_TESTS @-} TestEqParser
import {-@ HTF_TESTS @-} Parser.ParserTest

main :: IO ()
main = htfMain htf_importedTests
