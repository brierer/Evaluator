{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where
import Test.Framework

--import {-@ HTF_TESTS @-} Path.To.Module
import {-@ HTF_TESTS @-} TestExample
import {-@ HTF_TESTS @-} TestEqParser

main :: IO ()
main = htfMain htf_importedTests
