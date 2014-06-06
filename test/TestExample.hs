{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TestExample where

import Test.Framework
import Data.List

mysort [] = []
mysort [x] = [x]
mysort (x:xs) = mysort (filter (<x) xs) ++ [x] ++ mysort (filter (>= x) xs)

test_unitTest :: IO ()
test_unitTest = do
  assertEqual [1,2,3] (mysort [3,2,1] :: [Integer])
  assertEqual [1,2,3,4,5] (mysort [5,4,3,2,1] :: [Integer])

prop_randomTest :: [Integer] -> Bool
prop_randomTest xs = mysort xs == sort xs