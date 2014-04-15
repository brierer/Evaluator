{-# OPTIONS_GHC -XBangPatterns #-}

-----------------------------------------------------------------------------
-- Module      : Math.Statistics
-- Copyright   : (c) 2008 Marshall Beddoe
-- License     : BSD3
--
-- Maintainer  : mbeddoe@<nospam>gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Description :
--   A collection of commonly used statistical functions.
-----------------------------------------------------------------------------

module Evaluator.Stats where

import Data.List
import Data.Ord (comparing)
import Statistics.Sample.Powers as Stat
import Data.Vector.Unboxed
import Evaluator.DValue

descriptive :: [Double] -> DValue
descriptive ds = DArray $ [DArray $ Data.List.map DString $ fst desc, DArray $  Data.List.map DNum $ snd desc]
		where vs = fromList ds
		      ps = powers 4 vs
		      desc = Data.List.unzip $ [("count",fromIntegral $ Stat.count ps),
						("sum", Stat.sum ps),
						("mean", Stat.mean ps),	
						("variance", Stat.variance ps),	
						("skewness", Stat.skewness ps),	
						("kurtosis", Stat.kurtosis ps)	]		



nTimes :: Double -> Double -> DValue
nTimes n t = DArray $ Data.List.map DNum $  [n..t] 

