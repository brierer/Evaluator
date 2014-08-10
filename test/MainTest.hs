{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where
import Test.Framework

import {-@ HTF_TESTS @-} Engine.EngineUnitFailureType
import {-@ HTF_TESTS @-} Engine.EngineUnitFailureConstraint
import {-@ HTF_TESTS @-} Engine.EngineUnitSuccess
import {-@ HTF_TESTS @-} Marshall.MarshallUnit
import {-@ HTF_TESTS @-} MatchType.MatchTypeUnitFailure
import {-@ HTF_TESTS @-} MatchType.MatchTypeUnitSuccess
import {-@ HTF_TESTS @-} MultiPass.MultiPassUnit
import {-@ HTF_TESTS @-} Parser.ParserUnitFailure
import {-@ HTF_TESTS @-} Parser.ParserUnitSuccess
import {-@ HTF_TESTS @-} Serialize.SerializeUnit

import {-@ HTF_TESTS @-} Engine.EnginePropFailureType
import {-@ HTF_TESTS @-} Engine.EnginePropFailureConstraint
import {-@ HTF_TESTS @-} Engine.EnginePropSuccess
import {-@ HTF_TESTS @-} Marshall.MarshallPropFailure
import {-@ HTF_TESTS @-} Marshall.MarshallPropSuccess
import {-@ HTF_TESTS @-} MatchType.MatchTypeProp
import {-@ HTF_TESTS @-} MultiPass.MultiPassProp
--import {-@ HTF_TESTS @-} Parser.ParserPropFailure
import {-@ HTF_TESTS @-} Parser.ParserPropSuccess
--import {-@ HTF_TESTS @-} Serialize.SerializeProp

main :: IO ()
main = htfMain htf_importedTests
