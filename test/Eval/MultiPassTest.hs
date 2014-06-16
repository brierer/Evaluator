{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Eval.MultiPassTest where

import Test.Framework
import qualified Data.Map as M
import TestUtils

import Eval.MultiPass

prop_initState (UniqueDefs prog) = Right (State (fromProgForms prog) M.empty) == initState prog
prop_initStateMultipleDef (MultiDefs prog x) = Left (MultipleDefinitions x) == initState prog

prop_validEval (ValidProg prog) = 
  let Right s@(State inBefore outBefore) = initState $ toToken prog
      Right (State inAfter  outAfter) = derefVars s
      Right (State expectedIn expectedOut) = initState $ toToken $ derefValidProg prog
  in  M.null outBefore && M.null inAfter && M.null expectedOut &&
      M.keys inBefore == M.keys outAfter && outAfter == expectedIn

prop_undefs (UndefProg prog x) = not (null $ forms prog) ==>
                                 let Right s = initState $ toToken prog
                                 in  Left (UndefinedVariable x) == derefVars s

prop_cycles (CycleProg prog xs) = not (null $ forms prog) ==>
                                  let Right s = initState $ toToken prog
                                  in  Left (CycleInVariables xs) == derefVars s








