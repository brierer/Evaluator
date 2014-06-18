{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Eval.MultiPassTest where

import qualified Data.Map as M (null,keys,empty)

import Eval.EvalTestUtils      (HasProg,UniqueDefs(..),MultiDefs(..),ValidProg(..),UndefProg(..),CycleProg(..),initState',derefValidProg',nonEmpty,fromProgForms,toToken)
import Eval.MultiPass          (State(..),EvalError(..),initState,derefVars)
import Test.Framework          (TestSuite,makeTestSuite,makeQuickCheckTest,makeLoc,qcAssertion,(==>))

prop_validInitState (UniqueDefs prog) = Right (State (fromProgForms prog) M.empty) == initState (toToken prog)

prop_validEval (ValidProg prog) = let before  @(State beforeIn   _)         = initState' prog
                                      expected                              = let State a b = derefValidProg' prog in State b a
                                      actual  @(State actualIn   actualOut) = (\(Right x)->x) $ derefVars before
                                  in  expected == actual && M.null actualIn && M.keys beforeIn == M.keys actualOut

prop_multiDef (MultiDefs prog x)  = nonEmpty prog ==> Left (MultipleDefinitions x) == initState (toToken prog)
prop_undefs   (UndefProg prog x)  = nonEmpty prog ==> Left (UndefinedVariable x)   == derefVars (initState' prog)
prop_cycles   (CycleProg prog xs) = nonEmpty prog ==> Left (CycleInVariables xs)   == derefVars (initState' prog)

