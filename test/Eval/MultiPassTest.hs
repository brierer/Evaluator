{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Eval.MultiPassTest where

import Eval.EvalTestUtils      (HasProg,MultiDefs(..),UniqueDefs(..),UndefVars(..),CycleVars(..),ValidVars(..),initTable',derefValidProg',nonEmpty,fromProgForms,toToken)
import Eval.MultiPass          (EvalError(..),initTable,derefVars)
import Test.Framework          (TestSuite,makeTestSuite,makeQuickCheckTest,makeLoc,qcAssertion,(==>))

prop_multiDefs (MultiDefs prog x)  = nonEmpty prog ==> Left (MultipleDefinitions x) == initTable (toToken prog)
prop_validDefs (UniqueDefs prog)   =                     Right (fromProgForms prog) == initTable (toToken prog)

prop_undefVars (UndefVars prog x)  = nonEmpty prog ==> Left (UndefinedVariable x)   == derefVars (initTable' prog)
prop_cycleVars (CycleVars prog xs) = nonEmpty prog ==> Left (CycleInVariables xs)   == derefVars (initTable' prog)
prop_validVars (ValidVars prog)    =                   Right (derefValidProg' prog) == derefVars (initTable' prog)

