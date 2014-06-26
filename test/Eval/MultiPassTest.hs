{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Eval.MultiPassTest where

import Data.Eval               (EvalError(..))
import Eval.EvalTestUtils      (HasProg,UniqueDefs(..),MultiDefs(..),ValidVars(..),UndefVars(..),CycleVars(..),ValidFuncs(..),UndefFuncs(..),NonTopShowFuncs(..),NoShowFuncs(..),
                                initTable',derefValidProg',nonEmpty,fromProgForms,toToken)
import Eval.MultiPass          (initTable,derefVars,validateFunctions)
import Test.Framework          (TestSuite,makeTestSuite,makeQuickCheckTest,makeLoc,qcAssertion,(==>))

prop_MultiDefs (MultiDefs  prog p x)                        = nonEmpty prog ==> Left (MultipleDefinitions p x) == initTable (toToken prog)
prop_ValidDefs (UniqueDefs prog)                            =                   Right (fromProgForms prog)     == initTable (toToken prog)
                                                                                                            
prop_UndefVars (UndefVars prog x)                           = nonEmpty prog ==> Left (UndefinedVariable x)     == derefVars (initTable' prog)
prop_CycleVars (CycleVars prog ps)                          = nonEmpty prog ==> Left (CycleInDefinitions ps)   == derefVars (initTable' prog)
prop_ValidVars (ValidVars prog)                             =                   Right (derefValidProg' prog)   == derefVars (initTable' prog)

prop_UndefFuncs (UndefFuncs      (ValidFuncs prog ns) p fn) = nonEmpty prog ==> Left (UndefinedFunction p fn)  == validateFunctions ns (initTable' prog) 
prop_NonTopShow (NonTopShowFuncs (ValidFuncs prog ns) p)    = nonEmpty prog ==> Left (NonTopLevelShow p)       == validateFunctions ns (initTable' prog) 
prop_NoShow     (NoShowFuncs     (ValidFuncs prog ns))      = nonEmpty prog ==> Left NoShow                    == validateFunctions ns (initTable' prog)
prop_ValidFuncs (ValidFuncs      prog ns)                   =                   Right ()                       == validateFunctions ns (initTable' prog)

