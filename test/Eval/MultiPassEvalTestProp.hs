{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Eval.MultiPassEvalTestProp where

import Data.Eval
import Data.EvalError
import Data.ExpToken
import Eval.MultiPass
import Eval.MultiPassEvalTestUtils
import Test.Framework

{-# ANN module "HLint: ignore Use camelCase" #-}

prop_MultiDefs (MultiDefs  prog p x)                        = nonEmpty prog ==> Left (MultipleDefinitions p x) == initTable (toToken prog)
prop_ValidDefs (UniqueDefs prog)                            =                   Right (formTable $ forms prog) == initTable (toToken prog)

prop_UndefVars (UndefVars prog p x)                         = nonEmpty prog ==> Left (UndefinedVariable p x)   == derefVars (unsafeInitProg prog)
prop_CycleVars (CycleVars prog ps)                          = nonEmpty prog ==> Left (CycleInDefinitions ps)   == derefVars (unsafeInitProg prog)
prop_ValidVars (ValidVars prog)                             =                   Right (unsafeDerefVars prog)   == derefVars (unsafeInitProg prog)

prop_UndefFuncs (UndefFuncs      (ValidFuncs prog ns) p fn) = nonEmpty prog ==> Left (UndefinedFunction p fn)  == validateFunctions ns (unsafeInitProg prog)
prop_NonTopShow (NonTopShowFuncs (ValidFuncs prog ns) p)    = nonEmpty prog ==> Left (NonTopLevelShow p)       == validateFunctions ns (unsafeInitProg prog)
prop_NoShow     (NoShowFuncs     (ValidFuncs prog ns))      = nonEmpty prog ==> Left NoShow                    == validateFunctions ns (unsafeInitProg prog)
prop_ValidFuncs (ValidFuncs      prog ns)                   =                   Right ()                       == validateFunctions ns (unsafeInitProg prog)

{-| Utils -}
nonEmpty        :: HasProg a => a -> Bool
toToken         :: HasProg a => a -> ProgToken
unsafeInitProg  :: HasProg a => a -> Table
unsafeDerefVars :: HasProg a => a -> Table

nonEmpty = not.null.forms
toToken = fromForms.forms
unsafeInitProg = unsafeInitTable.toToken
unsafeDerefVars = unsafeInitTable.fromForms.derefAll.forms