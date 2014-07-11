{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Eval.MultiPassEvalTest where

import qualified Data.Map as M

import Data.EvalError
import Eval.MultiPass
import Eval.MultiPassEvalTestUtils
import Parser.MonolithicParserTestUtils
import Test.Framework

{-# ANN module "HLint: ignore Use camelCase" #-}

{-| Units -}
test_MultiDefs = do
  assertEqual (Left (MultipleDefinitions (1,8) "x"))                                $ initTable $ unsafeProg "x=null;x=true"
  assertEqual (Right $ M.fromList [("x",(mkNull,(1,1))),("y",(mkBool True,(1,8)))]) $ initTable $ unsafeProg "x=null;y=true"

{-| Props -}
prop_MultiDefs (MultiDefs  prog p x)                        = nonEmpty prog ==> Left (MultipleDefinitions p x) == initTable (toToken prog)
prop_ValidDefs (UniqueDefs prog)                            =                   Right (fromProgForms prog)     == initTable (toToken prog)

prop_UndefVars (UndefVars prog p x)                         = nonEmpty prog ==> Left (UndefinedVariable p x)   == derefVars (initTable' prog)
prop_CycleVars (CycleVars prog ps)                          = nonEmpty prog ==> Left (CycleInDefinitions ps)   == derefVars (initTable' prog)
prop_ValidVars (ValidVars prog)                             =                   Right (derefValidProg' prog)   == derefVars (initTable' prog)

prop_UndefFuncs (UndefFuncs      (ValidFuncs prog ns) p fn) = nonEmpty prog ==> Left (UndefinedFunction p fn)  == validateFunctions ns (initTable' prog)
prop_NonTopShow (NonTopShowFuncs (ValidFuncs prog ns) p)    = nonEmpty prog ==> Left (NonTopLevelShow p)       == validateFunctions ns (initTable' prog)
prop_NoShow     (NoShowFuncs     (ValidFuncs prog ns))      = nonEmpty prog ==> Left NoShow                    == validateFunctions ns (initTable' prog)
prop_ValidFuncs (ValidFuncs      prog ns)                   =                   Right ()                       == validateFunctions ns (initTable' prog)