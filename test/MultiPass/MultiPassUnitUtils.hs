module MultiPass.MultiPassUnitUtils where

import qualified Data.Map as M

import Eval.MultiPass

import MultiPass.MultiPassUtils

formTable = M.fromList.map toTriple
initProg = initTable.unsafeProg
derefProg = derefVars.unsafeInitTable.unsafeProg
validateProg fs = validateFunctions fs.unsafeInitTable.unsafeProg





















