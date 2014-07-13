module Common.Eval.MultiPassEvalUtils where

import qualified Data.Map as M

import Data.ExpToken
import Eval.MultiPass
import Parser.Monolithic

import Common.Parser.MonolithicParserUtils

toTriple(FormT (IdT p _ a) b) = (a,(b,p))

unsafeProg = unsafeParse progT
formTable = M.fromList.map toTriple
unsafeInitTable = unsafeRight.initTable
