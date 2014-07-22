module MultiPass.MultiPassUtils where

import qualified Data.Map as M

import Data.ExpToken
import Eval.MultiPass
import Eval.Parser

import Parser.ParserUtils

toTriple(FormT (IdT p _ a) b) = (a,(b,p))

unsafeProg = unsafeParse progT
formTable = M.fromList.map toTriple
unsafeInitTable = unsafeRight.initTable
