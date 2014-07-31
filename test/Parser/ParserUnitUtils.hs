module Parser.ParserUnitUtils where

import Data.ExpToken
import Data.List

import Parser.ParserUtils

mkProg = ProgT
mkForm = mkForm' p0
mkPair = mkPair' p0
mkId   = mkId'   p0
mkFunc = mkFunc' p0
mkArr  = mkArr'  p0
mkObj  = mkObj'  p0
mkVar  = mkVar'  p0
mkStr  = mkStr'  p0
mkNum  = mkNum'  p0
mkBool = mkBool' p0
mkNull = mkNull' p0

mkForm' p = FormT      .mkId' p
mkPair' p = PairT      .mkId' p
mkId'   p = IdT   p ws2
mkFunc' p = FuncT   ws1.mkId' p
mkArr'  p = ArrT  p ws2
mkObj'  p = ObjT  p ws2
mkVar'  p = VarT       .mkId' p
mkStr'  p = StrT  p ws2
mkNum'  p = NumT  p ws2
mkBool' p = BoolT p ws2
mkNull' p = NullT p ws2

ascii = [' '..'~'] \\ "\"\\"