module Parser.ParserUnitUtils where

import Data.ExpToken
import Data.List

import Parser.ParserUtils

mkProg = ProgT
mkForm = FormT       .mkId
mkPair = PairT       .mkId
mkId   = IdT   p0 ws2
mkFunc = FuncT    ws1.mkId
mkArr  = ArrT  p0 ws2
mkObj  = ObjT  p0 ws2
mkVar  = VarT        .mkId
mkStr  = StrT  p0 ws2
mkNum  = NumT  p0 ws2
mkBool = BoolT p0 ws2
mkNull = NullT p0 ws2

mkForm' p = FormT       .mkId' p
mkPair' p = PairT       .mkId' p
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