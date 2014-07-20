module Common.Parser.MonolithicParserUtils where

import Data.ExpToken
import Text.ParserCombinators.Parsec

p0 = (0,0) :: Pos
ws1 = ""
ws2 = ("","")

unsafeParse p = unsafeRight . parse p ""

unsafeRight (Right x) = x
unsafeRight x         = error $ "MonolithicParserUtils::unsafeRight [UnexpectedPattern ["++show x++"]]"
