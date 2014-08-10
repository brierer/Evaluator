module Parser.ParserUtils where

import Data.ExpToken
import Eval.Parser
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error

instance Eq ParseError where a == b = errorPos a == errorPos b && errorMessages a == errorMessages b

p0 = (0,0) :: Pos
ws1 = ""
ws2 = ("","")

unsafeParse p = unsafeRight . evalParse p
simpleParse p = parse p ""

unsafeRight (Right x) = x
unsafeRight x         = error $ "MonolithicParserUtils::unsafeRight [UnexpectedPattern ["++show x++"]]"