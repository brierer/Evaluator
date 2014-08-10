module Parser.ParserUnitFailureUtils where

import Data.EvalError

mkInvalidParse x y rest = Left $ InvalidParse (x,y) $ ("(line "++show x++", column "++show y++"):"):rest

expectEq   = "expecting space or \"=\""
expectForm = "expecting space, end of input or identifier"
expectExp  = "expecting space, \"null\", \"true\", \"false\", number, \"\\\"\", identifier, \"[\" or \"{\""

expectForm2 = "expecting space or identifier"