module Parser.ParserUnitFailureUtils where

import Data.EvalError

mkInvalidParse x y rest = Left $ InvalidParse (x,y) $ ("(line "++show x++", column "++show y++"):"):rest

expectForm  = "expecting space, end of input or identifier"
expectId    = "expecting space or identifier"
expectExp   = "expecting space, \"null\", \"true\", \"false\", number, \"\\\"\", identifier, \"[\" or \"{\""

expectFuncEnd1 = "expecting space, \"null\", \"true\", \"false\", number, \"\\\"\", identifier, \"[\", \"{\" or \")\""
expectFuncEnd2 = "expecting space, \",\" or \")\""
expectFuncEnd3 = "expecting \",\" or \")\""

expectArr      = "expecting space or \"[\""
expectArrEnd1  = "expecting space, \"null\", \"true\", \"false\", number, \"\\\"\", identifier, \"[\", \"{\" or \"]\""
expectArrEnd2  = "expecting space, \",\" or \"]\""
expectArrEnd3  = "expecting \",\" or \"]\""

expectObj      = "expecting space or \"{\""
expectObjEnd1  = "expecting space, identifier or \"}\""
expectObjEnd2  = "expecting digit, \".\", space, \",\" or \"}\""
expectObjEnd3  = "expecting \",\" or \"}\""

expectStr   = "expecting space or \"\\\"\""
expectNum'  = "expecting space or number"
expectNum   = "expecting number"
expectDigit = "expecting digit"
expectBool  = "expecting space, \"true\" or \"false\""
expectTrue  = "expecting \"true\""
expectFalse = "expecting \"false\""
expectNull' = "expecting space or \"null\""
expectNull  = "expecting \"null\""

expectEq    = "expecting space or \"=\""
expectColon = "expecting space or \":\""
expectQuote = "expecting \"\\\"\""
expectParen = "expecting space or \"(\""
