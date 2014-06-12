module Parser.Parser where

import Text.ParserCombinators.Parsec
import Control.Monad
import Control.Applicative ((<$))

data Token = FuncT IdS [Token]
           | ArrayT [Token]
           | ObjT [(IdS,Token)]
           | VarT IdS
           | StrT String
           | NumT Double
           | BoolT Bool
           | NullT
             deriving (Eq,Show)

type IntegerS = String
type IdS      = String

{-| Composite expressions -}
funcT  :: Parser Token
arrayT :: Parser Token
objT   :: Parser Token

-- funcT -> id '(' expT* ')'
funcT  = liftM2 FuncT idS $ between (char '(') (char ')') $ commaSep expT

-- arrayT -> '[' expT* ']'
arrayT = liftM ArrayT $ between (char '[') (char ']') $ commaSep expT

-- objT -> '{' pairT* '}'
objT   = liftM ObjT   $ between (char '{') (char '}') $ commaSep pairT

{-| Sequence elements -}
expT   :: Parser Token
pairT  :: Parser (IdS,Token)

-- expT -> nullT  |  boolT  |  numT  |  strT   |  funcT  |  arrayT  |  objT  |  varT
expT =     try nullT <|> try boolT <|> try numT <|> try strT  <|> try funcT <|> try arrayT <|> try objT <|> try varT

-- pairT -> idS ':' expT
pairT = let f x _ = (,) x in liftM3 f idS (char ':') expT

{-| Atomic expressions -}
varT  :: Parser Token
strT  :: Parser Token
numT  :: Parser Token
boolT :: Parser Token
nullT :: Parser Token

-- varT -> id
varT = do
  x <- liftM VarT idS
  notFollowedBy (oneOf "(:")
  return x

-- strT -> '"' ($printable - ['"']) '"'
strT = liftM StrT $ between (char '"') (char '"') (many $ noneOf "\"")

-- numT -> integerS ('.' $digit+)? ([eE] integerS)?
numT = let (<:>) x y = option "" $ liftM2 (:) x y
           optDec = char '.'   <:> many1 digit
           optExp = oneOf "eE" <:> integerS
       in liftM (NumT .read.concat).sequence $ integerS:[optDec,optExp]

-- boolT -> 'true' | 'false'
boolT = liftM BoolT $ True <$ kw "true" <|> False <$ kw "false"

-- nullT -> 'null' 
nullT = kw "null" >> return NullT


{-| Subatomic helpers -}
integerS :: Parser IntegerS
idS      :: Parser IdS

-- integerS -> '-'? $digit+
integerS = liftM2 (++) (option "" $ string "-") $ many1 digit

-- idS -> alpha [alpha | digit]*
idS = let alpha = ['a'..'z'] ++ ['A'..'Z'] 
      in liftM2 (:) (oneOf alpha) $ many (oneOf alpha <|> oneOf ['0'..'9'])

{-| Misc helpers -}
kw :: String -> Parser ()
kw s = string s >> notFollowedBy (noneOf ")}],")

commaSep :: Parser a -> Parser [a]
commaSep p  = p `sepBy` char ','
