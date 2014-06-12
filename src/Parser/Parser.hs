module Parser.Parser where

import Text.ParserCombinators.Parsec
import Control.Monad
import Control.Applicative ((<$))

data ProgToken = ProgT [FormToken] deriving (Eq,Show)
data FormToken = FormT IdS ExpToken    deriving (Eq,Show)

data ExpToken = FuncT IdS [ExpToken]
              | ArrayT [ExpToken]
              | ObjT [(IdS,ExpToken)]
              | VarT IdS
              | StrT String
              | NumT Double
              | BoolT Bool
              | NullT
                deriving (Eq,Show)

type IntegerS = String
type IdS      = String

{-| Formula tokens -}
progT :: Parser ProgToken
formT :: Parser FormToken

-- progT -> (formT '\n')*
progT = ws $ liftM ProgT $ formT `sepBy` many space

-- formT -> id '=' expT
formT = ws $ liftM2 FormT idS (wChar '=' >> expT)

{-| Composite expressions -}
funcT  :: Parser ExpToken
arrayT :: Parser ExpToken
objT   :: Parser ExpToken

-- funcT -> id '(' expT* ')'
funcT  = ws $ liftM2 FuncT idS $ between (wChar '(') (wChar ')') $ commaSep expT

-- arrayT -> '[' expT* ']'
arrayT = ws $ liftM ArrayT $ between (wChar '[') (wChar ']') $ commaSep expT

-- objT -> '{' pairT* '}'
objT   = ws $ liftM ObjT   $ between (wChar '{') (wChar '}') $ commaSep pairT

{-| Sequence elements -}
expT   :: Parser ExpToken
pairT  :: Parser (IdS,ExpToken)

-- expT ->      nullT  |      boolT  |      numT  |      strT   |      funcT  |      arrayT  |      objT  |      varT
expT = ws $ try nullT <|> try boolT <|> try numT <|> try strT  <|> try funcT <|> try arrayT <|> try objT <|> try varT

-- pairT -> idS ':' expT
pairT = ws $ let f x _ = (,) x in liftM3 f idS (wChar ':') expT

{-| Atomic expressions -}
varT  :: Parser ExpToken
strT  :: Parser ExpToken
numT  :: Parser ExpToken
boolT :: Parser ExpToken
nullT :: Parser ExpToken

-- varT -> id
varT = ws $ do
  x <- liftM VarT idS
  notFollowedBy (oneOf "(:")
  return x

-- strT -> '"' ($printable - ['"']) '"'
strT = ws $ liftM StrT $ between (char '"') (char '"') (many $ noneOf "\"")

-- numT -> integerS ('.' $digit+)? ([eE] integerS)?
numT = ws $ let (<:>) x y = option "" $ liftM2 (:) x y
                optDec = wChar '.'   <:> many1 digit
                optExp = oneOf "eE" <:> integerS
            in liftM (NumT .read.concat).sequence $ integerS:[optDec,optExp]

-- boolT -> 'true' | 'false'
boolT = ws $ liftM BoolT $ True <$ kw "true" <|> False <$ kw "false"

-- nullT -> 'null' 
nullT = ws $ kw "null" >> return NullT


{-| Subatomic helpers -}
integerS :: Parser IntegerS
idS      :: Parser IdS

-- integerS -> '-'? $digit+
integerS = ws $ liftM2 (++) (option "" $ string "-") $ many1 digit

-- idS -> alpha [alpha | digit]*
idS = ws $ let alpha = ['a'..'z'] ++ ['A'..'Z'] 
           in liftM2 (:) (oneOf alpha) $ many (oneOf alpha <|> oneOf ['0'..'9'])

{-| Misc helpers -}
kw :: String -> Parser ()
kw s = string s >> notFollowedBy (noneOf ")}], \n")

commaSep :: Parser a -> Parser [a]
commaSep p  = p `sepBy` wChar ','

wChar :: Char -> Parser Char
wChar = ws . char

ws = between (many space) (many space)


