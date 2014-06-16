module Parser.Monolithic where

import Text.ParserCombinators.Parsec
import Control.Monad
import Control.Applicative ((<$))
import Data.Char
import Data.List

import Data.Token

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
pairT  :: Parser PairToken

-- expT ->      nullT  |      boolT  |      numT  |      strT   |      funcT  |      arrayT  |      objT  |      varT
expT = ws $ try nullT <|> try boolT <|> try numT <|> try strT  <|> try funcT <|> try arrayT <|> try objT <|> try varT

-- pairT -> idS ':' expT
pairT = ws $ let f x _ = PairT x in liftM3 f idS (wChar ':') expT

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
numT = ws $ let (<:>) x y = option "" $ liftM2 (:) x y in do
  intPart <- integerS
  optDec <- wChar '.'   <:> many1 digit
  optExp <- oneOf "eE" <:> integerS
  let o = concat [intPart,optDec,optExp]
  return $ NumT o $ read o

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

ws :: Parser a -> Parser a
ws = between (many space) (many space)

{-| Unparse the derivation tree, exactly as it was parsed -}
class Unparse a where unparse :: a -> String
instance Unparse ProgToken where unparse (ProgT fs)  = unparses' "\n" fs
instance Unparse FormToken where unparse (FormT s e) = s ++ "=" ++ unparse e       
instance Unparse PairToken where unparse (PairT s e) = s ++ ":" ++ unparse e       
instance Unparse ExpToken  where unparse             = unparseExp                                    

unparses :: Unparse a => [a] -> String
unparses  = unparses' ","

unparses' :: Unparse a => String -> [a] -> String
unparses' sep = intercalate sep . map unparse

unparseExp :: ExpToken -> String
unparseExp (FuncT i es) = i ++ "(" ++ unparses es ++ ")"
unparseExp (ArrayT es)  = "[" ++ unparses es ++ "]"
unparseExp (ObjT ps)    = "{" ++ unparses ps ++ "}"
unparseExp (VarT v)     = v
unparseExp (StrT s)     = show s
unparseExp (NumT o _)   = o
unparseExp (BoolT b)    = map toLower $ show b
unparseExp NullT        = "null"



















