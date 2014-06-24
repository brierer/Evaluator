module Parser.Monolithic where

import Data.Char                     (toLower)
import Data.List                     (intercalate)
import Data.Token                    (ProgToken(..),FormToken(..),PairToken(..),IdToken(..),ExpToken(..),IntegerS)
import Control.Monad                 (liftM,liftM2)
import Control.Applicative           ((<$), (<$>), (<*>))
import Text.ParserCombinators.Parsec (Parser,many,space,sepBy,char,try,notFollowedBy,oneOf,between,noneOf,option,many1,string,digit,(<|>))

ws :: Parser String
ws = many space

type P = Parser

seq3 :: (P a, P b, P c) -> P (a,b,c)
seq3 (a,b,c) = (,,) <$> a <*> b <*> c

seq5 :: (P a, P b, P c, P d, P e) -> P (a,b,c,d,e)
seq5 (a,b,c,d,e) = (,,,,) <$> a <*> b <*> c <*> d <*> e

{-| Formula tokens -}
progT :: Parser ProgToken
formT :: Parser FormToken

-- progT -> EMPTY | formT (';' formT)*
progT = liftM ProgT $ formT `sepBy` char ';'

-- formT -> idT '=' expT
formT = do (i,_,e) <- seq3 (idT, char '=', expT); return $ FormT i e

{-| Composite expressions -}
funcT  :: Parser ExpToken
arrayT :: Parser ExpToken
objT   :: Parser ExpToken

-- COMM_SEP x = EMPTY | x (',' x)*

-- funcT -> idT '(' COMM_SEP expT ')'
funcT  = do ( i,_,es,_,wa) <- seq5 (idT, char '(', commaSep expT,  char ')', ws); return $ FuncT wa i es

-- arrayT -> '[' COMM_SEP expT ']'
arrayT = do (wb,_,es,_,wa) <- seq5 (ws,  char '[', commaSep expT,  char ']', ws); return $ ArrayT (wb,wa) es

-- objT -> '{' COMM_SEP pairT '}'
objT   = do (wb,_,ps,_,wa) <- seq5 (ws,  char '{', commaSep pairT, char '}', ws); return $ ObjT (wb,wa) ps

{-| Id element -}
idT :: Parser IdToken

-- idT -> alpha [alpha | digit]*
idT = let alpha = ['a'..'z'] ++ ['A'..'Z']
      in do (wb,i,wa) <- seq3 (ws,liftM2 (:) (oneOf alpha) $ many (oneOf alpha <|> oneOf ['0'..'9']),ws); return $ IdT (wb,wa) i
      
{-| Sequence elements -}
expT  :: Parser ExpToken
pairT :: Parser PairToken

-- expT -> nullT  |      boolT  |      numT  |      strT   |      funcT  |      arrayT  |      objT  |      varT
expT = try nullT <|> try boolT <|> try numT <|> try strT  <|> try funcT <|> try arrayT <|> try objT <|> try varT

-- pairT -> idT ':' expT
pairT = do (i,_,e) <- seq3 (idT, char ':', expT); return $ PairT i e

{-| Atomic expressions -}
varT  :: Parser ExpToken
strT  :: Parser ExpToken
numT  :: Parser ExpToken
boolT :: Parser ExpToken
nullT :: Parser ExpToken

-- varT -> idT -- Not func or pair id
varT = do i <- idT; notFollowedBy (oneOf "(:"); return $ VarT i

-- strT -> '"' ($printable - ['"'])* '"'
strT = do (wb,v,wa) <- seq3 (ws, between (char '"') (char '"') $ many $ noneOf "\"", ws); return $ StrT (wb,wa) v

-- numT -> integerS ('.' $digit+)? ([eE] integerS)?
numT = let (<:>) x y = option "" $ liftM2 (:) x y
           getNum = liftM concat $ sequence [integerS, char '.'   <:> many1 digit, oneOf "eE" <:> integerS]
       in do (wb,o,wa) <- seq3 (ws, getNum, ws); return $ NumT (wb,wa) o $ read o

-- boolT -> 'true' | 'false'
boolT = do (wb,v,wa) <- seq3 (ws,True <$ kw "true" <|> False <$ kw "false", ws); return $ BoolT (wb,wa) v

-- nullT -> 'null'
nullT = do (wb,_,wa) <- seq3 (ws, kw "null", ws); return $ NullT (wb,wa)

{-| Subatomic helpers -}
integerS :: Parser IntegerS

-- integerS -> '-'? $digit+
integerS = liftM2 (++) (option "" $ string "-") $ many1 digit

{-| Misc helpers -}
kw :: String -> Parser ()
kw s = string s >> notFollowedBy (noneOf ")}],; \v\f\t\r\n")

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` char ','

{-| Unparse the derivation tree, exactly as it was parsed -}
class Unparse a where unparse :: a -> String
instance Unparse ProgToken where unparse (ProgT fs)      = unparses' ";" fs
instance Unparse FormToken where unparse (FormT s e)     = unparse s ++ "=" ++ unparse e
instance Unparse PairToken where unparse (PairT s e)     = unparse s ++ ":" ++ unparse e
instance Unparse ExpToken  where unparse                 = unparseExp
instance Unparse IdToken   where unparse (IdT (wb,wa) s) = wb ++ s ++ wa 

unparses :: Unparse a => [a] -> String
unparses  = unparses' ","

unparses' :: Unparse a => String -> [a] -> String
unparses' sep = intercalate sep . map unparse

unparseExp :: ExpToken -> String
unparseExp (FuncT  wa i es)     = unparse i ++ "(" ++ unparses es ++ ")" ++ wa
unparseExp (ArrayT (wb,wa) es)  = wb ++ "[" ++ unparses es ++ "]" ++ wa
unparseExp (ObjT   (wb,wa) ps)  = wb ++ "{" ++ unparses ps ++ "}" ++ wa
unparseExp (VarT           v)   = unparse v
unparseExp (StrT   (wb,wa) s)   = wb ++ show s                    ++ wa
unparseExp (NumT   (wb,wa) o _) = wb ++ o                         ++ wa
unparseExp (BoolT  (wb,wa) b)   = wb ++ map toLower (show b)      ++ wa
unparseExp (NullT  (wb,wa))     = wb ++"null"                     ++ wa



















