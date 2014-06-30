module Parser.Monolithic
( Unparse(unparse)
, progT
, formT
, pairT
, idT
, expT
, funcT
, arrayT
, objT
, varT
, strT
, numT
, boolT
, nullT
) where

import Data.Char                     (toLower)
import Data.List                     (intercalate)
import Data.Token                    (ProgToken(..),FormToken(..),PairToken(..),IdToken(..),ExpToken(..),IntegerS)
import Control.Monad                 (liftM,liftM2)
import Control.Applicative           ((<$), (<$>), (<*>))
import Text.ParserCombinators.Parsec (Parser,many,space,sepBy,char,try,notFollowedBy,oneOf,
                                      between,noneOf,option,many1,string,digit,getPosition,sourceLine,sourceColumn,(<|>))


{-| Formal grammar 

START -> progT

progT -> EMPTY | formT (';' formT)*
formT -> idT '=' expT

expT  -> nullT | boolT | numT | strT | funcT | arrayT | objT | varT

funcT  -> idT '(' COMM_SEP expT ')'
arrayT -> '[' COMM_SEP expT ']'
objT   -> '{' COMM_SEP pairT '}'
varT   -> idT NOT_FOLLOWED_BY [(:]
strT   -> '"' (ASCII BUT '"')* '"'
numT   -> integerS ('.' DIGIT+)? ([eE] integerS)?
boolT  -> 'true' | 'false'
nullT  -> 'null'

pairT       -> idT ':' expT
idT         -> ALPHA (ALPHA | DIGIT)*
integerS   -> '-'? $digit+
COMM_SEP x -> EMPTY | x (',' x)*

-}

{-| Non expression tokens -}
progT :: Parser ProgToken
formT :: Parser FormToken
pairT :: Parser PairToken
idT   :: Parser IdToken
expT  :: Parser ExpToken

-- progT -> EMPTY | formT (';' formT)*
progT = do p <- pos; liftM (ProgT p) $ formT `sepBy` char ';'

-- formT -> idT '=' expT
formT = do (_,i,_,e) <- seq4 (pos, idT, char '=', expT); return $ FormT i e

-- pairT -> idT ':' expT
pairT = do (p,i,_,e) <- seq4 (pos, idT, char ':', expT); return $ PairT p i e

-- idT -> alpha [alpha | digit]*
idT = let alpha = ['a'..'z'] ++ ['A'..'Z']
      in do (wb,p,i,wa) <- seq4 (ws,pos,liftM2 (:) (oneOf alpha) $ many (oneOf alpha <|> oneOf ['0'..'9']),ws); return $ IdT p (wb,wa) i
      
-- expT -> nullT  |      boolT  |      numT  |      strT   |      funcT  |      arrayT  |      objT  |      varT
expT = try nullT <|> try boolT <|> try numT <|> try strT  <|> try funcT <|> try arrayT <|> try objT <|> try varT

{-| Composite expressions -}
funcT  :: Parser ExpToken
arrayT :: Parser ExpToken
objT   :: Parser ExpToken

-- COMM_SEP x = EMPTY | x (',' x)*

-- funcT -> idT '(' COMM_SEP expT ')'
funcT  = do ( _,i,_,es,_,wa) <- seq6 (pos, idT, char '(', commaSep expT,  char ')', ws); return $ FuncT wa i es

-- arrayT -> '[' COMM_SEP expT ']'
arrayT = do (wb,p,_,es,_,wa) <- seq6 (ws, pos,  char '[', commaSep expT,  char ']', ws); return $ ArrayT p (wb,wa) es

-- objT -> '{' COMM_SEP pairT '}'
objT   = do (wb,p,_,ps,_,wa) <- seq6 (ws, pos,  char '{', commaSep pairT, char '}', ws); return $ ObjT p (wb,wa) ps

{-| Atomic expressions -}
varT  :: Parser ExpToken
strT  :: Parser ExpToken
numT  :: Parser ExpToken
boolT :: Parser ExpToken
nullT :: Parser ExpToken

-- varT -> idT -- Not func or pair id
varT = do i <- idT; notFollowedBy (oneOf "(:"); return $ VarT i

-- strT -> '"' ($printable - ['"'])* '"'
strT = do (wb,p,v,wa) <- seq4 (ws, pos, between (char '"') (char '"') $ many $ noneOf "\"", ws); return $ StrT p (wb,wa) v

-- numT -> integerS ('.' $digit+)? ([eE] integerS)?
numT = let (<:>) x y = option "" $ liftM2 (:) x y
           getNum = liftM concat $ sequence [integerS, char '.'   <:> many1 digit, oneOf "eE" <:> integerS]
       in do (wb,p,o,wa) <- seq4 (ws, pos, getNum, ws); return $ NumT p (wb,wa) o $ read o

-- boolT -> 'true' | 'false'
boolT = do (wb,p,v,wa) <- seq4 (ws,pos,True <$ kw "true" <|> False <$ kw "false", ws); return $ BoolT p (wb,wa) v

-- nullT -> 'null'
nullT = do (wb,p,_,wa) <- seq4 (ws, pos, kw "null", ws); return $ NullT p (wb,wa)

{-| Subatomic helpers -}
integerS :: Parser IntegerS

-- integerS -> '-'? $digit+
integerS = liftM2 (++) (option "" $ string "-") $ many1 digit

{-| Misc helpers -}
kw :: String -> Parser ()
kw s = string s >> notFollowedBy (noneOf ")}],; \v\f\t\r\n")

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` char ','

ws :: Parser String
ws = many space

pos :: Parser (Int,Int)
pos = liftM ((,) <$> sourceLine <*> sourceColumn) getPosition

type P = Parser

seq4 :: (P a, P b, P c, P d) -> P (a,b,c,d)
seq4 (a,b,c,d) = (,,,) <$> a <*> b <*> c <*> d

seq6 :: (P a, P b, P c, P d, P e, P f) -> P (a,b,c,d,e,f)
seq6 (a,b,c,d,e,f) = (,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f

{-| Unparse the derivation tree, exactly as it was parsed -}
class Unparse a where unparse :: a -> String
instance Unparse ProgToken where unparse (ProgT _ fs)      = unparses' ";" fs
instance Unparse FormToken where unparse (FormT   s e)     = unparse s ++ "=" ++ unparse e
instance Unparse PairToken where unparse (PairT _ s e)     = unparse s ++ ":" ++ unparse e
instance Unparse ExpToken  where unparse                   = unparseExp
instance Unparse IdToken   where unparse (IdT _ (wb,wa) s) = wb ++ s ++ wa 

unparses :: Unparse a => [a] -> String
unparses  = unparses' ","

unparses' :: Unparse a => String -> [a] -> String
unparses' sep = intercalate sep . map unparse

unparseExp :: ExpToken -> String
unparseExp (FuncT     wa i es)    = unparse i ++ "(" ++ unparses es ++ ")" ++ wa
unparseExp (ArrayT _ (wb,wa) es)  = wb ++ "[" ++ unparses es ++ "]" ++ wa
unparseExp (ObjT   _ (wb,wa) ps)  = wb ++ "{" ++ unparses ps ++ "}" ++ wa
unparseExp (VarT             v)   = unparse v
unparseExp (StrT   _ (wb,wa) s)   = wb ++ show s                    ++ wa
unparseExp (NumT   _ (wb,wa) o _) = wb ++ o                         ++ wa
unparseExp (BoolT  _ (wb,wa) b)   = wb ++ map toLower (show b)      ++ wa
unparseExp (NullT  _ (wb,wa))     = wb ++"null"                     ++ wa



















