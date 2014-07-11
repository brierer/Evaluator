module Parser.Monolithic
( Unparse(unparse)
, progT,formT,pairT,idT,expT
, funcT,arrT,objT,varT,strT,numT,boolT,nullT
) where

import Prelude hiding                ((^),(>))

import Data.Char                     (toLower)
import Data.List                     (intercalate)
import Data.ExpToken                 (ProgToken(..),FormToken(..),PairToken(..),IdToken(..),ExpToken(..),IdS,IntegerS,Pos,W1,W2)
import Control.Monad                 (liftM,liftM2)
import Control.Applicative           (Applicative,(<$), (<$>), (<*>))
import Text.ParserCombinators.Parsec (Parser,many,space,sepBy,char,try,notFollowedBy,oneOf,
                                      between,noneOf,option,many1,string,digit,getPosition,sourceLine,sourceColumn,(<|>))

{-| Non expression tokens -}
-- START -> progT
-- progT -> EMPTY | formT (';' formT)*
progT = ProgT ^ pos > (formT `sepBy` char ';')

-- formT -> idT '=' expT
formT = FormT ^ idT > (char '=' >> expT)

-- pairT -> idT ':' expT
pairT = PairT ^ idT > (char ':' >> expT)

-- idT -> idS
idT = commonT IdT idS

-- expT ->                     nullT | boolT | numT | strT | funcT | arrT | objT | varT
expT = foldl1 (<|>) $ map try [nullT , boolT , numT , strT , funcT , arrT , objT , varT]

{-| Composite expressions -}
-- funcT -> idT '(' COMM_SEP expT ')'
funcT  = mkFunc ^ idT > (char '(' >> commaSep expT) > (char ')' >> ws) where mkFunc i es wa = FuncT wa i es

-- arrT -> '[' COMM_SEP expT ']'
arrT = commonT ArrT $ between (char '[') (char ']') $ commaSep expT

-- objT -> '{' COMM_SEP pairT '}'
objT   = commonT ObjT $ between (char '{') (char '}') $ commaSep pairT

{-| Atomic expressions -}
-- varT -> idT -- Not func or pair id
varT = mkVar ^ idT > notFollowedBy (oneOf "(:") where mkVar i _ = VarT i

-- strT -> '"' ($printable - ['"'])* '"'
strT = commonT StrT $ between (char '"') (char '"') $ many $ noneOf "\""

-- numT -> integerS ('.' $digit+)? ([eE] integerS)?
numT = commonT mk $ do n <- getNum; return (n,read n) where 
  mk x y    = uncurry $ NumT x y
  getNum    = liftM concat $ sequence [integerS, char '.' <:> many1 digit, oneOf "eE" <:> integerS] 
  (<:>) x y = option "" $ liftM2 (:) x y

-- boolT -> 'true' | 'false'
boolT = commonT BoolT $ True <$ kw "true" <|> False <$ kw "false"

-- nullT -> 'null'
nullT = commonT f $ kw "null" where f p w _ = NullT p w

{-| Subatomics -}
-- idS -> alpha [alpha | digit]*
idS = (:) ^ oneAlpha > many (oneAlpha <|> oneNum) where
  oneAlpha = oneOf $ ['a'..'z'] ++ ['A'..'Z']
  oneNum   = oneOf ['0'..'9']

-- integerS -> '-'? $digit+
integerS = (++) ^ option "" (string "-") > many1 digit

{-| Unparse the derivation tree, exactly as it was parsed -}
class Unparse a where unparse :: a -> String
instance Unparse ProgToken where unparse (ProgT _ fs) = unparses' ";" fs
instance Unparse FormToken where unparse (FormT s e)  = unparse s ++ "=" ++ unparse e
instance Unparse PairToken where unparse (PairT s e)  = unparse s ++ ":" ++ unparse e
instance Unparse IdToken   where unparse (IdT _ w s)  = withWS w s
instance Unparse ExpToken  where 
 unparse e = case e of
  FuncT   w i es -> withWS ("",w)  $ unparse i ++ "(" ++ unparses es ++ ")"
  ArrT  _ w es   -> withWS  w      $ "[" ++ unparses es ++ "]"
  ObjT  _ w ps   -> withWS  w      $ "{" ++ unparses ps ++ "}"
  VarT      v    -> withWS ("","") $ unparse v
  StrT  _ w s    -> withWS  w      $ show s                    
  NumT  _ w o _  -> withWS  w      o                         
  BoolT _ w b    -> withWS  w      $ map toLower (show b)      
  NullT _ w      -> withWS  w      "null"                     

{-| Utils -}
(^) = (<$>)
(>) = (<*>)
infixl 4 ^
infixl 4 >

commonT f p = mkAtom ^ ws > pos > p > ws where mkAtom wb po r wa = f po (wb,wa) r 
ws = many space
pos = ((,) ^ sourceLine > sourceColumn) ^ getPosition
kw s = string s >> notFollowedBy (noneOf ")}],; \v\f\t\r\n")
commaSep p = p `sepBy` char ','
withWS (wb,wa) v = wb ++ v ++ wa

unparses  = unparses' ","
unparses' sep = intercalate sep . map unparse

{-| Type signatures -}
progT  :: Parser ProgToken
formT  :: Parser FormToken
pairT  :: Parser PairToken
idT    :: Parser IdToken

expT  :: Parser ExpToken
funcT :: Parser ExpToken
arrT  :: Parser ExpToken
objT  :: Parser ExpToken
varT  :: Parser ExpToken
strT  :: Parser ExpToken
numT  :: Parser ExpToken
boolT :: Parser ExpToken
nullT :: Parser ExpToken

idS :: Parser IdS
integerS :: Parser IntegerS
(^) :: Functor f => (a -> b) -> f a -> f b
(>) :: Applicative f => f (a -> b) -> f a -> f b

pos      :: Parser Pos
ws       :: Parser W1
kw       :: String -> Parser ()
commonT  :: (Pos -> W2 -> a -> b) -> Parser a -> Parser b
commaSep :: Parser a -> Parser [a]
withWS   :: W2 -> String -> String

unparses   :: Unparse a => [a] -> String
unparses'  :: Unparse a => String -> [a] -> String
