{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Evaluator.EqParser where


import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Char
import           Data.Text                (Text, pack)
import           GHC.Generics
import           Text.Parsec              hiding (many, optional, (<|>))
import           Text.Parsec.String       (Parser)
import qualified Text.PrettyPrint         as PP


{-|
The 'Pvalue' type represents values with parse with parsec.
The structure is JSON-like except for Pfunction which add  
a function data type.
-}
data Pvalue = Parray [Pvalue] | Pstring String | Pnum Double | Pbool Bool | Pcom String | Pfunction (String,[Pvalue]) | Pobj [(String,Pvalue)]
        deriving (Show, Generic, Eq)

{-|
The Pvalue' type is a clone of the Pvalue structure
with white characters unminified. This is the structure to be parsed.
-}
data Pvalue' =  CompositeValue CompositeValue | PrimaryValue Pvalue deriving (Show, Eq)


{-|
The CompositeValue type represents Composite JSON-Type Data
added of the function data type. 
-}
data CompositeValue = ArrayValue [UnminifyValue] | 
                 FunctionValue (String,[UnminifyValue]) | 
                 ObjValue [(String,UnminifyValue)] 
                 deriving (Show, Generic, Eq)

{-|
The UnminifyValue type represents a Pvalue with white characters. 
-}
data UnminifyValue = UnminifyValue String Pvalue' String deriving (Show, Eq)

instance ToJSON Pvalue
instance ToJSON Pvalue' where
        toJSON (CompositeValue s) = toJSON s
        toJSON (PrimaryValue s) = toJSON s

instance ToJSON UnminifyValue where
        toJSON (UnminifyValue s1 v s2) = object [
                                             "s1"  .= s1
                                           , "v"   .= v
                                           , "s2"  .= s2
                                           ]
instance ToJSON CompositeValue where
        toJSON (ArrayValue ds) = object [ "a"  .= fmap  toJSON ds ]
        toJSON (FunctionValue (s,ds)) = object [ "f"  .= object[ "name"  .= s,"arg" .= fmap toJSON ds] ]
        toJSON (ObjValue ds) = object ["o" .= fmap toJSON ds]


prettyPrint (Pstring s) =  PP.doubleQuotes $ PP.text s
prettyPrint (Pnum d) =  PP.double d
prettyPrint (Pbool d) = PP.text $ show d

ppPvalue' (PrimaryValue p) = prettyPrint p
ppPvalue' (CompositeValue ps) = ppCompositeValue ps

ppCompositeValue (ArrayValue ps) = PP.brackets $ PP.sep $ map  ppUnminifyValue ps

ppUnminifyValue (UnminifyValue s1 p s2 )=  (PP.text s1) PP.<> (ppPvalue' p) PP.<> (PP.text s2)

encodeValues ds = encodePretty  ((ds))


keyConfig :: [(String, a)] -> (Text -> Text -> Ordering)
keyConfig ds = (keyOrder $ map (pack.fst) ds)

encodeValue (s,d) =  object ["tag" .= s, "value" .= d]

convertAllToPureValue :: [(String,UnminifyValue)] -> [(String,Pvalue)]
convertAllToPureValue ds = map (mapSnd toPureValue) ds

toPureValue :: UnminifyValue -> Pvalue
toPureValue (UnminifyValue _ (CompositeValue x) _) = deleteWhite x
toPureValue (UnminifyValue _ (PrimaryValue x) _) =  x

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x,y) = (x,f y)

deleteWhite :: CompositeValue -> Pvalue
deleteWhite (ArrayValue ds) = Parray $ map toPureValue ds
deleteWhite (FunctionValue (s,ds)) = Pfunction $ (s,map toPureValue ds)
deleteWhite (ObjValue ds) = Pobj $ zip strings values
                                         where unzipValue = unzip ds
                                               values = map toPureValue (snd unzipValue)
                                               strings = fst unzipValue

run :: Parser [(String,UnminifyValue)] -> String -> Either String [(String,UnminifyValue)]
run p input
        = case (parse p "" input) of
           Right x -> Right x
           Left  x  -> Left $ "Parser Error:" ++ (show x)



bloc :: Parser [(String,UnminifyValue)]
bloc =  many ((paire))

paire :: Parser (String,UnminifyValue)
paire = liftA2 (,) clef (char '=' *> valeurs)

--isPair :: Parser Char
--isPair = try(char '\n' <|> ( between (many space) (many space)  (many letter) ) `endBy` char '=')


--commentaire :: Parser (String,DValue)
--commentaire = liftA2 (,) ( show . sourceLine <$> getPosition) (DCom <$> (char '#' *> many (noneOf  "\n")))

eol :: Parser Char
eol = try(char $ '\n') <?> "valid eol"


clef :: Parser String
clef = between (mspace  ) (many space) (many1 letter) <?> "valid clef"


valeurs :: Parser UnminifyValue
valeurs =  (valeur) <* (lookAhead(isPair) <|> eof )


isPair :: Parser  ()
isPair =  (between (mspace) (mspace) (many letter)) *> (char '=') *> return () <?> "valid pair"


--newLine :: Parser String
--newLine = (many1 (try(char ';') <|> try(newline))) *>  (many space) *> (many1 letter) <* (many space) <* char '='

valeur :: Parser UnminifyValue
valeur = unminifyValue


unminifyValue :: Parser UnminifyValue
unminifyValue = UnminifyValue <$> pWhite <*> (try(primaryValue) <|> compositeValue)  <*> pWhite


primaryValue :: Parser Pvalue'
primaryValue = PrimaryValue <$> (
                 choice [ try  (Pbool <$> pBool)
                 ,try (Pnum <$> pNum)
                 ,try (Pstring <$> pString)
                ]
                )


compositeValue :: Parser Pvalue'
compositeValue = CompositeValue <$> (
                 choice [
                 try (ArrayValue <$> pArray),
                 try (FunctionValue <$> pFunction),
                 try (ObjValue <$> pObject)
                ]
                )


comment :: Parser Pvalue
comment = (Pcom <$> pWhite)

pWhite :: Parser [Char]
pWhite = skipMany (space') *> many (eol <* skipMany (space'))

pArguments :: Parser [UnminifyValue]
pArguments = ( (char '('  ) *> (valeur) `sepBy` (char ',') <* (char ')') )

pArray :: Parser [UnminifyValue]
pArray = ( (char '['  ) *> (valeur) `sepBy` (char ',') <* (char ']') )

pValeurs :: Parser [UnminifyValue]
pValeurs = many valeur



pBool :: Parser Bool
pBool = (True <$ string "True" <|> False <$ string "False")

pNum :: Parser Double
pNum = float


pString :: Parser [Char]
pString = between (char $ chr(34)) (char $ chr(34)) (many letter)


mspace :: Parser [Char]
mspace = many (space)

jspace :: Parser [Char]
jspace = many  ( space)


ctuple :: a -> b -> (a,b)
ctuple a b =  (a,b)

pObject :: Parser [(String,UnminifyValue)]
pObject = between (char '{' )  (char '}') (tag `sepBy` (char ','))

tag :: Parser (String, UnminifyValue)
tag =  (ctuple <$> tagName <*> tagValue)

tagName :: Parser String
tagName = between (many space) ( (many space) *> (char ':') ) (function)

tagValue :: Parser UnminifyValue
tagValue = valeur



pFunction :: Parser ( String , [UnminifyValue])
pFunction  = ctuple <$> function <*> ((many space') *> (choice [(pArguments), (return [])] ))

function :: Parser  String
function =  many1 (letter)

space' = char ' ' <|> tab

valideChar = (['a'..'z'] ++
              ['A'..'Z'])

(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b

number ::  Parser [Char]
number =  many1 digit

plus = char '+' *> number

minus = char '-' <:> number

integer = plus <|> minus <|> number

float :: Parser  Double
float =  fmap rd $ integer <++> decimal <++> exponent
      where
        rd       = read :: String -> Double
        decimal  = option "" $ char '.' <:> number
        exponent = option "" $ oneOf "eE" <:> integer
