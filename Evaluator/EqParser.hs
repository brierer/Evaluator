{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Evaluator.EqParser (
    Pvalue(..),
    run,
    bloc,
    convertAllToPureValue,
    encodeValues
    ) where

 
import Text.Parsec hiding ((<|>), many, optional)
import Text.Parsec.String (Parser)
import Control.Applicative
import Control.Monad
import Data.Char
import GHC.Generics
import Data.Aeson 
import Data.Aeson.Encode.Pretty
import Data.Text (Text, pack)

data Pvalue = Parray [Pvalue] | Pstring String | Pnum Double | Pbool Bool | Pcom String | Pfunction (String,[Pvalue]) | Pobj [(String,Pvalue)]
	deriving (Show, Generic) 

data OneOrManyValue =  ManyValue ManyValue | SingleValue Pvalue deriving (Show)

data ManyValue = ArrayValue [OneValue] | FunctionValue (String,[OneValue]) | ObjValue [(String,OneValue)] deriving (Show, Generic)

data OneValue = OneValue String OneOrManyValue String deriving (Show)

instance ToJSON Pvalue
instance ToJSON OneOrManyValue where
	toJSON (ManyValue s) = toJSON s
	toJSON (SingleValue s) = toJSON s

instance ToJSON OneValue where
	toJSON (OneValue s1 v s2) = object [ 
					     "s1"  .= s1
					   , "v"   .= v
					   , "s2"        .= s2
					   ]
instance ToJSON ManyValue where
	toJSON (ArrayValue ds) = object [ "a"  .= fmap  toJSON ds ]
	toJSON (FunctionValue (s,ds)) = object [ "f"  .= object[ "name"  .= s,"arg" .= fmap toJSON ds] ]
	toJSON (ObjValue ds) = object ["o" .= fmap toJSON ds]


encodeValues ds = encodePretty' (config ds) (object (fmap encodeValue ds))  

config :: [(String, a)] -> Config
config ds = Config { confIndent = 1, confCompare = keyConfig ds }

keyConfig :: [(String, a)] -> (Text -> Text -> Ordering)
keyConfig ds = (keyOrder $ map (pack.fst) ds)

encodeValue (s,d) =  (pack s) .= d

convertAllToPureValue :: [(String,OneValue)] -> [(String,Pvalue)] 
convertAllToPureValue ds = map (mapSnd toPureValue) ds 

toPureValue :: OneValue -> Pvalue
toPureValue (OneValue _ (ManyValue x) _) = manyValuetoPureValue x
toPureValue (OneValue _ (SingleValue x) _) =  x

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x,y) = (x,f y)

manyValuetoPureValue :: ManyValue -> Pvalue
manyValuetoPureValue (ArrayValue ds) = Parray $ map toPureValue ds
manyValuetoPureValue (FunctionValue (s,ds)) = Pfunction $ (s,map toPureValue ds)
manyValuetoPureValue (ObjValue ds) = Pobj $ zip strings values
					 where unzipValue = unzip ds
					       values = map toPureValue (snd unzipValue)
					       strings = fst unzipValue	

run :: Parser [(String,OneValue)] -> String -> Either String [(String,OneValue)]
run p input
        = case (parse p "" input) of 
           Right x -> Right x
           Left  x  -> Left $ "Parser Error:" ++ (show x)

 

bloc :: Parser [(String,OneValue)]
bloc =  many ((paire))
 
paire :: Parser (String,OneValue)
paire = liftA2 (,) clef (char '=' *> valeurs)
 
--isPair :: Parser Char
--isPair = try(char '\n' <|> ( between (many space) (many space)  (many letter) ) `endBy` char '=')


--commentaire :: Parser (String,DValue)
--commentaire = liftA2 (,) ( show . sourceLine <$> getPosition) (DCom <$> (char '#' *> many (noneOf  "\n"))) 

eol :: Parser Char
eol = try(char $ '\n') <?> "valid eol"
                 

clef :: Parser String
clef = between (mspace  ) (many space) (many1 letter) <?> "valid clef"
                 

valeurs :: Parser OneValue
valeurs =  (valeur) <* (lookAhead(isPair) <|> eof ) 
                 
 
isPair :: Parser  ()
isPair =  (between (mspace) (mspace) (many letter)) *> (char '=') *> return () <?> "valid pair"
                 

--newLine :: Parser String
--newLine = (many1 (try(char ';') <|> try(newline))) *>  (many space) *> (many1 letter) <* (many space) <* char '='

valeur :: Parser OneValue
valeur = oneOrManyValue
                 

oneOrManyValue :: Parser OneValue
oneOrManyValue = OneValue <$> pComment <*> (try(singleValue) <|> manyValue)  <*> pComment


singleValue :: Parser OneOrManyValue
singleValue = SingleValue <$> (
                 choice [ try  (Pbool <$> pBool)
                 ,try (Pnum <$> pNum)
	         ,try (Pstring <$> pString)
		]
		)


manyValue :: Parser OneOrManyValue
manyValue = ManyValue <$> (
                 choice [
                 try (ArrayValue <$> pArray),
		 try (FunctionValue <$> pFunction),
		 try (ObjValue <$> pObject)
		]
		)


comment :: Parser Pvalue
comment = (Pcom <$> pComment)

pComment :: Parser [Char]
pComment = skipMany (char ' ' <|> tab) *> many (eol <* skipMany (char ' ' <|> tab))  

pArguments :: Parser [OneValue]
pArguments = ( (char '('  ) *> (valeur) `sepBy` (char ',') <* (char ')') ) 

pArray :: Parser [OneValue]
pArray = ( (char '['  ) *> (valeur) `sepBy` (char ',') <* (char ']') ) 

pValeurs :: Parser [OneValue]
pValeurs = many valeur



pBool :: Parser Bool
pBool = (True <$ string "true" <|> False <$ string "false") 
 
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

pObject :: Parser [(String,OneValue)]
pObject = between (char '{' )  (char '}') (tag `sepBy` (char ','))

tag :: Parser (String, OneValue)
tag =  (ctuple <$> tagName <*> tagValue)

tagName :: Parser String
tagName = between (many space) ( (many space) *> (char ':') ) (function)

tagValue :: Parser OneValue
tagValue = valeur



pFunction :: Parser ( String , [OneValue])
pFunction  = ctuple <$> function <*> ((many space) *> (choice [(pArguments), (return [])] )) 

function :: Parser  String
function =  many1 (letter)


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
