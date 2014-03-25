
module Evaluator.MyParse (
    fromDValeur,
    typeOfDValeur,
    run,
    bloc
    ) where


import Evaluator.DValeur    
import Text.Parsec hiding ((<|>), many, optional)
import Text.Parsec.String (Parser)
import Control.Applicative
import Numeric
import Data.Char

run :: Parser [(String,DValeur)] -> String -> Either String [(String,DValeur)]
run p input
        = case (parse p "" input) of 
           Right x -> Right x
           Left  x  -> Left $ "Parser Error:" ++ (show x)

 
fichier :: Parser [(String,DValeur)]
fichier = bloc 
 
bloc :: Parser [(String,DValeur)]
bloc = many ((paire))
 
paire :: Parser (String,DValeur)
paire = liftA2 (,) clef (char '=' *> valeurs)
 
--isPair :: Parser Char
--isPair = try(char '\n' <|> ( between (many space) (many space)  (many letter) ) `endBy` char '=')


--commentaire :: Parser (String,DValeur)
--commentaire = liftA2 (,) ( show . sourceLine <$> getPosition) (DCom <$> (char '#' *> many (noneOf  "\n"))) 

eol :: Parser Char
eol = try(char $ ';') <?> "valid eol"
                 

clef :: Parser String
clef = between (many (eol) <|> mspace  ) (many space) (many1 letter) <?> "valid clef"
                 

valeurs :: Parser DValeur
valeurs =  (valeur) <* (lookAhead(isPair) <|> eof ) 
                 
 
isPair :: Parser  ()
isPair =  (between (mspace) (mspace) (many letter)) *> (char '=') *> return () <?> "valid pair"
                 

--newLine :: Parser String
--newLine = (many1 (try(char ';') <|> try(newline))) *>  (many space) *> (many1 letter) <* (many space) <* char '='

valeur :: Parser DValeur
valeur = between (jspace) (mspace) (
                 choice [ try (DBool <$> pBool)
                 ,try (DNum <$> pNum)
				 ,try (DString <$> pString)
                 ,try (DArray <$> pArray)
 				 ,try (DObj <$> pObject)
                 ,DFunction <$> pFunction ] <?> "a valid desktop value"
                 )



pArguments :: Parser [DValeur]
pArguments = ( (char '('  ) *> (valeur) `sepBy` (char ',') <* (char ')') ) 

pArray :: Parser [DValeur]
pArray = ( (char '['  ) *> (valeur) `sepBy` (char ',') <* (char ']') ) 

pValeurs :: Parser [DValeur]
pValeurs = many valeur



pBool :: Parser Bool
pBool = (True <$ string "true" <|> False <$ string "false") 
 
pNum :: Parser Double
pNum = float 


pString :: Parser [Char]
pString = between (char $ chr(34)) (char $ chr(34)) (many letter)

      
mspace :: Parser [Char]
mspace = many (space <|> newline) 

jspace :: Parser [Char]
jspace = many  ( space)


ctuple :: a -> b -> (a,b)
ctuple a b =  (a,b)

pObject :: Parser [(String,DValeur)]
pObject = between (char '{' )  (char '}') (tag `sepBy` (char ','))

tag :: Parser (String, DValeur)
tag =  (ctuple <$> tagName <*> tagValue)

tagName :: Parser String
tagName = between (many space) ( (many space) *> (char ':') ) (function)

tagValue :: Parser DValeur
tagValue = valeur



pFunction :: Parser ( String , [DValeur])
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
