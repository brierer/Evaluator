
module Evaluator.EqParser (
    fromDValue,
    run,
    bloc
    ) where


import Evaluator.DValue    
import Text.Parsec hiding ((<|>), many, optional)
import Text.Parsec.String (Parser)
import Control.Applicative
import Data.Char

run :: Parser [(String,DValue)] -> String -> Either String [(String,DValue)]
run p input
        = case (parse p "" input) of 
           Right x -> Right x
           Left  x  -> Left $ "Parser Error:" ++ (show x)

 
fichier :: Parser [(String,DValue)]
fichier = bloc 
 
bloc :: Parser [(String,DValue)]
bloc = many ((paire))
 
paire :: Parser (String,DValue)
paire = liftA2 (,) clef (char '=' *> valeurs)
 
--isPair :: Parser Char
--isPair = try(char '\n' <|> ( between (many space) (many space)  (many letter) ) `endBy` char '=')


--commentaire :: Parser (String,DValue)
--commentaire = liftA2 (,) ( show . sourceLine <$> getPosition) (DCom <$> (char '#' *> many (noneOf  "\n"))) 

eol :: Parser Char
eol = try(char $ ';') <?> "valid eol"
                 

clef :: Parser String
clef = between (many (eol) <|> mspace  ) (many space) (many1 letter) <?> "valid clef"
                 

valeurs :: Parser DValue
valeurs =  (valeur) <* (lookAhead(isPair) <|> eof ) 
                 
 
isPair :: Parser  ()
isPair =  (between (mspace) (mspace) (many letter)) *> (char '=') *> return () <?> "valid pair"
                 

--newLine :: Parser String
--newLine = (many1 (try(char ';') <|> try(newline))) *>  (many space) *> (many1 letter) <* (many space) <* char '='

valeur :: Parser DValue
valeur = between (jspace) (mspace) (
                 choice [ try (DBool <$> pBool)
                 ,try (DNum <$> pNum)
				 ,try (DString <$> pString)
                 ,try (DArray <$> pArray)
 				 ,try (DObj <$> pObject)
                 ,DFunction <$> pFunction ] <?> "a valid desktop value"
                 )



pArguments :: Parser [DValue]
pArguments = ( (char '('  ) *> (valeur) `sepBy` (char ',') <* (char ')') ) 

pArray :: Parser [DValue]
pArray = ( (char '['  ) *> (valeur) `sepBy` (char ',') <* (char ']') ) 

pValeurs :: Parser [DValue]
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

pObject :: Parser [(String,DValue)]
pObject = between (char '{' )  (char '}') (tag `sepBy` (char ','))

tag :: Parser (String, DValue)
tag =  (ctuple <$> tagName <*> tagValue)

tagName :: Parser String
tagName = between (many space) ( (many space) *> (char ':') ) (function)

tagValue :: Parser DValue
tagValue = valeur



pFunction :: Parser ( String , [DValue])
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
