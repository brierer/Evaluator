
module Evaluator.EqParser (
    Pvalue(..),
    run,
    bloc
    ) where

 
import Text.Parsec hiding ((<|>), many, optional)
import Text.Parsec.String (Parser)
import Control.Applicative
import Data.Char

data Pvalue = Parray [Pvalue] | Pstring String | Pnum Double | Pbool Bool | Pcom String | Pfunction (String,[Pvalue]) | Pobj [(String,Pvalue)]

run :: Parser [(String,Pvalue)] -> String -> Either String [(String,Pvalue)]
run p input
        = case (parse p "" input) of 
           Right x -> Right x
           Left  x  -> Left $ "Parser Error:" ++ (show x)

 
fichier :: Parser [(String,Pvalue)]
fichier = bloc 
 
bloc :: Parser [(String,Pvalue)]
bloc = many ((paire))
 
paire :: Parser (String,Pvalue)
paire = liftA2 (,) clef (char '=' *> valeurs)
 
--isPair :: Parser Char
--isPair = try(char '\n' <|> ( between (many space) (many space)  (many letter) ) `endBy` char '=')


--commentaire :: Parser (String,DValue)
--commentaire = liftA2 (,) ( show . sourceLine <$> getPosition) (DCom <$> (char '#' *> many (noneOf  "\n"))) 

eol :: Parser Char
eol = try(char $ ';') <?> "valid eol"
                 

clef :: Parser String
clef = between (many (eol) <|> mspace  ) (many space) (many1 letter) <?> "valid clef"
                 

valeurs :: Parser Pvalue
valeurs =  (valeur) <* (lookAhead(isPair) <|> eof ) 
                 
 
isPair :: Parser  ()
isPair =  (between (mspace) (mspace) (many letter)) *> (char '=') *> return () <?> "valid pair"
                 

--newLine :: Parser String
--newLine = (many1 (try(char ';') <|> try(newline))) *>  (many space) *> (many1 letter) <* (many space) <* char '='

valeur :: Parser Pvalue
valeur = between (jspace) (mspace) (
                 choice [ try (Pbool <$> pBool)
                 ,try (Pnum <$> pNum)
	         ,try (Pstring <$> pString)
                 ,try (Parray <$> pArray)
 		 ,try (Pobj <$> pObject)
                 ,Pfunction <$> pFunction ] <?> "a function"
                 )



pArguments :: Parser [Pvalue]
pArguments = ( (char '('  ) *> (valeur) `sepBy` (char ',') <* (char ')') ) 

pArray :: Parser [Pvalue]
pArray = ( (char '['  ) *> (valeur) `sepBy` (char ',') <* (char ']') ) 

pValeurs :: Parser [Pvalue]
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

pObject :: Parser [(String,Pvalue)]
pObject = between (char '{' )  (char '}') (tag `sepBy` (char ','))

tag :: Parser (String, Pvalue)
tag =  (ctuple <$> tagName <*> tagValue)

tagName :: Parser String
tagName = between (many space) ( (many space) *> (char ':') ) (function)

tagValue :: Parser Pvalue
tagValue = valeur



pFunction :: Parser ( String , [Pvalue])
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
