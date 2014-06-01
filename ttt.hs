{-# LANGUAGE ExistentialQuantification #-}

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)
import Data.Int


data ParseState = ParseState {
      string :: L.ByteString
    , offset :: Int64           -- imported from Data.Int
    } deriving (Show)



betterParse :: ParseState -> Either String (a, ParseState)
betterParse = undefined

newtype Parse a = Parse {
      runParse :: ParseState -> Either String (a, ParseState)
    }


identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))


-- file: ch10/Parse.hs
modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset initState newOffset =
    initState { offset = newOffset }



-- file: ch10/Parse.hs
parse :: Parse a -> L.ByteString -> Either String a
parse parser initState
    = case runParse parser (ParseState initState 0) of
        Left err          -> Left err
        Right (result, _) -> Right result





data ShowBox = forall s.Test s => SB s 


class Test a where  
    test :: a -> Bool 
    tests :: Test a => a -> [Bool] 	 
    
instance Test Double where
    test d1 = True 

heteroList :: [ShowBox]
heteroList = [SB (5.0 :: Double)]

instance Test [a] where
  tests ds = map test ds	

instance Test ShowBox where
  test (SB x) = test x
