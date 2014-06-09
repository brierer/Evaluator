{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module TestEqParser where

import           Control.Monad
import           Evaluator.EqParser
import           Test.Framework
import           Test.QuickCheck
import           Text.Parsec        hiding (many, optional, (<|>))
import           Text.Parsec.String (Parser)
import qualified Text.PrettyPrint   as PP
import Text.ParserCombinators.Parsec.Error(ParseError, Message, errorMessages, messageEq)

instance Eq ParseError where
   a == b = errorMessages a == errorMessages b




prop_UnminifyValueTest :: UnminifyValue -> Bool
prop_UnminifyValueTest ast =
  case (parse unminifyValue "" (PP.render $ ppUnminifyValue ast)) of
    Left  _ -> False
    Right a -> ast == a
  where types = ast :: UnminifyValue


test_array = do
	assertEqual (parse unminifyValue "" " []\n") (Right $ UnminifyValue  "" (CompositeValue $ ArrayValue []) "\n")
	assertEqual (PP.render $ ppUnminifyValue $ (UnminifyValue  " " (CompositeValue $ ArrayValue []) "\n")) ( " []\n")


test_PrimaryValue = do	
	assertEqual (parse primaryValue "" "2.0") (Right  $ PrimaryValue $ Pnum $ 2.0)
	assertEqual (parse primaryValue "" "\"cmbn\"") (Right  $ PrimaryValue $ Pstring $ "cmbn")
	assertEqual (parse' unminifyValue "\n\"ig\"\n") (Right $ UnminifyValue "\n" (PrimaryValue (Pstring "ig")) "\n")
	
parse' f v = parse f "" v


alphaFreqList = [ (26, choose ('a', 'z'))]
digitFreqList = [ (10, choose ('0', '9')) ]
whiteLetterFreqList = listOf1 (elements ['\n'])
letters = frequency alphaFreqList
num = choose (-1000, 1000) :: Gen Double
bool = elements [False,True]
letterOrDigit = frequency $ alphaFreqList ++ digitFreqList
stringGenerator = liftM2 (:) letters $ sized (\n -> replicateM n letters)
function'  = liftM2 (,) stringGenerator (listOf _UnminifyValue)
tag'  = liftM2 (,) stringGenerator (_UnminifyValue)
obj'  = (listOf tag')

_Pvalue = oneof
       [   liftM Pstring stringGenerator,
       	   liftM Pnum     num,
       	   liftM Pbool    bool
       ]

_UnminifyValue = liftM3 UnminifyValue  whiteLetterFreqList _Pvalue' whiteLetterFreqList

_CompositeValue = oneof
		[ liftM ArrayValue (listOf _UnminifyValue),
		  liftM FunctionValue function',
		  liftM ObjValue obj'
		]

_Pvalue' = oneof
       [ liftM PrimaryValue _Pvalue
        -- liftM CompositeValue _CompositeValue
       ]


instance Arbitrary Pvalue' where
  arbitrary = _Pvalue'

instance Arbitrary UnminifyValue where
  arbitrary = _UnminifyValue

