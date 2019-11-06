module System.HapistranoPropsSpec
  ( spec
  ) where

import Data.Char (isSpace)
import System.Hapistrano.Commands.Internal
  ( mkGenericCommand
  , quoteCmd
  , trim
  , unGenericCommand
  )
import Test.Hspec hiding (shouldBe, shouldReturn)
import Test.QuickCheck

spec :: Spec
spec =
  describe "QuickCheck" $
  context "Properties" $ do
    it "property of quote command" $ property propQuote'
    it "property of trimming a command" $
      property $ forAll trimGenerator propTrim'
    it "property of mkGenericCommand and unGenericCommand" $
      property $ forAll genericCmdGenerator propGenericCmd'

-- Is quoted determine
isQuoted :: String -> Bool
isQuoted str = head str == '"' && last str == '"'

-- | Quote function property
propQuote :: String -> Bool
propQuote str =
  if any isSpace str
    then isQuoted $ quoteCmd str
    else quoteCmd str == str

propQuote' :: String -> Property
propQuote' str =
  classify (any isSpace str) "has at least a space" $ propQuote str

-- | Is trimmed
isTrimmed' :: String -> Bool
isTrimmed' [] = True
isTrimmed' [_] = True
isTrimmed' str =
  let a = not . isSpace $ head str
      b = not . isSpace $ last str
   in a && b

-- | Prop trimm
propTrim :: String -> Bool
propTrim = isTrimmed' . trim

propTrim' :: String -> Property
propTrim' str =
  classify (not $ isTrimmed' str) "non trimmed strings" $ propTrim str

-- | Check that the string is perfect command String
isCmdString :: String -> Bool
isCmdString str = all ($str) [not . null, notElem '#', notElem '\n', isTrimmed']

-- | Prop Generic Command
-- If the string does not contain # or \n, is trimmed and non null, the command should be created 
propGenericCmd :: String -> Bool
propGenericCmd str =
  if isCmdString str
    then maybe False ((== str) . unGenericCommand) (mkGenericCommand str)
    else maybe True ((/= str) . unGenericCommand) (mkGenericCommand str) -- Either the command cannot be created or the command str is different to the original

propGenericCmd' :: String -> Property
propGenericCmd' str =
  classify (isCmdString str) "perfect command string" propGenericCmd

-- | Trim String Generator
trimGenerator :: Gen String
trimGenerator =
  let strGen = listOf arbitraryUnicodeChar
   in frequency
        [ (1, suchThat strGen isTrimmed')
        , (1, suchThat strGen (not . isTrimmed'))
        ]

-- | Generic Command generator
genericCmdGenerator :: Gen String
genericCmdGenerator =
  let strGen = listOf arbitraryUnicodeChar
   in frequency
        [(1, suchThat strGen isCmdString), (1, suchThat strGen (elem '#'))]
