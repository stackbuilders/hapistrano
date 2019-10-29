module System.HapistranoPropsSpec
  ( spec
  ) where

import Data.Char (isSpace)
import System.Hapistrano.Commands.Internal (quoteCmd, trim)
import Test.Hspec hiding (shouldBe, shouldReturn)
import Test.QuickCheck

spec :: Spec
spec =
  describe "QuickCheck" $
  context "Properties" $ do
    it "property of quote command" $ property propQuote'
    it "property of trimming a command" $
      property $ forAll trimGenerator propTrim'

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

-- | Trim String Generator
trimGenerator :: Gen String
trimGenerator =
  let strGen = listOf arbitraryUnicodeChar
   in frequency
        [ (1, suchThat strGen isTrimmed')
        , (1, suchThat strGen (not . isTrimmed'))
        ]
