{-
You are not required to edit this file, but do feel free to take a look. You may add some tests yourself, if you think of new ones and are able to discern the format that they should be specified in.
-}

{-
Tasty is the testing library that is used to specify tests.
The backends "tasty-hunit" and "tasty-quickcheck" specify the way that unit tests and property tests (respectively) are written.
-}
{-# LANGUAGE UndecidableInstances #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding (Failure, Success)
import Test.QuickCheck (Gen, NonEmptyList(..), elements, forAll, listOf, listOf1, shuffle, sublistOf, vectorOf, suchThat)
import Data.Char (toUpper, isSpace)
import Test.Tasty.Ingredients.Basic
import Test.Tasty.Runners
import Test.Tasty.Options
import Test.Tasty.Ingredients.ConsoleReporter
import Test.QuickCheck.Test (test)

import Backwords.Types
import Backwords.BasicGame
import Backwords.WordList
import CourseworkOne

import Data.List.Split (splitOn)

import Data.Digest.Pure.SHA (sha256, sha1)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (pack)
import Control.Monad (replicateM)
import Control.Exception (handle, SomeException, catch)
import Data.Data (Typeable, Data (toConstr))
import Data.Proxy
import System.Exit (exitSuccess)
import System.Console.ANSI (clearScreen)
import Data.List
import Data.Ratio ((%))
import Text.Printf (printf)

main :: IO ()
main = do
  clearScreen

  handle (\(_::SomeException) -> exitSuccess)
    $ defaultMainWithIngredients
      [ listingTests,
        consoleTestReporterQuiet
      ] tests

tests :: TestTree
tests = testGroup "Tests"
  [ ex1DisplayCharTests
  , ex2DisplayStringTests
  , ex3IsValidWordTests
  , ex4LetterValueTests
  , ex5ScoreWordTests
  , ex6PossibleWordsTests
  , ex7BestWordTests
  , ex8UseTilesTests
  , ex9BagDistributionTests
  , ex10AiMoveTests
  ]
prettyRational :: Rational -> String
prettyRational r = printf "%.2f" (fromRational r :: Double)

getAverageDirect :: Int -> IO Rational
getAverageDirect n = do
  scores <- replicateM n runAIDirectly
  pure $ fromIntegral (sum scores) % fromIntegral n

ex1DisplayCharTests :: TestTree
ex1DisplayCharTests =
  testGroup "Ex. 1: Display Char"
    [ testCase "Example 1 works" $
        display ('a' :: Char) @?= "+---+\n| A |\n+---+"
  , testProperty "Does not end with a newline" $
    \c -> not ("\n" `isSuffixOf` display (c :: Char))
  , testProperty "Always renders exactly three rows" $
    forAll (elements ['a'..'z'])
      $ \c -> length (lines (display (c :: Char))) == 3
    ]

ex2DisplayStringTests :: TestTree
ex2DisplayStringTests =
  testGroup "Ex. 2: Display String"
    [ testCase "Example 1 works" $
        display "hello" @?=
          "+---+ +---+ +---+ +---+ +---+\n| H | | E | | L | | L | | O |\n+---+ +---+ +---+ +---+ +---+"
  , testProperty "Does not end with a newline" $
    \xs -> not ("\n" `isSuffixOf` display (xs :: String))
  , testProperty "Renders exactly three rows for non-empty input" $
    forAll (listOf1 (elements ['a'..'z'])) $ \xs -> length (lines (display xs)) == 3
  , testCase "Empty string renders to empty string" $
      display "" @?= ""
    ]

ex3IsValidWordTests :: TestTree
ex3IsValidWordTests =
  testGroup "Ex. 3: isValidWord"
    [ testCase "Example 1 works" $
        isValidWord "Hello" @?= True
    , testCase "Example 2 works" $
        isValidWord "xyz" @?= False
    , testCase "Example 3 works" $
        isValidWord "elloh" @?= False
    , testProperty "Dictionary words are valid in any casing" $
        forAll (elements allWords) $ \w ->
          forAll (vectorOf (length w) (elements [False, True])) $ \flags ->
            let varied = zipWith (\flag c -> if flag then toUpper c else c) flags w
            in isValidWord varied
    ]


ex4LetterValueTests :: TestTree
ex4LetterValueTests =
  testGroup "Ex. 4: letterValue"
    [ testCase "Example 1 works" $
        letterValue 'a' @?= 1
    , testCase "Example 2 works" $
        letterValue ' ' @?= 0
    , testCase "Example 3 works" $
        sum (map letterValue ['a' .. 'z']) @?= 87
    ]

ex5ScoreWordTests :: TestTree
ex5ScoreWordTests =
  testGroup "Ex. 5: scoreWord"
    [ testCase "Example 1 works" $
        scoreWord "a" @?= 1
    , testCase "Example 2 works" $
        scoreWord "ab" @?= 7
    , testCase "Example 3 works" $
        scoreWord "wow" @?= 22
    , testCase "Example 4 works" $
        scoreWord "desserts" @?= 256
    , testCase "Example 5 works" $
        scoreWord "stressed" @?= 383
    ]

ex6PossibleWordsTests :: TestTree
ex6PossibleWordsTests =
  testGroup "Ex. 6: possibleWords"
    [ testCase "Example 1 works" $
        sort (possibleWords "aet") @?=
          sort ["ate", "eat", "eta", "tea"]
    , testCase "Example 2 works" $
        possibleWords "qqqqqqq" @?= []
    ]

ex7BestWordTests :: TestTree
ex7BestWordTests =
  testGroup "Ex. 7: bestWord"
    [ testCase "Example 1 works" $
        bestWord "aehrt" @?= Just "earth"
    , testCase "Example 2 works" $
        bestWord "qqqqqqq" @?= Nothing
    ]

ex8UseTilesTests :: TestTree
ex8UseTilesTests =
  testGroup "Ex. 8: useTiles"
    [ testCase "Example 1 works" $
        useTiles "abc" "abc" @?=
          [Used 'a', Used 'b', Used 'c']
    , testCase "Example 2 works" $
        useTiles "aabbcc" "abc" @?=
          [Used 'a', Unused 'a', Used 'b', Unused 'b', Used 'c', Unused 'c']
    , testCase "Example 3 works" $
        useTiles "abcabc" "cab" @?=
          [Used 'a', Used 'b', Used 'c', Unused 'a', Unused 'b', Unused 'c']
    , testProperty "useTiles uses one tile per character in the word" $
        forAll genRackAndWord $ \(rack, word) ->
          let tiles = useTiles rack word
              usedLetters = [c | Used c <- tiles]
          in length usedLetters == length word && sort usedLetters == sort word
      , testProperty "useTiles keeps tiles in original order" $
          forAll genRackAndWord $ \(rack, word) ->
            let tiles = useTiles rack word
            in map tileChar tiles == rack
    ]

tileChar :: Tile -> Char
tileChar (Used c) = c
tileChar (Unused c) = c

genRackAndWord :: Gen (String, String)
genRackAndWord = do
  rack <- listOf (elements ['a' .. 'z'])
  subset <- sublistOf rack
  word <- shuffle subset
  pure (rack, word)

ex9BagDistributionTests :: TestTree
ex9BagDistributionTests =
  testGroup "Ex. 9: bagDistribution"
    [ testCase "Example 1 works" $
        sort (bagDistribution "aabbc") @?=
          sort [('a', 2 % 5), ('b', 2 % 5), ('c', 1 % 5)]
    , testCase "Example 2 works" $
        bagDistribution "" @?= []
    , testProperty "bagDistribution probabilities sum to 1" $
        forAll (listOf1 (elements ['a' .. 'z'])) $ \bag ->
          sum (map snd (bagDistribution bag)) == 1
    ]

ex10AiMoveTests :: TestTree
ex10AiMoveTests =
  testGroup "Ex. 10: aiMove"
    [ testProperty "AI draws when rack under nine" $
        forAll (listOf (elements ['a' .. 'z'])) $ \rack ->
        forAll (listOf1 (elements ['a' .. 'z'])) $ \bag ->
          length rack < 9 ==>
            case aiMove bag rack of
              TakeVowel -> True
              TakeConsonant -> True
              _ -> False
    , testProperty "AI plays a valid word when rack is full" $
        forAll (vectorOf 9 (elements ['a' .. 'z']) `suchThat` (not . null . possibleWords)) $ \rack ->
        forAll (listOf (elements ['a' .. 'z'])) $ \bag ->
          case aiMove bag rack of
            PlayWord w -> w `elem` possibleWords rack
            _ -> False
    , testCaseSteps "AI average score sample" $ \step -> do
        let trials = 50
        avg <- getAverageDirect trials
        step $ "Average score over " ++ show trials ++ " games: " ++ prettyRational avg
    ]
-------------------------------------------------------------------------------
-- Some conveniences and boilerplate for testing.

err :: String -> c
err = errorWithoutStackTrace . ('\n':)

-- We make a special Display instance that can be overlapped. This is so that we can compile the tests before you have provided a proper one.
instance {-# OVERLAPPABLE #-} IsDisplayable a => Display a where
  display :: IsDisplayable a => a -> String
  display = err "error: no Display instance exists for this type."

-- -- Only a Cell has an IsCell instance. This prevents us from accidentally overlapping arbitrary types with our overlappable Show instance above.
class IsDisplayable a
instance IsDisplayable Char
instance IsDisplayable [Char]

-- -- | Run the test reporter. Do not add any information about how to rerun failed tests.
consoleTestReporterQuiet :: Ingredient
consoleTestReporterQuiet = TestReporter ctrOptions
  $ \opts tree ->
    let TestReporter _ cb = consoleTestReporterWithHook modifyResult
    in cb opts tree
  where
    ctrOptions :: [OptionDescription]
    ctrOptions =
      [ Option (Proxy :: Proxy Quiet)
      , Option (Proxy :: Proxy HideSuccesses)
      , Option (Proxy :: Proxy UseColor)
      , Option (Proxy :: Proxy AnsiTricks)
      ]

modifyResult :: [TestName] -> Result -> IO Result
modifyResult tests res = pure $ case resultOutcome res of
  Success -> res
  Failure {} -> res { resultDescription = dropWhileEnd isSpace rd'' }
    where
      (rd'' : _) 
        = splitOn "Use --quickcheck-replay" 
        rd'
      (rd' : _) 
          = splitOn "CallStack (from HasCallStack):" 
          $ resultDescription res


-- Compute the SHA1 hash of a string.
getSHA :: String -> String
getSHA = show . sha1 . LBS8.pack

padEnd :: Int -> Char -> String -> String
padEnd n c s = s ++ replicate (n - length s) c

