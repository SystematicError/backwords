-- Modified `package.yaml`

module CourseworkOne where

import Backwords.Types
import Backwords.WordList

import Data.List
import Data.Char
import Data.Ratio

import Data.Function ((&))
import Data.Ord (comparing, Down(..))
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (fromJust, isNothing)

-- For keeping count of characters in a string
import qualified Data.Map as Map

-- For efficiently storing and querying words
-- Requires the `bytestring` and `bytestring-trie` packages
import qualified Data.Trie as Trie
import qualified Data.ByteString.Char8 as ByteString

-- For generating unique Monte Carlo simulations
-- Requires the `random-shuffle` package
import System.Random (mkStdGen)
import System.Random.Shuffle (shuffle')

-- For parallelising simulations
-- Requires the `parallel` package
import Control.Parallel.Strategies (parMap, rseq)

{-
General Strategy
----------------

Since `aiMove` is stateless, the letter taking strategy and word playing strategy should be
independent from each other.

When taking a letter, the AI tries to maintain an ideal vowel to consonant ratio. For the given
dictionary, this ratio is around 4:5.

The word playing strategy is more elaborate, it performs static evaluation and then Monte Carlo
simulations to find the optimal word to play. This is inspired by Scrabble engines like Quackle.

1. For a given rack, the AI calculates an evaluation for all possible words it can play. The
   evaluation function is calculated using two other scoring functions:
   - `scoreWord`, which scores the word being played
   - `scoreRackLeave`, which scores the rack leave (the rack letters left after playing a word)

2. From the set of all possible words, the AI selects a few candidates with high evaluations.

3. For each candidate word, the AI simulates playing the next few words

4. The AI does numerous random simulations for every word, and plays the word with the highest
   average simulation score.

Results
-------

The AI's behaviour can be adjusted using some parameters defined before the main code. Tweaking
these makes the AI play stronger (at the cost of time) or quicker (at the cost of score). The
defaults I have given provide a reasonable balance.

On the DCS systems, running 70 games takes around 20 mins with an average score of 2842.65.


Optimisations
-------------

The program stores the dictionary as a Trie, which is designed to efficiently store and search
from a long list of words. This provides major speedups to functions like `isValidWord` and
`possibleWords`.

Running numerous simulations is computationally expensive, so simulations across different words are
parallelized to make the most of the given hardware. This also provides a decent speedup.

Relevant benchmarks highlighting the significant performance gains can be found at the bottom of
this file.

Additionally, I have added added the `-O2` compiler optimisation flag in `package.yaml`.

References
----------

Andrew W. Appel, Guy J. Jacobson - "The World's Fastest Scrabble Program"
https://www.cs.cmu.edu/afs/cs/academic/class/15451-s06/www/lectures/scrabble.pdf
    Shows how Tries / DAWGs can be used for word search efficiency

Jason Katz-Brown, John O'Laughlin - "How Quackle Plays Scrabble"
https://people.csail.mit.edu/jasonkb/quackle/doc/how_quackle_plays_scrabble.html
    Describes the general strategy used by Quackle to play Scrabble efficiently

Kenji Matsumoto - "Breaking the Computer Series"
http://www.breakingthegame.net/computerseries
    Dives into nuances of rack evaluation and simulations
-}

-- The number of letters held by the rack
rackSize = 9

-- How many vowels the AI tries to ensure is in the rack
-- For the given dictionary, 4 seems to provide optimal results
idealVowelCount = 4

-- The number of words possible words which the AI picks to run simulations on
-- Increasing this gives better scores, but worse performance
candidateWordCount = 20

-- The coefficient applied to word score in the evaluation function
-- Increasing this makes the AI more greedy, playing higher score words
wordScoreCoefficient = 17

-- The coefficient applied to rack leave score in the evaluation function
-- Increasing this means the AI more conservative, saving good letters for later
leaveScoreCoefficient = 4

-- For the given dictionary, the 17:4 coefficient ratio seems to provide optimal results

-- The number of simulations run for each candidate word
-- Increasing this gives better scores, but worse performance
simulationCount = 100

-- How many moves ahead the AI simulates
-- Increasing this makes the AI plan more long term
simulationDepth = 2

-- Due to the "branching factor" of possible states, `simulationCount` should be increased
-- proportionally to `simulationDepth` to get a good sample size.

-- Ex. 1:
-- Display a character
instance Display Char where
  display letter = "+---+\n| " ++ [toUpper letter] ++ " |\n+---+"

-- Ex. 2:
-- Display a list of characters
instance Display [Char] where
  display letters =
    letters
      & map (lines . display)
      & transpose
      & map unwords
      & intercalate "\n"

-- Store all the words as a Trie
-- The words are converted to ByteStrings, as that's what the data structure requires
-- The underlying data structure is technically a TrieMap, but we aren't using it
allWordsTrie :: Trie.Trie ()
-- (HLINT: Anti-pattern) Packing is fine here, every dictionary word's characters are single byte
allWordsTrie = Trie.fromList $ map (\word -> (ByteString.pack word, ())) allWords

-- Ex. 3:
-- Determine if a word is valid
isValidWord :: String -> Bool
-- (HLINT: Anti-pattern) Packing is fine here, every dictionary word's characters are single byte
isValidWord word = ByteString.pack (map toLower word) `Trie.member` allWordsTrie

-- Ex. 4:
-- Determine the points value of a letter
letterValue :: Char -> Int
letterValue letter = case toLower letter of
  'a' -> 1
  'b' -> 3
  'c' -> 3
  'd' -> 2
  'e' -> 1
  'f' -> 4
  'g' -> 2
  'h' -> 4
  'i' -> 1
  'j' -> 8
  'k' -> 5
  'l' -> 1
  'm' -> 3
  'n' -> 1
  'o' -> 1
  'p' -> 3
  'q' -> 10
  'r' -> 1
  's' -> 1
  't' -> 1
  'u' -> 1
  'v' -> 4
  'w' -> 4
  'x' -> 8
  'y' -> 4
  'z' -> 10
  _ -> 0

-- Ex. 5:
-- Score a word according to the Backwords scoring system
scoreWord :: String -> Int
scoreWord [] = 0
scoreWord (l : ls) = letterValue l + 2 * scoreWord ls

-- Ex. 6:
-- Get all words that can be formed from the given letters
possibleWords :: [Char] -> [String]
possibleWords rack =
  let -- Represent a rack as a map containing every letter and their counts
      -- (HLINT: Anti-pattern) Packing is fine here, every dictionary word's characters are single byte
      countLetters rack = Map.fromListWith (+) $ map (\letter -> (ByteString.pack [letter], 1)) rack

      -- Decrement a letters count, delete the entry when it hits 0
      decrementCount count = if count > 1 then Just (count - 1) else Nothing

      -- Does a Depth First Search into a Trie for possible words
      findWords prefix rackCounts trie =
        let words =
              if Map.null rackCounts || Trie.null trie
                then
                  -- Stop if we run out rack letters or if there are no more words to find
                  []
                else
                  -- Otherwise recurse into the branches down
                  concat
                    [ findWords prefix' rackCounts' trie'
                    | letter <- Map.keys rackCounts,
                      let prefix' = prefix <> letter,
                      let rackCounts' = Map.update decrementCount letter rackCounts,
                      let trie' = Trie.submap prefix trie
                    ]
         in -- All valid words, including the prefix if it is valid
            [prefix | prefix `Trie.member` trie] ++ words
   in map ByteString.unpack $
        findWords
          ByteString.empty
          (countLetters rack)
          allWordsTrie

-- Ex. 7:
-- Given a set of letters, find the highest scoring word that can be formed from them.
bestWord :: [Char] -> Maybe String
bestWord letters = maximumBy (comparing scoreWord) <$> nonEmpty (possibleWords letters)

-- Ex. 8:
-- Given a list of letters, and a word, mark as used all letters in the list that appear in the word.
useTiles :: [Char] -> String -> [Tile]
useTiles [] _ = []
useTiles (l : ls) word
  | l `elem` word = Used l : useTiles ls (delete l word)
  | otherwise = Unused l : useTiles ls word

-- Ex. 9:
-- Given a nonempty bag of possible letters as a list, return the chance of drawing
-- each letter.
bagDistribution :: [Char] -> [(Char, Rational)]
bagDistribution letters =
  letters
    & map (,1)
    & Map.fromListWith (+)
    & Map.map (% letterCount)
    & Map.toList
  where
    -- (HLINT: Infinite) Length should evaluate properly as the function is ran over finite lists
    letterCount = toInteger (length letters)

-- Count the number of vowels in a list
countVowels :: [Char] -> Int
-- (HLINT: Infinite) Length should evaluate properly as the function is ran over finite lists
countVowels letters = length $ filter (`elem` vowels) letters

-- Count the number of consonants in a list
countConsonants :: [Char] -> Int
-- (HLINT: Infinite) Length should evaluate properly as the function is ran over finite lists
countConsonants letters = length $ filter (`elem` consonants) letters

-- Take a letter, trying to maintain the ideal ratio
takeLetter :: [Char] -> [Char] -> Move
takeLetter bag rack
  | vowelPresent && not consonantPresent = TakeVowel
  | consonantPresent && not vowelPresent = TakeConsonant
  | otherwise = if countVowels rack < idealVowelCount then TakeVowel else TakeConsonant
  where
    vowelPresent = countVowels bag > 0
    consonantPresent = countConsonants bag > 0

-- The value of each letter in a rack leave
-- I determined these numbers through static analysis and some manual tweaking
-- This accounts for:
--   - Frequency: how often do we use this letter?
--   - Versatility: does this letter occur in various positions across different words?
--   - Positionality: how far to the right of the word does this letter occur in?
leaveLetterValue :: Char -> Int
leaveLetterValue letter = case toLower letter of
  's' -> 200
  'y' -> 175
  'e' -> 168
  'd' -> 148
  'g' -> 117
  'n' -> 80
  't' -> 78
  'r' -> 77
  'c' -> 77
  'm' -> 65
  'h' -> 64
  'k' -> 61
  'i' -> 58
  'l' -> 51
  'a' -> 46
  'o' -> 35
  'p' -> 32
  'f' -> 22
  'b' -> 20
  'w' -> 19
  'v' -> 17
  'u' -> 16
  'x' -> 15
  'z' -> 13
  'q' -> 2
  'j' -> 1
  _ -> 0

-- Calculate a score for the rack leave by averaging the leave letter values
scoreRackLeave :: [Char] -> Int
scoreRackLeave rackLeave =
  let scores = map leaveLetterValue rackLeave
   in -- (HLINT: Infinite) Both sum and length should evaluate properly as the function is ran
      -- over finite lists
      sum scores `div` (1 `max` length scores)

-- Evaluate a given rack a word to be played off of it
evaluate :: [Char] -> String -> Int
evaluate rack word =
  let rackLeave = rack \\ word
   in wordScoreCoefficient * scoreWord word
        + leaveScoreCoefficient * scoreRackLeave rackLeave

-- Given a set of letters, find the highest evaluating word that can be formed from them
bestEvaluatedWord :: [Char] -> Maybe String
bestEvaluatedWord rack = maximumBy (comparing $ evaluate rack) <$> nonEmpty (possibleWords rack)

-- Given a set of letters, return a limited amount of possible words with high evaluations
candidateWords :: [Char] -> [String]
candidateWords rack =
  take candidateWordCount $ sortBy (comparing (Down . evaluate rack)) (possibleWords rack)

-- Represents the state of a game being simulated
data SimulationState = SimulationState
  { getBag :: ![Char],
    getRack :: ![Char],
    getScore :: !Int,
    getOver :: !Bool
  }

-- Simulate playing a word and fill rack in afterwards
simulatePlayWord :: SimulationState -> String -> SimulationState
simulatePlayWord state word =
  let bag = getBag state
      rack = getRack state
      score = getScore state

      rackLeave = rack \\ word
      -- (HLINT: Infinite) Length should evaluate properly as the function is ran over finite lists
      emptyRackSlots = rackSize - length rackLeave

      bagVowelCount = countVowels bag
      bagConsonantCount = countConsonants bag

      extraVowelCount =
        idealVowelCount - countVowels rackLeave -- Maintain the ideal ratio
          & min bagVowelCount -- Don't take more vowels than possible
          & min emptyRackSlots -- Don't overfill the rack
          & max 0 -- Avoid negative values
      extraConsonantCount = emptyRackSlots - extraVowelCount

      -- Random letters taken from the bag
      extraLetters =
        take extraVowelCount (filter (`elem` vowels) bag)
          ++ take extraConsonantCount (filter (`elem` consonants) bag)

      newRack = rackLeave ++ extraLetters
      newBag = bag \\ extraLetters
   in if bagVowelCount + bagConsonantCount < emptyRackSlots
        then
          -- End the game if bag becomes too small
          state
            { getScore = score + scoreWord word,
              getOver = True
            }
        else
          -- Otherwise update the state
          state
            { getBag = newBag,
              getRack = newRack,
              getScore = score + scoreWord word
            }

-- Simulate a round where the best evaluated word is chosen
simulateRound :: SimulationState -> SimulationState
simulateRound state
  | getOver state = state -- Do nothing if game is already over
  | isNothing word = state {getOver = True} -- End game if no word can be formed
  -- (HLINT: Partial) fromJust evaluates properly since we check for Nothing beforehand
  | otherwise = simulatePlayWord state (fromJust word) -- Otherwise play word
  where
    word = bestEvaluatedWord (getRack state)

-- Simulate a game by running multiple rounds and return the total score
simulateGame :: SimulationState -> Int
simulateGame state =
  -- (HLINT: Partial) Indexing never fails since the list is infinite
  getScore (iterate simulateRound state !! simulationDepth)

-- Simulate multiple games by for a given word and return their average
simulateMultipleGames :: SimulationState -> String -> Int
simulateMultipleGames state word =
  let -- "Hash" the word to get the base seed for randomness
      -- This provides better results for words that have similar scores
      -- Confirmed to have no collisions within the given dictionary
      baseSeed = 1000 * foldr (\char seed -> ord char + 26 * seed) 0 word

      -- Given a state and a seed, shuffle its bag
      -- This prevents all the simulations from returning the same result
      randomGame game seed =
        let bag = getBag game
            gen = mkStdGen (baseSeed + seed)

            -- Note that we have to do a null since shuffle' is partial
            -- (HLINT: Infinite) Length should evaluate properly as the function is ran over
            shuffledBag = if null bag then bag else shuffle' bag (length bag) gen
         in game {getBag = shuffledBag}

      -- Create multiple copies of the original game state, with different bags
      games = parMap rseq (randomGame state) [1 .. simulationCount]

      -- Play the given word for all the games
      gamesAfterWord = parMap rseq (`simulatePlayWord` word) games

      -- Scores of the games after playing a few moves
      gameScores = parMap rseq simulateGame gamesAfterWord
   in -- Return the average score
      -- (HLINT: Infinite) Sum should evaluate properly as the function is ran over finite lists
      -- Note that division doesn't fail since simulationCount > 0
      sum gameScores `div` simulationCount

-- Find the optimal word to play through static evaluation and simulations
bestWordSmart :: [Char] -> [Char] -> String
bestWordSmart bag rack =
  let state =
        SimulationState
          { getBag = bag,
            getRack = rack,
            getScore = 0,
            getOver = False
          }

      words = candidateWords (getRack state)

      simulationResults = parMap rseq (\word -> (word, simulateMultipleGames state word)) words
   in -- (HLINT: Partial) When the AI plays there is guarenteed to be at least 1 valid word, so
      -- maximumBy always receives a non empty list
      fst $ maximumBy (comparing snd) simulationResults

-- Ex. 10:
-- Write an AI which plays the Backwords game as well as possible.
-- Note that it is guarenteed that there is always a valid word to play or a letter to take
aiMove :: [Char] -> [Char] -> Move
aiMove bag rack =
  -- (HLINT: Infinite) Length should evaluate properly as the function is ran over finite lists
  if length rack < rackSize
    then
      takeLetter bag rack
    else
      PlayWord $ bestWordSmart bag rack

{-
Trie Benchmark
--------------

Naive implementations of `isValidWord` and `possibleWords` could look like this:

```
isValidWord word = map Data.Char.toLower word `elem` allWords

possibleWords letters = filter (canFormWord letters) allWords
  where
  countCharacters letters = Map.fromListWith (+) $ map (,1) letters
  canFormWord letters word = Map.isSubmapOfBy (<=) (countCharacters word) (countCharacters letters)
```

Benchmarking this using `bestWordSmart` in a REPL:

```
$ stack repl
ghci> :set +s
ghci> bestWordSmart (concatMap (replicate 9) ['a'..'z']) "iowersiua"
"resow"
(1021.94 secs, 2,390,182,619,960 bytes)
```

Benchmarking an implementation using Tries instead:

```
$ stack repl
ghci> :set +s
ghci> bestWordSmart (concatMap (replicate 9) ['a'..'z']) "iowersiua"
"resow"
(42.48 secs, 51,382,998,568 bytes)
```

Note that this has been tested using 150 simulation rounds.
Also note that the parallelisation is disabled while in a REPL.

Parallelisation Benchmark
-------------------------

Testing parallelisation is a bit trickier since REPLs can't evaluate in parallel. Instead, this can
be roughly benchmarked by timing how long the AI thinks for after running `stack run ai`. The
difference should be quite noticeable. Alternatively, a program like Threadscope can be used to
visually see the how the compute load is being distributed.

To disable parallelisation, replace all instances of `parMap rseq` with `map`.

The unparallelised implementation takes 12.65 secs.
The parallelised implementation takes 2.01 secs.

Note that this has been tested using 150 simulation rounds and with the rack "iowersiua".

-----

Benchmarks done on DCS systems.
-}
