module CourseworkOne where

import Backwords.Types
import Backwords.WordList

import Data.List
import Data.Char
import Data.Ratio

--------------------------------------------------------------------------------
-- This file is your complete submission for the first coursework of CS141.
-- 
-- USER ID: 
--
-- Before starting on this file, ensure that you have read the specification IN
-- ITS ENTIRETY and that you understand everything that is required from a good
-- solution.
--------------------------------------------------------------------------------

-- Ex. 1: 
-- Read the spec to find out what goes here.

-- Ex. 2:
-- Read the spec to find out what goes here.

-- Ex. 3:
-- Determine if a word is valid.
isValidWord :: String -> Bool
isValidWord = error "Not implemented"

-- Ex. 4:
-- Determine the points value of a letter.
letterValue :: Char -> Int
letterValue = error "Not implemented"

-- Ex. 5:
-- Score a word according to the Backwords scoring system.
scoreWord :: String -> Int
scoreWord = error "Not implemented"

-- Ex. 6:
-- Get all words that can be formed from the given letters.
possibleWords :: [Char] -> [String]
possibleWords = error "Not implemented"

-- Ex. 7:
-- Given a set of letters, find the highest scoring word that can be formed from them.
bestWord :: [Char] -> Maybe String
bestWord = error "Not implemented"

-- Ex. 8:
-- Given a list of letters, and a word, mark as used all letters in the list that appear in the word.
useTiles :: [Char] -> String -> [Tile]
useTiles = error "Not implemented"

-- Ex. 9:
-- Given a nonempty bag of possible letters as a list, return the chance of drawing 
-- each letter.
bagDistribution :: [Char] -> [(Char, Rational)]
bagDistribution = error "Not implemented"


-- Ex. 9:
-- Write an AI which plays the Backwords game as well as possible.
aiMove :: [Char] -> [Char] -> Move
aiMove bag rack = error "Not implemented"
