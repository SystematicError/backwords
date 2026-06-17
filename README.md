# What Is Backwords?

Backwords is a Scrabble-like game where a player play words from your rack until no further words can be formed or until the bag runs out. Words are scored similar to Scrabble with one notable exception: letters towards the end of the word are weighted higher using powers of two (hence the name). The goal is to maximise your score. I have designed an AI that attempts to play Backwords as efficiently as possible.

This project was submitted as part of the coursework required by my university's [CS141](https://warwick.ac.uk/fac/sci/dcs/teaching/modules/cs141/) module, and was awarded a grade of 100% (High First Class).

![Demo image](https://github.com/user-attachments/assets/2f4b357f-13e5-4183-9b80-99fcdfd2d40a)

# How the AI Works

When drawing letters from the bag, the AI tries to maintain an ideal vowel to consonant ratio. For the given dictionary, this ratio is around 4:5.

The word playing strategy is more elaborate, it performs static evaluation and then runs [Monte Carlo simulations](https://en.wikipedia.org/wiki/Monte_Carlo_method) to find the optimal word to play. This is inspired by Scrabble engines like [Quackle](https://people.csail.mit.edu/jasonkb/quackle/).

1. For a given rack, the AI calculates an evaluation for all possible words it can play. The evaluation function is calculated using two other scoring functions:
   - `scoreWord`, which scores the word being played
   - `scoreRackLeave`, which scores the rack leave (the rack letters left after playing a word)
2. From the set of all possible words, the AI selects a few candidates with high evaluations.
3. For each candidate word, the AI simulates playing the next few words.
4. The AI does numerous random simulations for every word, and plays the word with the highest average simulation score.

There are a few parameters that can be adjusted to modify how the AI plays, the default values have been tested and tuned according to the given dictionary to maximise the score whilst remaining decently performant. These parameters are:

| Parameter               | Default Value | Description                                                                 |
| ----------------------- | ------------- | --------------------------------------------------------------------------- |
| `idealVowelCount`       | 4             | How many vowels the AI tries to ensure is in the rack                       |
| `candidateWordCount`    | 20            | The number of words possible words which the AI picks to run simulations on |
| `wordScoreCoefficient`  | 17            | The coefficient applied to word score in the evaluation function            |
| `leaveScoreCoefficient` | 4             | The coefficient applied to rack leave score in the evaluation function      |
| `simulationCount`       | 100           | The number of simulations run for each candidate word                       |
| `simulationDepth`       | 2             | How many moves ahead the AI simulates                                       |

The `scoreWord` evaluation used is based off of the game's default scoring function. On the otherhand, `scoreRackLeave` is a hand crafted scoring function which I computed through analysis of the dictionary and testing. Generally, for a given letter in the rack leave, score priorities the following:
 - **Frequency:** How often do we use this letter?
 - **Versatility:** Does this letter occur in various positions across different words?
 - **Positionality:** How far to the right of the word does this letter occur in?

Additionally, the program leverages parallelism and data structures like [Tries](https://en.wikipedia.org/wiki/Trie) to ensure that search finishes in a reasonable amount of time. Benchmarks for these have been given at the bottom of the `CourseworkOne.hs` file.

The AI achieves an average score of **2843**.

# Usage

| Command               | Description                               |
| --------------------- | ----------------------------------------- |
| `stack run`           | Run just the game, to try it out yourself |
| `stack run ai`        | Run the AI alongside the game GUI         |
| `stack run total [n]` | Run $n$ AI games in headless mode         |
| `stack test`          | Test suite                                |
