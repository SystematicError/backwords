# What Is Backwords?

Backwords is a Scrabble-like game where a player play words from your rack until no further words can be formed or until the bag runs out. Words are scored similar to scrabble with one notable exception: letters towards the end are weighted higher using powers of two (hence the name). The goal is to maximise your score.

This project was submitted as part of my university coursework. I have designed an AI that attempts to play as efficiently as possible.

![Demo image](https://github.com/user-attachments/assets/2f4b357f-13e5-4183-9b80-99fcdfd2d40a)

# How the AI Works

When drawing letters from the bag, the AI tries to maintain an ideal vowel to consonant ratio. For the given dictionary, this ratio is around 4:5.

The word playing strategy is more elaborate, it performs static evaluation and then [Monte Carlo simulations](https://en.wikipedia.org/wiki/Monte_Carlo_method) to find the optimal word to play. This is inspired by Scrabble engines like [Quackle](https://people.csail.mit.edu/jasonkb/quackle/).

1. For a given rack, the AI calculates an evaluation for all possible words it can play. The evaluation function is calculated using two other scoring functions:
   - `scoreWord`, which scores the word being played
   - `scoreRackLeave`, which scores the rack leave (the rack letters left after playing a word)
2. From the set of all possible words, the AI selects a few candidates with high evaluations.
3. For each candidate word, the AI simulates playing the next few words.
4. The AI does numerous random simulations for every word, and plays the word with the highest average simulation score.

The AI achieves an average score of **2843**.

# Usage

| Command               | Description                               |
| --------------------- | ----------------------------------------- |
| `stack run`           | Run just the game, to try it out yourself |
| `stack run ai`        | Run the AI alongside the game GUI         |
| `stack run total [n]` | Run $n$ AI games in headless mode         |
| `stack test`          | Test suite                                |
