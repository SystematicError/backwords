{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiWayIf #-}

-- | This module contains the code for the terminal user interface (TUI) for
--     Backwords. 
-- 
-- You do NOT need to worry about the contents of this file, but it
--     might be of interest to those of you looking to go deeper into advanced
--     Haskell programming.
--
--     Your code should go in src/CourseworkOne.hs.
module TUI where

import Brick hiding (Down)
import Brick.BChan qualified as Brick
import Brick.Widgets.Border
import Control.Applicative (asum)
import Control.Concurrent
import Control.Monad (forever, replicateM, void, when)
import Control.Monad.IO.Class (liftIO)
import CourseworkOne qualified as CW
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Graphics.Vty
import Graphics.Vty.CrossPlatform (mkVty)
import Backwords.BasicGame
import Backwords.Frontend.Types
import Backwords.Frontend.Utils
import Backwords.Frontend.Widgets
import Backwords.Types hiding (PlayWord)
import System.Environment
import System.Exit (exitSuccess)
import Text.Printf (printf)
import Text.Read (readMaybe)

import Data.Set qualified as Set
import Data.Set (Set)
import qualified Backwords.Types as Ty

--------------------------------------------------------------------------------
-- MAIN

-- | This is the main entrypoint to the program - everything gets set up and
--     run from here. Most of what's going on is dictated by the Brick library,
--     including the "App" data type which underpins most of the program.
main :: IO ()
main = do
  -- Get the command line arguments to the program
  args <- getArgs

  if "total" `elem` args
    then runTotal
    else runTUI ("ai" `elem` args)

--------------------------------------------------------------------------------
-- RUNNING THE AI DIRECTLY

getAverageDirect :: Int -> IO Rational
getAverageDirect n = do
  results <- replicateM n runAIDirectly
  pure $ fromIntegral (sum results) % fromIntegral n

-- Run the AI directly, without the TUI, and print the average score
runTotal :: IO ()
runTotal = do
  args <- getArgs
  putStrLn "Running the AI directly."
  -- find out how many to run by finding any number in the args
  let n = fromMaybe 1 $ asum (map readMaybe args)
  putStrLn $ "Running " <> show n <> " games, please wait..."

  avg <- getAverageDirect n
  let -- Print a ratio nicely as a multiple of 100
      avgStr :: String
      avgStr = printf "%.2f" (fromRational avg :: Double)

  putStrLn $ "Average score after " <> show n <> " games: " <> avgStr

--------------------------------------------------------------------------------
-- RUNNING THE TUI

-- |
--  This function is the main entrypoint for the TUI. It sets up the event
--  channel, the app, the initial state, and then runs the app.
runTUI :: Bool -> IO ()
runTUI ai = do
  eventChannel <- Brick.newBChan 10

  let app =
        Brick.App
          { appDraw = drawScreen,
            appChooseCursor = neverShowCursor,
            appHandleEvent = handleEvent,
            appStartEvent = do
              vty <- getVtyHandle
              let output = outputIface vty
              when (supportsMode output Mouse)
                $ liftIO $ setMode output Mouse True

              void $ liftIO $ forkIO $ forever $ do
                Brick.writeBChan eventChannel Tick
                threadDelay 100000
          , appAttrMap = const allAttributes
          }

  ts <- makeInitialTUIState ai

  let builder = mkVty defaultConfig
  vty <- builder

  _ <- customMainWithVty vty builder (Just eventChannel) app ts

  exitSuccess

allAttributes :: AttrMap
allAttributes =
  attrMap
    defAttr
    [ ("title", defAttr `withForeColor` magenta),
      ("blue", defAttr `withForeColor` blue `withStyle` bold),
      ("red", defAttr `withForeColor` red `withStyle` bold),
      ("greenBg", white `on` green),
      ("def", defAttr),
      ("bold", defAttr `withStyle` bold),
      ("redOnRed", defAttr `withForeColor` red `withBackColor` red),
      ("green", defAttr `withForeColor` green),
      ("blueBg", defAttr `withBackColor` blue),
      ("redBg", defAttr `withBackColor` red),
      ("gray", defAttr `withForeColor` brightBlack),
      ("grayBg", defAttr `withBackColor` brightBlack),
      ("gold", defAttr `withForeColor` yellow),
      ("usedTile", defAttr `withForeColor` red `withStyle` bold),
      -- Spelling out BACKWORDS with ROYGBI + baby blue, pink, white
      ("letter_b", defAttr `withBackColor` red),
      ("letter_a", defAttr `withBackColor` rgbColor 255 165 0),
      ("letter_c", defAttr `withBackColor` rgbColor 255 255 0),
      ("letter_k", defAttr `withBackColor` rgbColor 0 200 0),
      ("letter_w", defAttr `withBackColor` rgbColor 0 80 255),
      ("letter_o", defAttr `withBackColor` rgbColor 120 60 180),
      -- baby blue
      ("letter_r", defAttr `withBackColor` (rgbColor 135 206 250)),
      -- pink
      ("letter_d", defAttr `withBackColor` rgbColor 255 255 255),
      ("letter_s", defAttr `withBackColor` rgbColor 255 192 203)
    ]

--------------------------------------------------------------------------------
-- EVENTS

-- | Brick works by handling events, which are passed to the handleEvent
-- function. That includes mouse and keyboard events, but also the "tick" event
-- we use to animate the screen.
handleEvent :: BrickEvent Name Tick -> GameUpdate
-- Quit the game with the 'ESC' key
handleEvent (Brick.VtyEvent (EvKey KEsc [])) = liftIO exitSuccess
-- Quit the game with Ctrl+C
handleEvent (Brick.VtyEvent (EvKey (KChar 'c') [MCtrl])) = liftIO exitSuccess
-- Restart the game with 'Ctrl+R' key
handleEvent (Brick.VtyEvent (EvKey (KChar 'r') [MCtrl])) = do
  ai <- gets isAIGame
  liftIO (makeInitialTUIState ai) >>= put
handleEvent (Brick.VtyEvent (EvKey (KChar ' ') [])) = toggleBag
handleEvent (Brick.VtyEvent (EvKey (KChar c) [])) = do
  mode <- gets currentMode
  b <- gets (bag . gameState)
  case mode of
    PlayWord -> addLetter c
    ChooseTakeTile | c == 'v' && any (`elem` vowels) b -> chooseVowel
                   | c == 'c' && any (`elem` consonants) b -> chooseConsonant
                   | otherwise -> pure ()
    _ -> pure ()
handleEvent (Brick.VtyEvent (EvKey KBS [])) = backspace
handleEvent (Brick.VtyEvent (EvKey KEnter [])) = playWord
handleEvent (Brick.VtyEvent (EvKey KLeft [])) = chooseConsonant
handleEvent (Brick.VtyEvent (EvKey KRight [])) = chooseVowel
-- This runs every 1/10th of a second to update the screen
-- (e.g. so we can do animations)
handleEvent (Brick.AppEvent Tick) = tick
-- Click on a tile to put it into the play area
handleEvent (MouseDown (RackTile ix) BLeft _ _) = do
  selectTile ix >> click
handleEvent (MouseDown TakeVowelBtn BLeft _ _) = do
  chooseVowel >> click
handleEvent (MouseDown TakeConsonantBtn BLeft _ _) = do
  chooseConsonant >> click
handleEvent (MouseUp _ (Just BLeft) _) =
  modify $ \t -> t {clicked = False}

handleEvent _ = pure ()

ifCanModify :: GameUpdate -> GameUpdate
ifCanModify action = do
  TUIState {..} <- get
  let modifyAllowed =
        currentMode == PlayWord
          && waiting == 0
          && not isAIGame
          && not clicked
          && not showingBag
  when modifyAllowed action

selectTile :: Int -> GameUpdate
selectTile ix = ifCanModify $ do
  TUIState {..} <- get
  let l = rack gameState !! ix
  let used = Set.member ix usedTiles
  modify $ \t -> if used
    then t {
      currentWord = reverse $ List.delete l $ reverse currentWord
      , usedTiles = Set.delete ix usedTiles}
    else t {currentWord = currentWord ++ [l]
      , usedTiles = Set.insert ix usedTiles}

addLetter :: Char -> GameUpdate
addLetter = ifCanModify . appendLetter

appendLetter :: Char -> GameUpdate
appendLetter c = do
  TUIState {..} <- get
  let
    maskUsed ch ix = if Set.member ix usedTiles then '0' else ch
  let rackAuto = zipWith maskUsed (rack gameState) [0 ..]
      manualLetters = map (rack gameState !!) (Set.toList usedTiles)
      currentWordAuto = currentWord List.\\ manualLetters
  tiles <- tryGetOr [] (CW.useTiles rackAuto currentWordAuto)
  case List.elemIndex (Unused c) tiles of
    Nothing -> pure ()
    Just ix ->
      modify $ \t -> t
        { currentWord = currentWord ++ [c]
        , usedTiles = Set.insert ix usedTiles
        }

backspace :: GameUpdate
backspace = ifCanModify $ do
  TUIState {..} <- get
  case reverse currentWord of
    [] -> pure ()
    (w:ws) -> do
      let r = rack gameState
      -- Find the last occurrence of w in the rack that is used
      let revIx = filter (\i -> r !! i == w) (Set.toList usedTiles)
      case revIx of
        [] -> pure ()
        (rix:_) -> do
          modify $ \t -> t {
            currentWord = reverse ws,
            usedTiles = Set.delete rix usedTiles}

playWord :: GameUpdate
playWord = ifCanModify finalizeWord

finalizeWord :: GameUpdate
finalizeWord = do
  TUIState {..} <- get
  wordIsValid <- tryGetOr False (CW.isValidWord currentWord)
  modify $ \t -> t
    { currentMode = if wordIsValid then WaitCorrect else WaitWrong
    , animPos = if wordIsValid then -1 else 0
    }

ifCanTake :: GameUpdate -> GameUpdate
ifCanTake action = do
  TUIState {..} <- get
  let takeAllowed =
        currentMode == ChooseTakeTile
          && waiting == 0
          && not isAIGame
          && not clicked
          && not showingBag
  if
    | (null (bag gameState))
        -> modify $ \t -> t {currentMode = GameOver BagEmpty}
    | takeAllowed -> action
    | otherwise -> pure ()

chooseConsonant :: GameUpdate
chooseConsonant = chooseTile False

chooseVowel :: GameUpdate
chooseVowel = chooseTile True

toggleBag :: GameUpdate
toggleBag = do
  TUIState {..} <- get
  let canToggle =
        currentMode `elem` [PlayWord, ChooseTakeTile]
          && waiting == 0
          && not isAIGame
          && not clicked
  when canToggle $ modify $ \t -> t {showingBag = not showingBag}

chooseTile :: Bool -> GameUpdate
chooseTile = ifCanTake . applyChooseTile

applyChooseTile :: Bool -> GameUpdate
applyChooseTile isVowel = do
  TUIState {..} <- get
  let updatedGameState = if isVowel then takeVowel gameState else takeConsonant gameState
  modify $ \t -> t
    { currentMode = ChooseTakeTile
    , gameState = updatedGameState
    }
  checkMoveToPlay

checkMoveToPlay :: GameUpdate
checkMoveToPlay = do
  TUIState {..} <- get
  let gs = gameState
      fullRack = length (rack gs) >= numRack
  candidateWords <- tryGetOr [] (CW.possibleWords (rack gs))
  when fullRack $
    modify $ \t -> t {currentMode = if null candidateWords then GameOver NoWords else PlayWord}

-- --------------------------------------------------------------------------------
-- -- TICKS

-- -- The Tick function animates the game.
-- -- Each one of the options here will call one of the animation functions in the
-- -- next section.

tick :: GameUpdate
tick = gets currentMode >>= \case
  WaitCorrect -> animateCorrect
  WaitScore -> animateScore
  WaitDiscard -> animateDiscard
  WaitWrong -> animateWrong
  PlayWord -> do
    aig <- gets isAIGame
    when aig aiPlayWord
  ChooseTakeTile -> do
    aig <- gets isAIGame
    when aig aiChooseTile
  _ -> pure ()

animateScore :: GameUpdate
animateScore = do
  TUIState {animPos, currentWord} <- get
  sc <- tryGetOr 0 (CW.scoreWord currentWord)
  when (animPos == 2)
    $ modifyGameState $ \gs -> gs {
      totalScore = totalScore gs + sc
    }
  if animPos > 4 then
    modify $ \t -> t {currentMode = WaitDiscard, animPos = 0}
  else do
    modify $ \t -> t {animPos = animPos + 1}

modifyGameState :: (GameState -> GameState) -> GameUpdate
modifyGameState f = modify $ \t -> t {gameState = f (gameState t)}

animateCorrect :: GameUpdate
animateCorrect = do
  TUIState {gameState, currentWord, animPos} <- get
  if animPos > length currentWord + 1 then
    modify $ \t -> t {currentMode = WaitScore, animPos = 0}
  else do
    modify $ \t -> t {animPos = animPos + 1}

animateWrong :: GameUpdate
animateWrong = do
  TUIState {animPos} <- get
  if animPos > 4 then
    modify $ \t -> t {currentMode = PlayWord, animPos = 0}
  else do
    modify $ \t -> t {animPos = animPos + 1}

animateDiscard :: GameUpdate
animateDiscard = do
  TUIState {usedTiles, discardedTiles, currentWord, gameState} <- get
  let
    removeIndices :: Set Int -> [a] -> [a]
    removeIndices xs ys = [v | (i,v) <- zip [0..] ys, not (Set.member i xs)]

    done :: GameUpdate
    done =
          modify $ \t -> t {
            currentMode = ChooseTakeTile,
            animPos = 0,
            discardedTiles = Set.empty,
            usedTiles = Set.empty,
            gameState = gameState {
              rack = removeIndices usedTiles (rack gameState)
            }
          }
  -- mark as discarded the first tile in usedTiles that is not in discardedTiles
  case currentWord of
    [] -> done
    (w:ws) -> do
      let r = rack gameState
      let ixToDiscard = List.find (\ix -> r !! ix == w && not (Set.member ix discardedTiles)) (Set.toList usedTiles)
      case ixToDiscard of
        Nothing -> done
        Just ix -> modify $ \t -> t {
          discardedTiles = Set.insert ix discardedTiles,
          currentWord = ws
          }

aiPlayWord :: GameUpdate
aiPlayWord = do
  -- Generate a next move, if we don't have one
  aim <- gets aiMove
  case aim of
    Nothing -> do
      GameState {..} <- gets gameState
      tryGet (CW.aiMove bag rack) >>= \case
        Nothing -> do
          error "AI failed to play a word."
        Just (Ty.PlayWord w) -> do
          modify $ \ts -> ts {aiMove = Just w}
        Just _ -> do
          error "AI tried to take a letter when it should play a word."
    Just _ -> pure ()

  cw <- gets currentWord
  -- If the AI move exists, then we can execute it (one step at a time)
  gets aiMove >>= \case
    Nothing -> pure ()
    Just w -> case w List.\\ cw of
      [] -> do
        modify $ \ts -> ts {aiMove = Nothing}
        finalizeWord
      (c : _) -> do
        appendLetter c


aiChooseTile :: GameUpdate
aiChooseTile = do
  bag <- gets (bag . gameState)
  rack <- gets (rack . gameState)

  if null bag
    then do
      modify $ \t -> t { currentMode = GameOver BagEmpty }       
    else do
      tryGet (CW.aiMove bag rack) >>= \case
        Nothing -> do
          error "AI failed to choose a tile."
        Just move -> case move of
          Ty.TakeVowel -> applyChooseTile True
          Ty.TakeConsonant -> applyChooseTile False
          Ty.PlayWord _ -> do
            error "AI tried to play a word when it should take a letter."



click :: EventM n TUIState ()
click = modify $ \t -> t{clicked = True}
