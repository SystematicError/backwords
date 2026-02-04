{-# LANGUAGE OverloadedStrings #-}

-- |
--  This module contains the code for the terminal user interface (TUI) for
--  Backwords. You do NOT need to worry about the contents of this file, but it
--  might be of interest to those of you looking to go deeper into advanced
--  Haskell programming.
--
--  Your code should go in src/CourseworkOne.hs.
module Backwords.Frontend.Widgets where

import Backwords.BasicGame
import Backwords.Frontend.Types
import Backwords.Frontend.Utils
import Backwords.Types hiding (PlayWord)
import Brick hiding (Down)
import Brick.Widgets.Border
import Brick.Widgets.Center
import CourseworkOne qualified as CW
import Data.Char (toLower, toUpper)
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text qualified as Text
import System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------
-- WIDGETS

-- |
--  This is the function that takes the current game state and gives back a
--  tree of widgets that should be rendered to the screen.
--
--  It works a bit like Hatch (a function from current frame number to an Image),
--  but with a much larger family of primitives and operations.
drawScreen :: TUIState -> [Widget Name]
drawScreen TUIState {..} =
  [ renderBag | showingBag ] ++ [ joinBorders $
      vBox
        [ -- The title at the top
          hCenter renderTitle,
          hBorder,
          hCenter $
            styleScoreAnim $
              padded $ "Total score: " ++ show (totalScore gameState),
          txt " ",
          case currentMode of
            ChooseTakeTile -> renderTileSelection
            GameOver _ -> renderGameOver
            _ -> renderPlayArea,
          txt " ",
          renderRack,
          hBorder,
          vLimit 2 $ hBox
            [ hCenter $ txt " Space: Show/hide Bag ",
              vLimit 1 vBorder,
              hCenter $ txt " Escape: Quit ",
              vLimit 1 vBorder,
              hCenter $ txt " Click or type: select/deselect ",
              vLimit 1 vBorder,
              hCenter $ txt " Enter: Play word "
            ]
        ]
  ]
  where
    rackTiles :: [(Char, Bool)]
    rackTiles =
      zipWith
        (\ix c -> (c, Set.member ix usedTiles))
        [0 ..]
        (rack gameState)

    renderRack :: Widget Name
    renderRack =
      vBox
        [ hCenter $ txt "Your rack:",
          vLimit 4 $
            hCenter $
              hBox $
                zipWith renderRackTile [0 ..] rackTiles
        ]

    styleScoreAnim :: Widget Name -> Widget Name
    styleScoreAnim w
      | currentMode == WaitScore && even animPos = withAttr "greenBg" w
      | otherwise = w

    isRaised ix =
      let d = animPos - ix
       in d <= 1 && d >= 0 && currentMode == WaitCorrect

    renderPlayArea :: Widget Name
    renderPlayArea = clickable PlayArea $
      center $
        vBox
          [ hCenter $
              (if currentMode == WaitWrong && even animPos
                then withStyledBorder "redOnRed" . const
                else id
              )
              $ hBox $
                if null currentWord then [vLimit 3 $ fill ' '] else
                zipWith
                  (\ix c -> renderTile (isRaised ix) c False)
                  [0 ..]
                  currentWord,
            wordScoreLine
          ]

    wordScoreLine
      | currentMode `elem` [WaitScore, WaitCorrect, PlayWord] =
          hCenter $
            styleScoreAnim $
              padded $ "Word score: " ++ scoreText
      | currentMode == WaitWrong =
          hCenter $
            withAttr "redBg" $ padded "Invalid word!"
      | otherwise = emptyWidget

    renderTileSelection :: Widget Name
    renderTileSelection =
      center $
        border $
          padSides 1 $
            hLimit 64 $
              vBox
                [ hCenter $ str $ tilesRemainingLabel ++ " tiles left to choose",
                  hCenter $ hBox
                    [ selectionButton TakeConsonantBtn "Take consonant" consonantCount (txt " [c / Left]")
                    , selectionButton TakeVowelBtn "Take vowel" vowelCount (padLeft Max $ txt " [v / Right]")
                    ]
                ]

    renderGameOver :: Widget Name
    renderGameOver = center
      $ border
      $ hLimit 60
      $ vBox
          [ hCenter $ txt "Game Over!",
            hBorder,
            hCenter $ case currentMode of
              GameOver NoWords -> txt "No valid words can be formed from your rack."
              GameOver BagEmpty -> txt "The bag is empty."
              _ -> emptyWidget,
            hCenter $ str $ "Final score: " ++ show (totalScore gameState),
            hCenter $ txt "Press 'Esc' to quit or 'R' to restart."
          ]

    renderBag :: Widget Name
    renderBag =
      center $
        border $
          hLimit 80 $
            vBox
              [ hCenter $ txt "Letter Bag"
              , hBorder
              , hCenter $
                  paddedWidget
                    (hBox $ List.intersperse (txt " ") $ map renderBagColumn ["abcdef", "ghijkl", "mnopqr", "stuvwx", "yz"])
              , hCenter $ str $ show (length (bag gameState)) ++ " letters remaining in bag."
              ]

    dist = tryGetUnsafe Nothing (Just $ CW.bagDistribution (bag gameState))

    tilesRemainingLabel = show (numRack - length (rack gameState))
    consonantCount = length $ filter (`elem` consonants) (bag gameState)
    vowelCount = length $ filter (`elem` vowels) (bag gameState)
    scoreText = maybe "???" show (unsafePerformIO $ tryGetIO (CW.scoreWord currentWord))

    padded :: String -> Widget Name
    padded = padLeft (Pad 1) . padRight (Pad 1) . str

    paddedWidget :: Widget Name -> Widget Name
    paddedWidget = padLeft (Pad 1) . padRight (Pad 1)

    selectionButton :: Name -> String -> Int -> Widget Name -> Widget Name
    selectionButton btn label remaining hint =
      (if remaining > 0 then clickable btn else id) $
        border $
          hLimit 30 $
            vLimit 10 $
              vBox
                [ center $ txt (Text.pack label)
                , center $ str $ show remaining ++ " left"
                , padTop Max hint
                ]

    renderBagColumn :: String -> Widget Name
    renderBagColumn letters =
      vBox $ map renderEntry letters
      where
        renderEntry letter =
          let count = fromMaybe 0 . lookup letter <$> dist
          in renderLetter letter <+> padLeft (Pad 1) (padTop (Pad 1) $ str $ "x" ++ maybe "???" show count)

    renderRackTile :: Int -> (Char, Bool) -> Widget Name
    renderRackTile ix (c, isUsed)
      | ix `Set.member` discardedTiles = emptyWidget
      | otherwise = clickable (RackTile ix) $ renderTile False c isUsed

-- The Bool indicates whether the letter is used
renderTile :: Bool -> Char -> Bool -> Widget Name
renderTile isRaised c isUsed =
  (if isUsed then withAttr "usedTile" else id) $
    (if isRaised then padBottom (Pad 1) else padTop (Pad 1)) $ vBox [
      hBox [borderTL, hLimit 3 hBorder, borderTR],
      hBox [ 
        vLimit 1 vBorder,
        hLimit 3 $ hCenter $ txt $ Text.pack [toUpper c],
        vLimit 1 vBorder
        ],
      hBox [borderBL, hLimit 4 $ hBorder <+> str tileScore]
    ]
  where 
    tileScore = tryGetUnsafe "?" (show $ CW.letterValue c)
    borderTL = joinableBorder $ Edges False True False True
    borderTR = joinableBorder $ Edges False True True False
    borderBL = joinableBorder $ Edges True False False True

renderLetter :: Char -> Widget Name
-- renderLetter c = border $ hLimit 3 $ hCenter $ txt $ Text.pack [toUpper c]
renderLetter c =
  vBox
    [ hBox [borderTL, hLimit 3 hBorder, borderTR],
      hBox
        [ vLimit 1 vBorder,
          hLimit 3 $ hCenter $ txt $ Text.pack [toUpper c],
          vLimit 1 vBorder
        ],
      hBox [borderBL, hLimit 4 $ hBorder <+> str tileScore]
    ]
  where
    tileScore = tryGetUnsafe "?" (show $ CW.letterValue c)
    borderTL = joinableBorder $ Edges False True False True
    borderTR = joinableBorder $ Edges False True True False
    borderBL = joinableBorder $ Edges True False False True

renderTitle :: Widget Name
renderTitle = hBox $ map renderChar "BACKWORDS"
  where
    renderChar :: Char -> Widget Name
    renderChar c = let
      an = attrName ("letter_" <> [toLower c])
      in withAttr an $ withStyledBorder an $ const $ renderLetter c

styledBorder :: AttrName -> Widget n -> Widget n
styledBorder attr w = withStyledBorder attr $ \revert -> border $ revert w

withStyledBorder :: AttrName -> ((Widget n -> Widget n) -> Widget n) -> Widget n
withStyledBorder attr withRevert = do
  overrideAttr "__border__" "border"
    . overrideAttr "border" attr
    $ withRevert (overrideAttr "border" "__border__")


padSides :: Int -> Widget n -> Widget n
padSides n = padLeft (Pad n) . padRight (Pad n)
