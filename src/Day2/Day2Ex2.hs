{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Day2.Day2Ex2 where

import Day2.Day2Ex1 (Move(..), loadMoves)

data Position = Position
  { horizontal :: Int
  , depth      :: Int
  , aim        :: Int
  }

initPosition :: Position
initPosition = Position
  { horizontal = 0
  , depth = 0
  , aim = 0
  }

main :: IO ()
main = do
  moves <- loadMoves "src/Day2/input-ex2.txt"
  let result = foldl doMove initPosition moves
  print $ (horizontal result) * (depth result)

doMove :: Position -> Move -> Position
doMove position@Position{..} = \case
  Forward steps -> position {horizontal = horizontal + steps, depth = depth + aim * steps}
  Up steps -> position {aim = aim - steps}
  Down steps -> position { aim = aim + steps}
