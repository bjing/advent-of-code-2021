{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Day2.Day2.Ex1 where

type HorizontalPosition = Int
type Depth = Int

data Move =
    Forward Int
  | Up Int
  | Down Int

main :: IO ()
main = do
  moves <- loadMoves
  let result = foldl doMove (0, 0) moves
  print $ uncurry (*) result

doMove :: (HorizontalPosition, Depth) -> Move -> (HorizontalPosition, Depth)
doMove (hor, ver) = \case
  Forward steps -> (hor + steps, ver)
  Up steps -> (hor, ver - steps)
  Down steps -> (hor, ver + steps)

loadMoves :: IO [Move]
loadMoves = do
  cmdTexts <- lines <$> readFile "src/Day2/input-ex1.txt"
  return $ genMove <$> cmdTexts

genMove :: String -> Move
genMove txt =
  let
    action:stepStr:[] = words txt
    steps = read stepStr :: Int
  in
    case action of
      "forward" -> Forward steps
      "up" -> Up steps
      "down" -> Down steps
      _ -> error "Initiating self-destruct sequence"
