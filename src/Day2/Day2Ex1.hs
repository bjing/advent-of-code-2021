{-# LANGUAGE LambdaCase #-}

module Day2.Day2Ex1 where

type HorizontalPosition = Int
type Depth = Int
type Position = (HorizontalPosition, Depth)

data Move =
    Forward Int
  | Up Int
  | Down Int

main :: IO ()
main = do
  moves <- loadMoves "src/Day2/input-ex1.txt"
  let result = foldl doMove (0, 0) moves
  print $ uncurry (*) result

doMove :: Position -> Move -> Position
doMove (hor, ver) = \case
  Forward steps -> (hor + steps, ver)
  Up steps -> (hor, ver - steps)
  Down steps -> (hor, ver + steps)

loadMoves :: FilePath -> IO [Move]
loadMoves path = do
  cmdTexts <- lines <$> readFile path
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
