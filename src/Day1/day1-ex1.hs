module Day1Ex1 where

main :: IO ()
main = do
  pairs <- loadAdjacentPairs
  print $ length $ filter (\(x, y) -> x < y) pairs

loadAdjacentPairs :: IO [(Int, Int)]
loadAdjacentPairs = do
  content <- readFile "src/Day1/input"
  let readings = (read <$> lines content) :: [Int]
  return $ zip (init readings) (tail readings)
