module Day1Ex2 where

main :: IO ()
main = do
  sums <- loadSlidingWindowSum
  let pairs = zip (init sums) (tail sums)
  print $ length $ filter (\(x, y) -> x < y) pairs

loadSlidingWindowSum:: IO [Int]
loadSlidingWindowSum = do
  content <- readFile "src/Day1/input-ex2.txt"
  let readings = (read <$> lines content) :: [Int]
  let sumedUpWindow = (\(x,y,z) -> x + y + z) <$> zip3 (init readings) (tail readings) (tail $ tail readings)
  return sumedUpWindow

