{-# LANGUAGE LambdaCase #-}

module Day3.Day3Ex1 where

import Data.List
import Data.Ord
import Data.Char

type BinarySeq = [Bool]

{-
  Basic idea is as follows:
  1. load binary sequences from file,
  2. zip up all the sequences loaded, e.g. [1,0,0] and [1,1,0] zipped up will become
     [[1,1], [0,1], [0,0]]. The final result will be a list of binary sequences,
     a sequence being a zipped up list of all the digits from a specific column.
  3. find the most common digit for each binary sequence from step 2,
     this will result in a binary sequence, i.e. gamma
  4. invert each digit of gamma to get epsilon
  5. multiply gamma and epsilon and profit!
-}

main :: IO ()
main = do
  binaries <- loadBinarySeqs
  let seqLength = length $ head binaries
  let accuInitValue = replicate seqLength []
  let combinedSeqs = foldl accumulate accuInitValue binaries
  let gamma = getMostCommonBit <$> combinedSeqs
  let epsilon = not <$> gamma
  print $ binaryToDecimal gamma * binaryToDecimal epsilon

accumulate :: [BinarySeq] -> BinarySeq -> [BinarySeq]
accumulate binSeq [] = binSeq
accumulate (hs:ts) (h:t) = (h:hs) : accumulate ts t

loadBinarySeqs :: IO [BinarySeq]
loadBinarySeqs = do
  numStrings <- lines <$> readFile "src/Day3/input-ex1.txt"
  return $ (charToBool <$>) <$> numStrings

charToBool :: Char -> Bool
charToBool = \case
  '0' -> False
  '1' -> True
  _ -> error "What is this? I can only recognise binary!"

getMostCommonBit :: BinarySeq -> Bool
getMostCommonBit = head . maximumBy (comparing length) . (group . sort)

binaryToDecimal :: [Bool] -> Int
binaryToDecimal = foldr (\x y -> fromEnum x + 2*y) 0 . reverse
