import Data.List
import Data.Char

sumMatch :: [Int] -> Int
sumMatch [] = 0
sumMatch [_] = 0
sumMatch (x : y : ys) =
  if x == y
  then x + sumMatch (y:ys)
  else sumMatch (y:ys)

digitMatch :: [Int] -> Int
digitMatch [] = 0
digitMatch xs =
  if last xs == head xs
  then last xs + sumMatch xs
  else sumMatch xs

main :: IO ()
main = do
  file <- readFile "DigitMatch.txt"
  print $ digitMatch $ fmap digitToInt $ head $ lines file


test :: Bool
test =
  [3, 4, 0, 9] ==
  fmap (digitMatch . snd)
  [ (3, [1,1,2,2])
  , (4, [1, 1, 1, 1])
  , (0, [1, 2, 3, 4])
  , (9, [9, 1, 2, 1, 2, 1, 2, 9])
  ]
