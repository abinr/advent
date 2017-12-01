import Data.List
import Data.Char

digitMatch :: Int -> [Int] -> Int
digitMatch steps list =
    sum
    $ map fst
    $ filter (uncurry (==))
    $ zip list (drop steps list ++ take steps list)

main :: IO ()
main = do
  file <- readFile "DigitMatch.txt"
  let list = fmap digitToInt $ head $ lines file
  -- let steps = 1
  let steps = length list `div` 2
  print $ digitMatch steps list

test :: Bool
test =
  all id
  [ (3 == digitMatch 1 [1,1,2,2])
  , (4 == digitMatch 1 [1, 1, 1, 1])
  , (0 == digitMatch 1 [1, 2, 3, 4])
  , (9 == digitMatch 1 [9, 1, 2, 1, 2, 1, 2, 9])
  ]

test' :: Bool
test' =
  all id
  [ (6 == digitMatch 2 [1, 2, 1, 2])
  , (0 == digitMatch 2 [1, 2, 2, 1])
  , (4 == digitMatch 3 [1, 2, 3, 4, 2, 5])
  , (12 == digitMatch 3 [1, 2, 3, 1, 2, 3])
  , (4 == digitMatch 4 [1, 2, 1, 3, 1, 4, 1, 5])
  ]

