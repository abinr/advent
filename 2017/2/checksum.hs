import Control.Applicative

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

diffMinMax :: [Integer] -> Integer
diffMinMax list =
  maximum list - minimum list

sumNoRem :: [Integer] -> Integer
sumNoRem ints =
  liftA2 (,) ints ints
  |> filter (\(a, b) -> a `rem` b == 0)
  |> fmap (uncurry div)
  |> filter (/= 1)
  |> sum

rows :: [[Integer]]
rows =
  [ [5, 1, 9, 5]
  , [7, 5, 3]
  , [2, 4, 6, 8]
  ]

rows' :: [[Integer]]
rows' =
  [ [5, 9, 2, 8]
  , [9, 4, 7, 3]
  , [3, 8, 6, 5]
  ]

main :: IO ()
main = do
  file <- readFile "checksum.txt"
  let rowsOfWords = fmap words $ lines file
  let rowsOfInts = fmap (fmap (\x -> read x :: Integer)) rowsOfWords
  -- print $ sum $ fmap diffMinMax rowsOfInts
  print $ sum $ fmap sumNoRem rowsOfInts
