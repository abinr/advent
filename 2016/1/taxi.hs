import Data.Char
import Data.Bifunctor

data Heading
 = North
 | East
 | South
 | West
 deriving Show

data Turn
  = L
  | R
  deriving Show

toCarta :: Heading -> Turn -> Int -> (Heading, (Int, Int))
toCarta North L n = (West, (negate n, 0))
toCarta East L n = (North, (0, n))
toCarta South L n = (East, (n, 0))
toCarta West L n = (South, (0, negate n))
toCarta North R n = (East, (n, 0))
toCarta East R n = (South, (0, negate n))
toCarta West R n = (North, (0, n))
toCarta South R n = (West, (negate n, 0))

toTurn :: String -> (Turn, Int)
toTurn ('L' : xs) = (L, read xs)
toTurn ('R' : xs) = (R, read xs)

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (x, y) (dx, dy) =
  (x + dx, y + dy)

f :: (Heading, (Int, Int)) -> String -> (Heading, (Int, Int))
f (h, (x, y)) n =
  fmap (add (x, y)) . uncurry (toCarta h) $ toTurn n

main :: IO ()
main = do
  m <- readFile "taxi.txt"
  let r = words $ filter (/= ',') m
  print . uncurry (+) . bimap abs abs . snd $ foldl f (North, (0,0)) r
