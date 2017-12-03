import Data.Bifunctor
import Data.List

data Heading
 = East
 | North
 | West
 | South
 deriving (Enum, Eq, Show)

type Grid = [((Int, Int), Int)]
type Layer = Int

data Square
  = Square Grid Heading Layer (Int, Int)
  deriving (Show)


step :: Square -> Square
step (Square g h l (x, y)) =
  case h of
    East ->
      let
        p = (x + 1, y)
        ng = nth g p
      in
        if (x, y) == (l, negate l)
        then Square ng North (l + 1) p
        else Square ng East l p
    North ->
      let
        p = (x, y + 1)
        ng = nth g p
      in
        if (y + 1) == l
        then Square ng West l p
        else Square ng North l p
    West ->
      let
        p = (x - 1, y)
        ng = nth g p
      in
        if (x - 1) == negate l
        then Square ng South l p
        else Square ng West l p
    South ->
      let
        p = (x, y - 1)
        ng = nth g p
      in
        if (y - 1) == negate l
        then Square ng East l p
        else Square ng South l p

nth :: Grid -> (Int, Int) -> Grid
nth grid point =
  let
    ns = filter (/= point) $ neighbors point
    r = sum . fmap snd $ filter (\x -> fst x `elem` ns) grid
  in
    (point, r) : grid

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = do
  dx <- [-1..1]
  dy <- [-1..1]
  return (x + dx, y + dy)

point :: Square -> (Int, Int)
point (Square _ _  _ p) = p

grid :: Square -> Grid
grid (Square g _ _ _) = g

f :: Int -> Square -> Bool
f n s =
  (snd . head $ grid s) <= n

ngrid n =
  grid . head . take n $ iterate step seed

solve n =
  uncurry (+)
  . bimap abs abs
  . point . snd . last
  $ zip [1..n] $ iterate step (Square [] East 0  (0,0))

--- OMG, this was tough. Brute forced it with this.
solve2 n =
  head
  . grid
  . last
  . take n
  $ iterate step (Square [((0,0), 1)] East 0 (0,0))

seed :: Square
seed =
  Square [((0,0), 1)] East 0 (0,0)

test =
  all id
  [ 0 == solve 1
  , 3 == solve 12
  , 2 == solve 23
  , 31 == solve 1024
  ]
