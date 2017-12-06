module Main where

import Data.Map.Strict as M
import Data.Array as A

-- This was too slow for Part 2
tapeNav' (ls, r:rs) =
  let
    l =
      length ls - (abs r)
    n =
      if r > 2 then r - 1
      else r + 1
  in
    if r < 0
    then
      (take l $ ls ++ (n : rs), drop l $ ls ++ (n : rs))
    else
      (ls ++ take r (n : rs), drop r (n : rs))

data Tape
  = Tape Int (Map Int Int) Int
  | Done Int
  deriving Show

tape :: Tape -> [Int]
tape (Done _) = []
tape (Tape _ m _) = M.elems m

tapeNav :: Tape -> Tape
tapeNav (Done x) = Done x
tapeNav (Tape k m c) =
  case M.lookup k m of
    Just x ->
      let
        o = if x > 2 then x - 1 else x + 1
      in
        tapeNav $ Tape (k + x) (M.insert k o m) (c + 1)
    Nothing ->
      Done c


mapper :: [a] -> Map Int a
mapper xs =
  fromList . A.assocs $ A.listArray (1, length xs) xs

main :: IO ()
main = do
  file <- readFile "/home/abinr/workspace/advent/2017/5/tape/src/tape.txt"
  let ls = lines file
  let tape = fmap (\x -> read x :: Int) ls
  print $ tapeNav (Tape 1 (mapper tape) 0)
--  print . length . takeWhile (not . null . snd) $ iterate tapeNav ([], tape)

test :: [Int]
test = [0, 3, 0, 1, -3]
