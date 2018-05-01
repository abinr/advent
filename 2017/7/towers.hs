import Data.List
import Data.Char
import Data.Tree

type Line = String
type Id = String

branch :: [Line] -> [Id] -> [Int]
branch s [] = []
branch s (k:ks) =
  let
    l = findLine s k
    rs = fmap (findLine s) $ getRefs l
  in
    if areLeaves rs
    then getWeight l : fmap getWeight rs
    else getWeight l : branch s ks

b :: [Line] -> [Id] -> [[Int]]
b s [] = []
b s (r:rs) =
  branch s [r] : b s rs

areLeaves :: [Line] -> Bool
areLeaves  =
  all null . fmap getRefs

findLine :: [Line] -> Id -> Line
findLine ls id =
  head $ filter ((== id) . takeWhile (/=' ')) ls

getRefs :: Line -> [Id]
getRefs l =
   words . filter (/= ',') $ filter (/= '>') $ dropWhile (/= '>') l

getWeight :: Line -> Int
getWeight =
  read . takeWhile isDigit . dropWhile (not . isDigit)

root :: [Line] -> Id
root s =
  head $ parents s \\ children s

children :: [Line] -> [Id]
children =
  concat . fmap getRefs . filter (elem '-')

parents :: [Line] -> [Id]
parents =
  fmap (takeWhile isLetter) . filter (elem '-')

main :: IO ()
main = do
  file <- readFile "towers.txt"
  let ls = lines file
  -- print $ root ls
  print . b test $ getRefs $ findLine test $ root test

test =
  [ "pbga (66)"
  , "xhth (57)"
  , "ebii (61)"
  , "havc (66)"
  , "ktlj (57)"
  , "fwft (72) -> ktlj, cntj, xhth"
  , "qoyq (66)"
  , "padx (45) -> pbga, havc, qoyq"
  , "tknk (41) -> ugml, padx, fwft"
  , "jptl (61)"
  , "ugml (68) -> gyxo, ebii, jptl"
  , "gyxo (61)"
  , "cntj (57)"
  ]
