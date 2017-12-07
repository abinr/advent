import qualified Data.Map.Strict as M
import Data.List

main :: IO ()
main = do
  file <- readFile "banks.txt"
  let ws = fmap (\x -> read x :: Int) $ words file
  let f@(Tape m _ _) = step $ Tape (toMem ws) [] 0
  print f
  let second = step $ Tape m [] 0
  print second

type Mem = M.Map Int Int

data Tape =
  Tape Mem [[Int]] Int

instance Show Tape where
  show (Tape m _ c) =
    "Banks: " ++ (show m) ++ ", Cycles: " ++ (show c)

step :: Tape -> Tape
step (Tape m ls c) =
  let
    (v, k) = maxValue m -- find leftmost largest value
    m' = M.insert k (0 :: Int) m -- set its bank to zero
    ks =
      take v -- take the banks for redistribution
      . concat -- flatten
      . replicate 2 -- clone the keys
      $ drop (k + 1) (M.keys m) ++ take (k + 1) (M.keys m) -- put indexes in order
    mem = foldr addOneFor m' ks -- add one to each
  in
    if M.elems mem `elem` ls
    then Tape mem (M.elems mem : ls) (c + 1)
    else step $ Tape mem (M.elems mem : ls) (c + 1)


addOneFor :: Int -> Mem -> Mem
addOneFor k m =
  M.insertWith (+) k 1 m

maxValue :: Mem -> (Int, Int) -- (v, k)
maxValue =
  M.findMax . valueToKey

valueToKey :: Mem -> Mem
valueToKey =
  M.foldrWithKey (flip M.insert) M.empty

toMem :: [Int] -> Mem
toMem =
  M.fromList . zip [0..]


test = [0, 2, 7, 0] :: [Int]
