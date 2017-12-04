import Data.List

noDups :: String -> Bool
noDups =
  noDups' . words

noDups' :: [String] -> Bool
noDups' =
  all ((== 1) . length) . group . sort

noAnams =
  noDups' . map sort . words

main :: IO ()
main = do
  file <- readFile "pass.txt"
  let ls = lines file
  -- print . length $ filter noDups ls
  print . length $ filter noAnams ls

test =
  all id
  [ True == noDups "aa bb cc dd ee"
  , False == noDups "aa bb cc dd aa"
  , True == noDups "aa bb cc dd aaa"
  ]

test' =
  all id
  [ True == noAnams "abcde fghij"
  , False == noAnams "abcde xyz ecdab"
  , True == noAnams "a ab abc abd abf abj"
  , True == noAnams "iiii oiii ooii oooi oooo"
  , False == noAnams "oiii ioii iioi iiio"
  ]
