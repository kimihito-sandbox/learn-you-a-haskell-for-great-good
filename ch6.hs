import Data.List

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

wordNums :: String -> [(String, Int)]
wordNums = map (\xs -> (head xs, length xs)) . group . sort . words