import Data.List
import Data.Char
import qualified Data.Map as Map

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

wordNums :: String -> [(String, Int)]
wordNums = map (\xs -> (head xs, length xs)) . group . sort . words

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)

encode :: Int -> String -> String
encode offset msg = map (\c -> chr $ ord c + offset) msg

decode :: Int -> String -> String
decode shift msg  = encode (negate  shift) msg

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstTo40 :: Maybe Int
firstTo40 = find (\x -> digitSum x == 40) [1..]

firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1..]

phoneBook = 
    [("betty", "555-2938")
    , ("betty", "342-2492")
    , ("bonnie", "452-2928")
    , ("patsy", "493-2928")
    , ("patsy", "943-2929")
    , ("patsy", "827-9162")
    , ("lucille", "205-2928")
    , ("wendy", "939-8282")
    , ("penny", "853-2492")
    , ("penny", "555-2111")]

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key xs = foldr
                    (\(k, v) acc -> if key == k then Just v else acc)
                    Nothing xs

string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith add xs
    where add number1 number2 = number1 ++ ", " ++ number2