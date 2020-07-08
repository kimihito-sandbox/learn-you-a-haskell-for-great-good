respondPalindromes :: String -> String
respondPalindromes =
    unlines .
    map (\xs -> if isPal xs then "palidrome" else "not a palindrome") .
    lines

isPal :: String -> Bool
isPal xs = xs == reverse xs

main = interact respondPalindromes