doubleMe x = x + x

doubleUs x y = doubleMe x  + doubleMe y

doubleSmallNumber x = if x > 100
                         then x
                         else x*2

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

conanO'Brien = "It's a-me, Conan O'Brien!"

removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

factorial :: Integer -> Integer
factorial n = product [1..n]