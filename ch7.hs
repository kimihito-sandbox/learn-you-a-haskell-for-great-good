data Person = Person { firstName :: String
                       , lastName :: String 
                       , age :: Int
                       , height :: Float
                       , phoneNumber :: String
                       , flavor :: String } deriving(Show)

data Car  = Car { company :: String
                 , model :: String
                 , year ::  Int               
                 } deriving (Show)
tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) = 
    "This " ++ c ++ " " ++ m ++ " was made in " ++ show y