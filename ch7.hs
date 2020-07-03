import qualified Data.Map as Map

data Person = Person { firstName :: String
                       , lastName :: String 
                       , age :: Int
                       } deriving(Eq, Show, Read)
mikeD = Person { firstName = "Michael", lastName = "Diamond", age = 43 }
adRock = Person { firstName = "Adam", lastName = "Horovitz", age = 41 }
mca = Person { firstName = "Adam", lastName = "Yauch", age = 44}

data Car  = Car { company :: String
                 , model :: String
                 , year ::  Int               
                 } deriving (Show)
tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) = 
    "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

data Vector a = Vector a a a deriving (Show)
vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) =  Vector (i+l) (j+m) (k+n)

dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = i*l + j*m + k*n

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i*m) (j*m) (k*m)

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker " ++ show lockerNumber
                        ++ "doesn't exist!"
    Just (state, code) -> if state /= Taken
                            then Right code
                            else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"
lockers :: LockerMap
lockers = Map.fromList
    [(100, (Taken, "ZD39I"))
    ,(101, (Free, "JAH3I"))
    ,(103, (Free, "IQSA9"))
    ]

data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)