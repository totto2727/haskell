import           Data.Char

-- powerOfTwo::Int->[Int ]
-- powerOfTwo n=do
--     value<-[1 .. n]
--     return $ 2^value

-- powerOfTwoAndThree::Int->[(Int,Int)]
-- powerOfTwoAndThree n=do
--     value<-[1 .. n]
--     return (2^value, 3^value)

powerOfTwoAndThree :: Int -> [(Int, Int)]
powerOfTwoAndThree n =
    [ (powersOfTwo, powersOfThree)
    | value <- [1 .. n]
    , let powersOfTwo   = 2 ^ value
    , let powersOfThree = 3 ^ value
    ]

-- allEvenOdds :: Int -> [(Int, Int)]
-- allEvenOdds n = do
--     odd  <- [1, 3 .. n]
--     even <- [2, 4 .. n]
--     return (odd, even)

allEvenOdds :: Int -> [(Int, Int)]
allEvenOdds n = [ (o, e) | o <- [1, 3 .. n], e <- [2, 4 .. n] ]

evensGuard :: Int -> [Int]
evensGuard n = [ v | v <- [1 .. n], even v ]

upperAndMr :: [String] -> [String]
upperAndMr list =
    [ r
    | v <- list
    , let u = (\(x : xs) -> toUpper x : xs) v
    , let r = mconcat ["Mr.", v]
    ]

monthInfo::[(Int,Int)]
monthInfo=[(1,31),(2,28),(3,31),(4,30),(5,31),(6,30),(7,31),(8,31),(9,30),(10,31),(11,30),(12,31)]

calender::[String]
calender=
    [ date
    | info<-monthInfo
    , day<-[ 1 .. snd info]
    , let date=mconcat [show $fst info,"/",show day]
    ]

calenderDo::[String]
calenderDo=do
    info<-monthInfo
    day<-[1 .. snd info]
    return $ mconcat [show $fst info,"/",show day]

calenderMonad::[String ]
calenderMonad=
    monthInfo>>=(\info->
        [1 .. snd info]>>=(\day->
            return $ mconcat [show $fst info,"/",show day]))