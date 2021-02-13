
doorPrice :: [Int]
doorPrice=[1000,2000,3000]

boxPrice :: [Int]
boxPrice=[10000,20000]

boxMagnification :: [Int]
boxMagnification=[10,50]

totalPrice::[Int ]
totalPrice=(+) <$> doorPrice <*> boxPrice

totalPrice1::[Int ]
totalPrice1=(*)<$>doorPrice<*>boxMagnification

primesToN::Int->[Int]
primesToN n=
    let
        twoThroughN=[2 .. n]
        composite=(*) <$> twoThroughN <*> twoThroughN
    in filter (not .(`elem` composite)) twoThroughN

data User=User{
    name::String,
    gameID::Int,
    score::Int
} deriving Show

testName::[String]
testName=[
    "John Smith",
    "Robert'); DROP TABLE Students",
    "Cristina NULL",
    "Randall Munroe"
    ]

testIDs::[Int]
testIDs=[1337,0123,999999]

testScore :: [Int]
testScore=[0,100000,-99999]

testData::[User]
testData=User <$> testName <*>testIDs <*> testScore

allFmap::Applicative f=>(a->b)->f a->f b
allFmap func app=(pure func) <*> app

example::Int
example=(*) ((+) 2 4) 6

exampleMaybe::Maybe Int
exampleMaybe=pure (*) <*> (pure (+) <*> pure 2 <*> pure 4) <*> pure 6

beerPack::[Int]
beerPack=[6,12]

member::[Int]
member=[2,3]

drinkBeerPer::[Int]
drinkBeerPer=[3,4]

buyBeer::Int
buyBeer=
    let
        drinkBeers=(*) <$> drinkBeerPer <*> member
        restBeers=(-)<$> beerPack<*>pure 4
    in maximum  $ (-) <$>drinkBeers <*>restBeers