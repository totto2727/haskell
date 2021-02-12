import           Data.Maybe
import qualified Data.Map as Map
import Data.Functor

reverseMaybe :: Maybe String -> Maybe String
reverseMaybe Nothing    = Nothing
reverseMaybe (Just str) = Just . reverse $ str

data RobotPart = RobotPart
    { name        :: String
    , description :: String
    , cost        :: Double
    , count       :: Int
    }
    deriving Show

leftArm :: RobotPart
leftArm = RobotPart "left arm" "left arm for face punching" 1000.00 3

rightArm :: RobotPart
rightArm = RobotPart "right arm" "right arm for kind hand gestures" 1025.00 5

robotHead :: RobotPart
robotHead = RobotPart "robot head" "this head looks mad" 5092.25 2

type Html = String
renderHtml :: RobotPart -> Html
renderHtml part = mconcat
    [ "<h2>"
    , name part
    , "</h2>"
    , "<p>"
    , "<h3>"
    , "desc"
    , "</h3>"
    , description part
    , "</p>"
    , "<p>"
    , "<h3>"
    , "cost"
    , "</h3>"
    , show . cost $ part
    , "</p>"
    , "<p>"
    , "<h3>"
    , "count"
    , "</h3>"
    , show . count $ part
    , "</p>"
    ]

partsDB::Map.Map Int RobotPart
partsDB=Map.fromList . zip [1 ..] $ [leftArm,rightArm,robotHead]

partValue::Maybe RobotPart
partValue = Map.lookup 1 partsDB

partHtml::Maybe Html
partHtml=renderHtml <$> partValue

allParts::[RobotPart]
allParts =snd <$> Map.toList partsDB

allPartsHtml::[Html]
allPartsHtml=renderHtml <$> allParts

htmlPartsDB::Map.Map Int Html
htmlPartsDB=renderHtml <$> partsDB

leftArmIO::IO RobotPart
leftArmIO=return leftArm

htmlSnippet::IO Html
htmlSnippet=renderHtml <$> leftArmIO

newtype Box a=Box a deriving Show

instance Functor Box where
    fmap func (Box x)=Box .func $ x

morePresents::Int->Box a->Box [a]
morePresents =fmap . replicate


unWrap::Box a->a
unWrap (Box x)=x

lookupPart::Int->Maybe RobotPart
lookupPart id =Map.lookup id partsDB

printMaybe::Maybe String->IO ()
printMaybe Nothing =print "Nothing"
printMaybe (Just cost)=print cost

readInt::IO Int
readInt=read <$> getLine

lowerCost::RobotPart->RobotPart->RobotPart
lowerCost p1 p2
    | cost p1<=cost p2=p1
    | otherwise= p2

lookupLowerCost::Int->Int->Maybe RobotPart
lookupLowerCost id1 id2=
    let
        part1=Map.lookup id1 partsDB
        part2=Map.lookup id2 partsDB
    in
        lowerCost <$> part1 <*> part2

printLowerCost::Maybe RobotPart->IO ()
printLowerCost Nothing=putStrLn "存在しないIDが入力されました"
printLowerCost (Just part)=print part

lookupLowerCostIO::IO (Maybe RobotPart)
lookupLowerCostIO=lookupLowerCost <$>readInt <*>readInt

main::IO ()
main=do
    part<-lookupLowerCostIO
    printLowerCost part
-- main::IO ()
-- main =do
--     line<-getLine
--     let id=read line
--         part=lookupPart id
--         theCost=show.cost <$> part
--     printMaybe theCost
