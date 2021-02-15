import qualified Data.Map                      as Map

type UserName = String
type GameID = Int
type PlayerCredits = Int
type WillCoID = Int

userNameDB :: Map.Map GameID UserName
userNameDB =
    Map.fromList [(1, "aaa"), (2, "bbb"), (3, "ccc"), (4, "ddd"), (5, "eee")]

creditsDB :: Map.Map UserName PlayerCredits
creditsDB = Map.fromList
    [("aaa", 10), ("bbb", 20), ("ccc", 30), ("ddd", 40), ("eee", 50)]

gameIDDB :: Map.Map WillCoID GameID
gameIDDB = Map.fromList [(10, 1), (20, 2), (30, 3), (40, 4), (50, 5)]

lookupUserName :: GameID -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits userName = Map.lookup userName creditsDB

lookupGameID::WillCoID->Maybe GameID
lookupGameID id=Map.lookup id gameIDDB

altLookupCredits :: Maybe UserName -> Maybe PlayerCredits
altLookupCredits Nothing         = Nothing
altLookupCredits (Just userName) = Map.lookup userName creditsDB

-- creditsFromID::GameID->Maybe PlayerCredits
-- creditsFromID=altLookupCredits .lookupUserName

creditsFromID :: GameID -> Maybe PlayerCredits
creditsFromID id = lookupUserName id >>= lookupCredits

creditsFromWillCoID::WillCoID->Maybe PlayerCredits
creditsFromWillCoID id =lookupGameID id>>=lookupUserName>>=lookupCredits

readInt::IO Int
readInt=read <$> getLine

askForName :: IO ()
askForName=putStrLn "what is your name?"

nameStatement::String->String
nameStatement name=mconcat ["Hello, ",name,"!"]

helloWorld :: IO ()
helloWorld=askForName>>getLine >>= return . nameStatement>>=putStrLn

printDouble::Int->IO ()
printDouble int =print $ int*2

printDoubleInt::IO()
printDoubleInt=readInt>>=printDouble

allFmapM::Monad m=>(a->b)->m a->m b
allFmapM func x=x>>= return .func

allApp::Monad m=>m (a->b)->m a->m b
allApp funcM x= funcM <*> x

bind::Maybe a->(a->Maybe b)->Maybe b
bind Nothing _=Nothing
bind (Just x) func=func x