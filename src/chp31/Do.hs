import qualified Data.Map as Map

mapPairM :: (Monad m, Ord a) => m (a, a) -> m a
mapPairM pair = (\(x, y) -> if x >= y then x else y) <$> pair

helloPerson :: String -> String
helloPerson name = mconcat ["Hello, ", name, "!"]

hello :: IO ()
hello = do
    name <- getLine
    let mes = helloPerson name
    putStrLn mes

hello1 :: IO ()
hello1 = getLine >>= (return . helloPerson) >>= putStrLn

data Grade=F|D|C|B|A deriving (Eq,Ord,Enum,Show,Read)

data Degree=HS|BA|MS|PhD deriving (Eq,Ord,Enum,Show,Read)

data Candidate = Candidate
    { candidateID :: Int
    , codeReview  :: Grade
    , cultureFit  :: Grade
    , education   :: Degree
    }
    deriving Show

candidate1 :: Candidate
candidate1 = Candidate 1 B B MS

candidate2 :: Candidate
candidate2 = Candidate 2 A A PhD

candidate3 :: Candidate
candidate3 = Candidate 3 A D BA

candidateDB::Map.Map Int Candidate
candidateDB=Map.fromList [(1,candidate1),(2,candidate2),(3,candidate3)]

candidates::[Candidate]
candidates=[candidate1,candidate2,candidate3]

viable :: Candidate -> Bool
viable candidate =
    let passedCording  = codeReview candidate > B
        passCultureFit = cultureFit candidate > C
        educationMin   = education candidate >= MS
        tests          = [passedCording, passCultureFit, educationMin]
    in  all (== True) tests

readInt :: IO Int
readInt = read <$> getLine

readGrade :: IO Grade
readGrade = read <$> getLine

readDegree :: IO Degree
readDegree = read <$> getLine

readCandidate :: IO Candidate
readCandidate = do
    putStrLn "enter id"
    cID <- readInt
    putStrLn "enter code grades"
    codeGrade <- readGrade
    putStrLn "enter culture fit grade"
    cultureGrade <- readGrade
    putStrLn "enter education"
    Candidate cID codeGrade cultureGrade <$> readDegree

assessCandidateIO :: IO String
assessCandidateIO = do
    candidate <- readCandidate
    let passed    = viable candidate
        statement = if passed then "passed" else "failed"
    return statement

assessCandidateMaybe::Int->Maybe String
assessCandidateMaybe cID=do
    candidate<-Map.lookup cID candidateDB
    let passed=viable candidate
        statement=if passed then "passed" else "failed"
    return statement

assessCandidateList::[Candidate]->[String]
assessCandidateList candidates=do
    candidate<-candidates
    let passed=viable candidate
        statement=if passed then "passed" else "failed"
    return statement

assessCandidate::Monad m=>m Candidate->m String
assessCandidate candidateM=do
    candidate<-candidateM
    let passed=viable candidate
        statement=if passed then "passed" else "failed"
    return statement

