import           Control.Applicative
import           Control.Monad

data Name = Name
    { firstName :: String
    , lastName  :: String
    }
instance Show Name where
    show (Name first last) = mconcat [first, " ", last]

data GradeLevel=Freshman|Sophomore|Junior|Senior deriving (Eq,Ord,Enum,Show)

data Student = Student
    { studentID   :: Int
    , gradeLevel  :: GradeLevel
    , studentName :: Name
    }
    deriving Show

data Teacher = Teacher
    { teacherID   :: Int
    , teacherName :: Name
    }
    deriving Show

data Course = Course
    { courseID    :: Int
    , courseTitle :: String
    , teacher     :: Int
    }
    deriving Show

data HINQ m a b =HINQ (m a->m b) (m a) (m a->m a)|HINQ_ (m a->m b) (m a)

data Enrollment = Enrollment
    { student :: Int
    , course  :: Int
    }
    deriving Show

students :: [Student]
students =
    [ Student 1 Freshman  (Name "1" "a")
    , Student 2 Sophomore (Name "2" "b")
    , Student 3 Junior    (Name "3" "c")
    , Student 4 Senior    (Name "4" "d")
    , Student 5 Freshman  (Name "5" "e")
    ]

teachers :: [Teacher]
teachers = [Teacher 1 (Name "t1" "aa"), Teacher 2 (Name "t2" "bb")]

courses :: [Course]
courses = [Course 1 "c1" 1, Course 2 "c2" 2]

enrollments :: [Enrollment]
enrollments =
    [ Enrollment 1 1
    , Enrollment 2 2
    , Enrollment 3 1
    , Enrollment 4 2
    , Enrollment 5 1
    , Enrollment 1 2
    ]

_select :: Monad m => (a -> b) -> m a -> m b
_select prop values = prop <$> values

_where :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
_where test values = do
    value <- values
    guard $ test value
    return value

_join
    :: (Eq c, Monad m, Alternative m)
    => m a
    -> m b
    -> (a -> c)
    -> (b -> c)
    -> m (a, b)
_join data1 data2 prop1 prop2 = do
    d1 <- data1
    d2 <- data2
    let dPair = (d1, d2)
    guard $ (\(x, y) -> (==) (prop1 x) (prop2 y)) dPair
    return (d1, d2)


_hinq :: (Monad m, Alternative m) => (m a -> m b) -> m a -> (m a -> m a) -> m b
_hinq selectQ joinQ whereQ = selectQ . whereQ $ joinQ

runHINQ :: (Monad m, Alternative m) => HINQ m a b -> m b
runHINQ (HINQ_ selectQ joinQ      ) = _hinq selectQ joinQ (_where (const True))
runHINQ (HINQ selectQ joinQ whereQ) = _hinq selectQ joinQ whereQ

query1 :: HINQ [] (Teacher, Course) Name
query1 = HINQ_ (_select (teacherName . fst))
               (_join teachers courses teacherID teacher)

query2 :: HINQ [] (Teacher, Course) Name
query2 = HINQ (_select (teacherName . fst))
              (_join teachers courses teacherID teacher)
              (_where ((== "c1") . courseTitle . snd))

query3 :: HINQ [] (Teacher, Course) Name
query3 = HINQ (_select (teacherName . fst))
              (_join teachers courses teacherID teacher)
              (_where ((== "c3") . courseTitle . snd))


possibleTeacher :: Maybe Teacher
possibleTeacher = Just $ head teachers

possibleCourse :: Maybe Course
possibleCourse = Just $ head courses

maybeQuery1 :: HINQ Maybe (Teacher, Course) Name
maybeQuery1 = HINQ (_select (teacherName . fst))
                   (_join possibleTeacher possibleCourse teacherID teacher)
                   (_where ((== "c1") . courseTitle . snd))

maybeQuery2 :: HINQ Maybe (Teacher, Course) Name
maybeQuery2 = HINQ (_select (teacherName . fst))
                   (_join possibleTeacher possibleCourse teacherID teacher)
                   (_where ((== "c3") . courseTitle . snd))

studentEnrollmentsQ :: HINQ [] (Student, Enrollment) (Name, Int)
studentEnrollmentsQ = HINQ_
    (_select (\(st, en) -> (studentName st, course en)))
    (_join students enrollments studentID student)

studentEnrollments :: [(Name, Int)]
studentEnrollments = runHINQ studentEnrollmentsQ

c1StudentsQ :: HINQ [] ((Name, Int), Course) Name
c1StudentsQ = HINQ (_select (fst . fst))
                   (_join studentEnrollments courses snd courseID)
                   (_where ((== "c1") . courseTitle . snd))

getEnrollments :: String -> [Name]
getEnrollments courseName = runHINQ courseQuery
  where
    courseQuery = HINQ (_select (fst . fst))
                       (_join studentEnrollments courses snd courseID)
                       (_where ((== courseName) . courseTitle . snd))
