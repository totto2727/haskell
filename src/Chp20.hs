import           Data.List
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.Semigroup

file1 :: [(Int, Double)]
file1 = [(1, 1.1), (2, 2.2), (3, 3.3), (4, 4.4), (5, 5.5)]

file2 :: [(Int, Double)]
file2 = [(4, 4.1), (6, 6.3), (7, 7.4), (8, 8.5), (10, 10.6)]

file3 :: [(Int, Double)]
file3 = [(9, 9.1), (10, 10.2), (12, 12.3), (14, 14.4), (15, 15.5)]

data TS a = TS [Int] [Maybe a]
  deriving Show

instance Semigroup (TS a) where
  (<>) = combineTS

instance Monoid (TS a) where
  mempty  = TS [] []
  mappend = (<>)

ts1 :: TS Double
ts1 = fileToTS file1

ts2 :: TS Double
ts2 = fileToTS file2

ts3 :: TS Double
ts3 = fileToTS file3

ts :: TS Double
ts = mconcat [ts1, ts2, ts3]

createTS :: [Int] -> [a] -> TS a
createTS times values =
  let completeTimes  = [minimum times .. maximum times]
      timeValueMap   = Map.fromList $ zip times values
      extendedValues = map (`Map.lookup` timeValueMap) completeTimes
  in  TS completeTimes extendedValues

fileToTS :: [(Int, a)] -> TS a
fileToTS file =
  let times  = map fst file
      values = map snd file
  in  createTS times values

insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertMaybePair myMap (_, Nothing) = myMap
insertMaybePair myMap (k, Just v ) = Map.insert k v myMap

combineTS :: TS a -> TS a -> TS a
combineTS (TS [] []) ts2        = ts2
combineTS ts1        (TS [] []) = ts1
combineTS (TS t1 v1) (TS t2 v2) =
  let bothTimes      = mconcat [t1, t2]
      completeTimes  = [minimum bothTimes .. maximum bothTimes]
      tvMap          = foldl insertMaybePair Map.empty $ zip t1 v1
      updatedMap     = foldl insertMaybePair tvMap $ zip t2 v2
      combinedValues = map (`Map.lookup` updatedMap) completeTimes
  in  TS completeTimes combinedValues

mean :: Real a => [a] -> Double
mean xs =
  let total = realToFrac . sum $ xs
      count = realToFrac . length $ xs
  in  total / count

meanTS :: Real a => TS a -> Maybe Double
meanTS (TS _ []) = Nothing
meanTS (TS _ v ) = if all (== Nothing) v
  then Nothing
  else Just . mean . map (\(Just x) -> x) . filter (/= Nothing) $ v

type CompareFunc a = a -> a -> a

type TSCompareFunc a = CompareFunc (Int, Maybe a)

makeTSCompare :: Eq a => CompareFunc a -> TSCompareFunc a
makeTSCompare func =
  let newFunc (t1, Nothing) (_ , Nothing) = (t1, Nothing)
      newFunc (t1, v1     ) (_ , Nothing) = (t1, v1)
      newFunc (_ , Nothing) (t2, v2     ) = (t2, v2)
      newFunc (t1, Just v1) (t2, Just v2) =
        if func v1 v2 == v1 then (t1, Just v1) else (t2, Just v2)
  in  newFunc

minTS :: Ord a => TSCompareFunc a
minTS = makeTSCompare min

maxTS :: Ord a => TSCompareFunc a
maxTS = makeTSCompare max

diffPair :: Num a => Maybe a -> Maybe a -> Maybe a
diffPair Nothing  _        = Nothing
diffPair _        Nothing  = Nothing
diffPair (Just x) (Just y) = Just $ x - y

diffTS :: Num a => TS a -> TS a
diffTS (TS t v) = TS t $ zipWith diffPair v $ Nothing : v

meanMaybe :: Real a => [Maybe a] -> Maybe Double
meanMaybe xs =
  if Nothing `elem` xs then Nothing else Just . mean . map fromJust $ xs

movingArg :: Real a => [Maybe a] -> Int -> [Maybe Double]
movingArg [] n = []
movingArg xs n =
  let calcXs = take n xs
      restXs = tail xs
  in  if length calcXs == n then meanMaybe calcXs : movingArg restXs n else []

maTS :: Real a => TS a -> Int -> TS Double
maTS (TS [] []) _ = TS [] []
maTS (TS t v) n =
  let ma             = movingArg v n
      nothings       = replicate (n `div` 2) Nothing
      smoothedValues = mconcat [nothings, ma, nothings]
  in  TS t smoothedValues
