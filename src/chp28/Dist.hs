import qualified Data.Map                      as Map

type LatLong = (Double, Double)

locationDB :: Map.Map String LatLong
locationDB = Map.fromList
    [ ("Arkham"   , (42.6054, -70.7829))
    , ("Innsmouth", (42.8250, -70.8150))
    , ("Carcosa"  , (29.9714, -90.7694))
    , ("New York" , (40.7776, -73.9691))
    ]

toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

latLongToRads :: LatLong -> (Double, Double)
latLongToRads (lat, long) =
    let rlat  = toRadians lat
        rlong = toRadians long
    in  (lat, long)

haversine :: LatLong -> LatLong -> Double
haversine coords1 coords2 =
    let
        (rlat1, rlong1) = latLongToRads coords1
        (rlat2, rlong2) = latLongToRads coords2
        dlat            = rlat2 - rlat1
        dlong           = rlong2 - rlong1
        a = sin (dlat / 2) ^ 2 + cos rlat1 * cos rlat2 * sin (dlong / 2) ^ 2
        c               = 2 * atan2 (sqrt a) (sqrt (1 - a))
        earthRadius     = 3961.0
    in
        earthRadius * c

haversineIO::IO LatLong->IO LatLong->IO Double
haversineIO v1 v2=haversine <$> v1<*>v2

printDistance :: Maybe Double -> IO ()
printDistance Nothing         = putStrLn "Error, invalid city entered"
printDistance (Just distance) = putStrLn . mconcat $ [show distance, " miles"]

addMaybe::Maybe Int->Maybe Int->Maybe Int
addMaybe Nothing _=Nothing
addMaybe _ Nothing =Nothing
addMaybe (Just x) (Just y)=Just (x+y)

main ::IO ()
main=do
    putStrLn "enter start city name"
    startCityName<-getLine
    let startCity=Map.lookup startCityName locationDB
    putStrLn "enter destination city"
    distCityName<-getLine
    let distCity=Map.lookup distCityName locationDB
        distance=haversine <$> startCity <*> distCity
    printDistance distance