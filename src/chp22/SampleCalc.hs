import Control.Monad
import Data.List.Split

main::IO ()
main=do
    str<-getLine
    let result=calc str
    print result

stringToInts::[String] ->[Int]
stringToInts = map read

calcPlus::String ->Int
calcPlus= sum . stringToInts . splitOn "+"
calcmulti::String ->Int
calcmulti= product . stringToInts . splitOn "*"

calc::String ->Int
calc str
    | '*' `elem` str = calcmulti str
    | '+' `elem` str = calcPlus str
    | otherwise = 0