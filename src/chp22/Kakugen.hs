import System.Environment
import Control.Monad
import Data.List.Split

reply::String->String
reply str
    |str=="1"="one"
    |str=="2"="two"
    |str=="3"="three"
    |str=="4"="four"
    |str=="5"="five"
    |otherwise = "zero"

replys::[String ]->[String ]
replys []= []
replys ("n":_)= ["finish"]
replys (x:xs)=reply x:replys xs

main::IO ()
main=do
    input<-getContents
    let line=lines input
    let kakugen=replys line
    mapM_ print kakugen
