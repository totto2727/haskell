import System.Environment
import Control.Monad

toInt ::String ->[Int]
toInt=map read . lines

main::IO ()
main=do
    input<-getContents
    let ints=toInt input
    print $ sum ints

-- main::IO ()
-- main=do
--     input<-getContents
--     let reversedInput=reverse input
--     mapM_ print reversedInput

