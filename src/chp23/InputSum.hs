{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy                     as T
import qualified Data.Text.Lazy.IO                  as TIO
import           System.Environment

sums :: Int -> [Int] -> [Int]
sums x []        = []
sums x (x1 : xs) = sumNow : sums sumNow xs where sumNow = x + x1

-- main :: IO ()
-- main = do
--     input <- getContents
--     let inputToLines=lines input
--     let numbers = map read inputToLines
--     let sumInts = sums 0 numbers
--     mapM_ print sumInts

main::IO ()
main=do
    input<-TIO.getContents
    let inputToLines=T.lines input
    let ints=map (read.T.unpack) inputToLines
    let sumInts=sums 0 ints
    mapM_ print sumInts
