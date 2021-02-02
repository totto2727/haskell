{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

getData::[String ]->(String ,String)
getData []=("","")
getData (original:copy:_)=(original,copy)

main::IO ()
main=do
    args<-getArgs
    let original =head args
    let copy =args !! 1
    original<-TIO.readFile  original
    TIO.writeFile copy original
    print "copy!"