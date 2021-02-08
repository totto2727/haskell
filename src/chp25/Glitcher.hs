{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.Random
import Control.Monad

sortSection::Int->Int->BC.ByteString ->BC.ByteString
sortSection start size bytes=
    let
        (before,rest)=BC.splitAt start bytes
        (target,after)=BC.splitAt size rest
        changed=BC.reverse target
    in
        mconcat [before,changed,after]

randomSortSection::BC.ByteString ->IO BC.ByteString
randomSortSection bytes=do
    let size=25
    let bytesLength=BC.length bytes
    start<-randomRIO (0,bytesLength-size)
    return $ sortSection start size bytes

intToChar::Int ->Char
intToChar int=toEnum (int `mod` 255)

intToBC::Int->BC.ByteString
intToBC int=BC.pack [intToChar int]

replaceByte::Int->Int ->BC.ByteString ->BC.ByteString
replaceByte location char bytes=
    let
        (before,rest)=BC.splitAt location  bytes
        after=BC.drop 1 rest
    in
        mconcat [before,intToBC char,after]

randomReplaceBytes::BC.ByteString ->IO BC.ByteString
randomReplaceBytes bytes=do
    let bytesLength=BC.length bytes
    location<-randomRIO (0,bytesLength)
    int<-randomRIO (0,255)
    return $ replaceByte location int  bytes

glitcheActions::[BC.ByteString ->IO BC.ByteString]
glitcheActions=mconcat [replicate 3 randomSortSection ,replicate 2 randomReplaceBytes]

main::IO ()
main=do
    args<-getArgs
    let fileName=head args
    imageFile<-BC.readFile fileName
    gliched<-foldM (\bytes func->func bytes) imageFile glitcheActions
    let glichedFileName=mconcat ["glitched_",fileName]
    BC.writeFile glichedFileName gliched
    print "done"

    -- args<-getArgs
    -- let fileName=head args
    -- imageFile<-BC.readFile fileName
    -- glitchedImageFile<-randomSortSection imageFile
    -- BC.writeFile (mconcat ["glitched_",fileName]) glitchedImageFile
    -- print "done"

-- main::IO ()
-- main=do
--     args<-getArgs
--     let fileName=head getArgs
--     imageFile<-BC.readFile fileName
