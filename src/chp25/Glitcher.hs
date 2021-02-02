{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

main::IO ()
main=do
    args<-getArgs
    let fileName=head getArgs
    imageFile<-BC.readFile fileName
    