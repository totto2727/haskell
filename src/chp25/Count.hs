{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

main::IO ()
main=do
    args<-getArgs
    let fileName=head args
    fileData<-B.readFile fileName
    let bytesLength=B.length fileData
    let textLength=T.length . TE.decodeUtf8 $ fileData
    print . mconcat $ [show bytesLength,":",show textLength]