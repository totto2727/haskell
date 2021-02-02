{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main::IO ()
main=do
    args<-getArgs
    let fileName=head args
    fileData<-TIO.readFile fileName
    let upperData=T.toUpper fileData
    TIO.writeFile (mconcat [fileName,"_Upper"]) upperData
    print upperData
