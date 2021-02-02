{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO

main :: IO ()
main = do
    TIO.putStrLn "What's your name?"
    input <- TIO.getLine
    let reply = mconcat ["Hello, ", input, "!"]
    TIO.putStrLn reply
