module Lib
    ( someFunc
    ) where

import qualified Data.Text                     as T

someFunc :: IO ()
someFunc = print test
--someFunc = putStrLn "someFunc"

test :: T.Text
test = "aaa"
