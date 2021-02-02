{-# LANGUAGE OverloadedStrings #-}

module Text where

import qualified Data.Text as T

someText::T.Text
someText="Some\ntest for\t you"

cutText::[T.Text]
cutText=T.words someText

