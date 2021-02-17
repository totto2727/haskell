module Palindrome (isPalindrome,preProcess) where

import Data.Char (toLower,isSpace,isPunctuation)

stripWhiteSpace::String ->String
stripWhiteSpace=filter (not .isSpace )

toLowerCase::String ->String
toLowerCase=map toLower

stripPunctuation::String->String
stripPunctuation=filter (not.isPunctuation )

preProcess::String ->String
preProcess=toLowerCase .stripWhiteSpace .stripPunctuation

isPalindrome::String ->Bool
isPalindrome text=
    let cleanText=preProcess text
    in cleanText==reverse cleanText
