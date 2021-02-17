module Main where

import Palindrome(isPalindrome)

main::IO ()
main=do
    text<-getLine
    print $isPalindrome  text
