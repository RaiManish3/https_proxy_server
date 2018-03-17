{-# LANGUAGE TypeSynonymInstances #-}
module Hex where

unhex :: String -> Int
unhex xs = unhex' (reverse xs) 0
unhex' [] _ = 0
unhex' (x:xs) i = (c x) * (16^i) + unhex' xs (i+1)


c :: Char -> Int
c '0' =  0
c '1' =  1
c '2' =  2
c '3' =  3
c '4' =  4
c '5' =  5
c '6' =  6
c '7' =  7
c '8' =  8
c '9' =  9
c 'A' =  10
c 'B' =  11
c 'C' =  12
c 'D' =  13
c 'E' =  14
c 'F' =  15
c 'a' =  10
c 'b' =  11
c 'c' =  12
c 'd' =  13
c 'e' =  14
c 'f' =  15
c _   = error "Invalid hex digit!"
