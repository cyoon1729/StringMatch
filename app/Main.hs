module Main where

import Lib

main :: IO ()
main = do
    let pattern  = "TCTAGA"
        filePath = "data/human_g1k_v37.txt"
    -- text <- readFile filePath
    -- let matches = rabinKarp pattern text
    matches <- parRabinKarp4 pattern filePath
    print matches 
