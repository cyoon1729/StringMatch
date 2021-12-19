module Main where

import Lib

main :: IO ()
main = do
    let pattern  = "TCTCCCAT"
        filePath = "data/human_g1k_v37.txt"
    -- let pattern  = "question"
    --     filePath = "data/hamlet.txt"
    -- text <- readFile filePath
    -- let matches = rabinKarp pattern text
    -- matches <- parRabinKarp4 pattern filePath
    matches <- seqRabinKarp pattern filePath
    print matches 
