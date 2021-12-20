module Main where

import Lib

main :: IO ()
main = do
    let pattern  = "TCTCCCAT"
        filePath = "data/human_g1k_v37.txt"
    -- let pattern  = "Jesus"
    --     filePath = "data/bible.txt"
    -- let pattern  = "question"
    --     filePath = "data/hamlet.txt"
    
    -- text <- readFile filePath
    -- let matches = rabinKarp pattern text

    -- matches <- seqRabinKarp pattern filePath
    -- matches <- parRabinKarp4 pattern filePath
    matches <- parRabinKarpN pattern filePath 32
    -- matches <- parRabinKarp pattern filePath
    -- matches <- parRabinKarp' pattern filePath
    print matches 
