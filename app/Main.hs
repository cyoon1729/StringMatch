module Main where

import Lib
import System.IO
import System.Directory 

{-
readLines path = do
    content <- readFile path
    return $ lines content

parsePattern :: String -> [String]
parsePattern pattern = do
    let patternIsFile = isFile pattern
    case patternIsFile of
        True  -> readLines pattern
        False -> [pattern]



main :: IO()
main = do
   let pattern = "TCTCCAT"
       patt = parsePattern pattern
   print patt
-}

main :: IO ()
main = do
    -- let pattern  = "TCTCCCAT"
    --    filePath = "data/human_g1k_v37.txt"
    -- let pattern  = "Jesus"
    --     filePath = "data/bible.txt"
    let pattern  = "question"
        filePath = "data/hamlet.txt"
    
    -- text <- readFile filePath
    -- let matches = rabinKarp pattern text

    -- matches <- seqRabinKarp pattern filePath
    matches <- parRabinKarp4 pattern filePath
    -- matches <- parRabinKarpN pattern filePath 32
    -- matches <- parRabinKarp pattern filePath
    -- matches <- parRabinKarp' pattern filePath
    print matches

