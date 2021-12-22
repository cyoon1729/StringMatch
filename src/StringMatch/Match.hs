{-
 - Implements utilities to parse program arguments and 
   perform match
-}
module StringMatch.Match 
    (
      doMatch
    ) where


import System.IO
import StringMatch.Parallel (parRabinKarpN)
import StringMatch.RabinKarp (rabinKarp)


usage :: [String]
usage = [
          "Usage:"
        , "- To match non-parallel, where input pattern is given as string:" 
        , "     stack run pattern path_to_text"
        , "- To match parallel, where input pattern is given as a string:"
        , "     stack run p pattern path_to_text num_partitions"
        , "- To match non-parallel, where input pattern is given as a file path:"
        , "     stack run f path_to_pattern path_to_text"
        , "- To match parallel, where input pattern is given as a file path:"
        , "     stack run pf path_to_pattern path_to_text"
        ]

foundPattern :: String -> String
foundPattern pattern = "Found \"" ++ pattern ++ "\" in locations:" 


-- | Match in sequential, where pattern is given as a string
matchStr :: String -> String -> IO [Int]
matchStr pattern filePath = do
    text    <- readFile filePath 
    return $ rabinKarp pattern text


-- | Match in parallel, where pattern in given as a string
matchStrPar :: String -> String -> Int -> IO [Int]
matchStrPar pattern filePath numPartitions = do
    matches <- parRabinKarpN pattern filePath numPartitions
    return matches


-- | Match in sequential, where pattern is given as a file
matchFile :: String -> String -> IO [Int]
matchFile patternPath filePath = do
    text     <- readFile filePath
    patternRaw <- readFile patternPath
    let pattern = filter (/='\n') patternRaw
    return $ rabinKarp pattern text


-- | Match in parallel, where pattern is given as a file
matchFilePar :: String -> String -> Int -> IO [Int]
matchFilePar patternPath filePath numPartitions = do
    text <- readFile filePath
    patternRaw <- readFile patternPath
    let pattern = filter (/='\n') patternRaw
    matches <- parRabinKarpN pattern filePath numPartitions
    return matches


-- | Parse program arguments and perform specified match
doMatch :: [String] -> IO [()]
doMatch args = do
    case args of
        [pattern, fPath] -> do
            matches <- matchStr pattern fPath
            putStrLn $ foundPattern pattern 
            mapM putStrLn $ map show matches
        ["p", pattern, filePath, numPartitions] -> do
            let numParts = read numPartitions :: Int
            matches <- matchStrPar pattern filePath numParts 
            putStrLn $ foundPattern pattern
            mapM putStrLn $ map show matches 
        ["f", pattPath, filePath] -> do
            patternRaw <- readFile pattPath
            let pattern = filter (/='\n') patternRaw
            matches <- matchFile pattPath filePath
            putStrLn $ foundPattern pattern
            mapM putStrLn $ map show matches 
        ["pf", pattPath, filePath, numPartitions] -> do 
            patternRaw <- readFile pattPath
            let pattern  = filter (/='\n') patternRaw
                numParts = read numPartitions :: Int
            matches <- matchFilePar pattPath filePath numParts
            putStrLn $ foundPattern pattern
            mapM putStrLn $ map show matches 
        _ -> do
            mapM putStrLn usage


