module Lib
    ( 
      someFunc
    , rabinKarpRoll
    , rabinKarpMatch
    , rabinKarp
    , rabinKarpOnePass
    , parRabinKarp4
    ) where

import StringMatch.Hash
import StringMatch.RabinKarp
    (
      rabinKarpRoll,
      rabinKarpMatch,
      rabinKarp,
      rabinKarpOnePass
    )
import StringMatch.Parallel (parRabinKarp4)

someFunc :: IO ()
someFunc = putStrLn $ unlines $ map show $ rollingHash "test" "--test-- test"


