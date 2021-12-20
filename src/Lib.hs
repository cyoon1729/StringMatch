module Lib
    ( 
      someFunc
    , rabinKarpRoll
    , rabinKarpMatch
    , rabinKarp
    , rabinKarpOnePass
    , seqRabinKarp
    , parRabinKarp4
    , parRabinKarpN
    , parRabinKarp
    , parRabinKarp'
    ) where

import StringMatch.Hash
import StringMatch.RabinKarp
    (
      rabinKarpRoll,
      rabinKarpMatch,
      rabinKarp,
      rabinKarpOnePass,
      seqRabinKarp
    )
import StringMatch.Parallel
    (
      parRabinKarp4,
      parRabinKarpN,
      parRabinKarp,
      parRabinKarp'
    )

someFunc :: IO ()
someFunc = putStrLn $ unlines $ map show $ rollingHash "test" "--test-- test"
