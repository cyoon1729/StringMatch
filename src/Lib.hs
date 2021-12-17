module Lib
    ( 
      someFunc
    , rabinKarpRoll
    , rabinKarpMatch
    , rabinKarp
    , rabinKarpOnePass
    ) where

import StringMatch.Hash
import StringMatch.RabinKarp
    (
      rabinKarpRoll,
      rabinKarpMatch,
      rabinKarp,
      rabinKarpOnePass
    )

someFunc :: IO ()
someFunc = putStrLn $ unlines $ map show $ rollingHash "test" "--test-- test"


