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
      rabinKarpOnePass,
      seqRabinKarp
    )

someFunc :: IO ()
--someFunc = putStrLn $ unlines $ map show $ rabinKarpOnePass "test" "--test-- test"
someFunc = seqRabinKarp


