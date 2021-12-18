module Lib
    ( 
      someFunc
    , rabinKarpRoll
    , rabinKarpMatch
    , rabinKarp
    , rabinKarpOnePass
    ) where

import StringMatch.Hash
{-
import StringMatch.RabinKarp
    (
      rabinKarpRoll,
      rabinKarpMatch,
      rabinKarp,
      rabinKarpOnePass,
      seqRabinKarp
    )
-}
import StringMatch.ParRabinKarp
    (
      rabinKarpRoll,
      rabinKarpMatch,
      rabinKarp,
      rabinKarpOnePass,
      parRabinKarp
    )

someFunc :: IO ()
--someFunc = putStrLn $ unlines $ map show $ rabinKarpOnePass "test" "--test-- test"
someFunc = parRabinKarp


