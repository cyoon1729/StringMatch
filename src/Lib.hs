module Lib
    ( someFunc
    ) where
import StringMatch.Hash

someFunc :: IO ()
someFunc = putStrLn $ unlines $ map show $ rollingHash "test" "--test-- test"
