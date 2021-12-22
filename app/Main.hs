module Main where

import System.IO
import System.Environment (getArgs)
import Lib (doMatch)


main :: IO [()]
main = do
     args <- getArgs
     doMatch args

