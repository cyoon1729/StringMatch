module Hash () where

import Data.ByteString.Char8 (ByteString)
import Data.Char (ord)


--| An inefficient but correct rolling hash function for testing
rollHashVerify :: Int -> [Char] -> Int
rollHashVerify winLen lst = map (polySum) $ slidingWindow winLen lst
  where
    polySum xs = foldl (\acc x -> (ord x) + acc * base) 0 xs 
    slidingWindow _ [] = []
    slidingWindow size (x:xs)
        | length (x:xs) >= size = (take size (x:xs)) : slidingWindow size xs
        | otherwise             = []


--| Rabin-Karp rolling hash function.
rollHash :: [Char] -> [Char] -> Int -> Int -> Int -> [Int]
rollHash [] _ winHash _ _ = [winHash]
rollHash (x:xs) (y:ys) winHash currLen winLen
    | currLen == winLen = winHash : rollHash xs ys rollWinHash currLen winLen 
    | otherwise         = rollHash xs (y:ys) nWinHash (currLen + 1) winLen
  where
    nWinHash    = winHash * base + (ord x)
    rollWinHash = (winHash - (ord y) * base^(winLen - 1)) * base + (ord x) 
    base        = 7
    modulo      = 0 
-- | A Rabin-Karp polynomial hash function.
hashFn :: Int -> Int -> Char8 -> Int
hashFN _ _ _ = 0


