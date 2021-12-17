module StringMatch.SeqHash where

-- import Data.ByteString.Char8 (ByteString)
import Data.Char (ord)


-- | Modulo exponentiation, taken from https://gist.github.com/trevordixon/6788535
modExp :: Int -> Int -> Int -> Int
modExp  x y n = mod (x^(mod y (n-1))) (n)


-- | polynomial hash for rabin-karp hashing pattern
polyHash b m str = foldl (\acc a -> polyMod a acc) 0 str
  where
    polyMod a acc = modM $ (ord a) + modM (acc * b)
    modM val      = val `mod` m


-- | An inefficient but correct rolling hash function for testing
rollHashVerify :: Int -> [Char] -> [Int]
rollHashVerify winLen lst = map (polyHash b m) $ slidingWindow winLen lst
  where
    slidingWindow _ [] = []
    slidingWindow size (x:xs)
        | length (x:xs) >= size = (take size (x:xs)) : slidingWindow size xs
        | otherwise             = []
    (b, m) = (31, 100003) 


-- | Rabin-Karp rolling hash helper function that discards non-matches on-the-fly.
rollHashMatch :: [Char] -> [Char] -> Int -> Int -> Int -> Int -> Int -> [Int]  -- Yuck! Disgusting!
rollHashMatch [] _ idx hashT hashC _ _
    | hashT == hashC = [idx]
    | otherwise      = []
rollHashMatch (x:xs) (y:ys) idx hashT hashC nc nw
    | nc < nw   = rollHashMatch xs (y:ys) idx hashT hashC' (nc + 1) nw 
    | isMatch   = idx : rollHashMatch xs ys (idx + 1) hashT hashR nc nw
    | otherwise = rollHashMatch xs ys (idx + 1) hashT hashR nc nw
  where 
    isMatch  = nc == nw && hashT == hashC
    hashC'   = modM $ ex + modM (hashC * b)
    hashR    = modM $ ex + modM ((hashC - ey * (mExp b (nw - 1))) * b)
    modM val = val `mod` m
    mExp p q = modExp p q m
    (ex, ey) = (ord x, ord y)
    (b, m)   = (31, 100003)


-- | Rolling hash function, finds indices in `text` at which the `pattern` appears.
rollingHash :: [Char] -> [Char] -> [Int]
rollingHash pattern text = rollHashMatch text text 0 hashP 0 0 (length pattern)
  where
    hashP = polyHash b m pattern
    (b, m) = (31, 100003)
