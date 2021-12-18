module StringMatch.RabinKarp
    (
      rabinKarpRoll
    , rabinKarpMatch
    , rabinKarp
    , rabinKarpOnePass
    , seqRabinKarp
    )  where

import qualified Data.Char as DC
import qualified Data.List as DL
import System.Environment(getArgs)
import System.IO(readFile)


-- | Modulo exponentiation, taken from https://gist.github.com/trevordixon/6788535
modExp :: Int -> Int -> Int -> Int
modExp  x y n = mod (x^(mod y (n-1))) (n)


-- | polynomial hash for rabin-karp hashing pattern
polyHash b m str = foldl (\acc a -> polyMod a acc) 0 str
  where
    polyMod a acc = modM $ (DC.ord a) + modM (acc * b)
    modM val      = val `mod` m


-- | Internal Rabin-Karp rolling hash function helper that discards non-matches.
rabinKarpRoll :: [Char] -> Int -> Int -> [(Int, [Char])]
rabinKarpRoll text targetHash ws = roll text "" 0 0
  where
    roll [] subStr hashC idx
      | targetHash == hashC = [(idx, subStr)]
      | otherwise      = []
    roll (x:xs) subStr hashC idx
      | length subStr < ws = roll xs (subStr ++ [x]) hashC' idx 
      | isMatch          = (idx, subStr) : roll xs (ss ++ [x]) hashR (idx + 1)
      | otherwise        = roll xs (ss ++ [x]) hashR (idx + 1)
      where
        isMatch  = length subStr == ws && targetHash == hashC
        hashC'   = modM $ ex + modM (hashC * b)
        hashR    = modM $ ex + modM ((hashC - es * (mExp b (ws - 1))) * b)
        modM val = val `mod` m
        mExp p q = modExp p q m
        (s:ss)   = subStr
        (ex, es) = (DC.ord x, DC.ord s)
        (b, m)   = (31, 100003)


-- | Outputs substrings and their indices that match pattern
rabinKarpMatch :: [Char] -> [(Int, [Char])] -> [Int]
rabinKarpMatch pattern candidates = DL.map (\(idx, _) -> idx) matches
  where
    matches = DL.filter (\(_, str) -> str == pattern) candidates  


-- | Rabin-Karp with decoupled candidate selection and matching.
rabinKarp :: [Char] -> [Char] -> [Int]
rabinKarp pattern text = rabinKarpMatch pattern candidates
  where
    candidates    = rabinKarpRoll text patternHash patternLength
    patternHash   = polyHash 31 100003 pattern
    patternLength = length pattern


-- | Rabin-Karp in one pass.
rabinKarpOnePass :: [Char] -> [Char] -> [Int]
rabinKarpOnePass pattern text = rkRoll text "" 0 0
  where
    rkRoll [] subStr hashC idx
      | pattern == subStr = [idx]
      | otherwise         = []
    rkRoll (x:xs) subStr hashC idx
      | length subStr < ws = rkRoll xs (subStr ++ [x]) hashC' idx 
      | hashMatch          = pattMatch 
      | otherwise          = rkRoll xs (ss ++ [x]) hashR (idx + 1)
      where
        hashMatch = length subStr == ws && targetHash == hashC
        pattMatch = 
            case (pattern == subStr) of
                True -> idx : rkRoll xs (ss ++ [x]) hashR (idx + 1)
                False -> rkRoll xs (ss ++ [x]) hashR (idx + 1)
        hashC'   = modM $ ex + modM (hashC * b)
        hashR    = modM $ ex + modM ((hashC - es * (mExp b (ws - 1))) * b)
        modM val = val `mod` m
        mExp p q = modExp p q m
        (s:ss)   = subStr
        (ex, es) = (DC.ord x, DC.ord s)
    targetHash = polyHash b m pattern
    ws         = length pattern
    (b, m)     = (31, 100003)

seqRabinKarp :: IO ()
seqRabinKarp = do
  args <- getArgs
  (pattern, path) <- case args of
    [pattern, path] -> return (pattern, path)
  readFile path >>= \s -> putStrLn $ unlines $ map show $ rabinKarpOnePass pattern s
