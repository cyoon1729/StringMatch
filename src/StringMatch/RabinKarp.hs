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
import qualified Data.Set as DS
import qualified Data.Map as DM

type HashValue = Int


-- | Modulo exponentiation, taken from https://gist.github.com/trevordixon/6788535
modExp :: Int -> Int -> Int -> Int
modExp x y n = mod (x^(mod y (n-1))) (n)


-- | polynomial hash for rabin-karp hashing pattern
polyHash :: Int -> Int -> [Char] -> HashValue
polyHash b m str = foldl (\acc a -> polyMod a acc) 0 str
  where
    polyMod a acc = modM $ (DC.ord a) + modM (acc * b)
    modM val      = val `mod` m


-- | Internal Rabin-Karp rolling hash function helper that discards non-matches.
rabinKarpRoll :: [Char] -> HashValue -> Int -> [(Int, [Char])]
rabinKarpRoll text targetHash ws = roll text "" 0 0
  where
    roll [] subStr hashC idx
      | targetHash == hashC = [(idx, subStr)]
      | otherwise      = []
    roll (x:xs) subStr hashC idx
      | length subStr < ws = roll xs (subStr ++ [x]) hashC' idx 
      | isMatch            = (idx, subStr) : roll xs (ss ++ [x]) hashR (idx + 1)
      | otherwise          = roll xs (ss ++ [x]) hashR (idx + 1)
      where
        isMatch  = length subStr == ws && targetHash == hashC
        hashC'   = modM $ ex + modM (hashC * b)
        hashR    = modM $ ex + modM ((hashC - es * (mExp b (ws - 1))) * b)
        modM val = val `mod` m
        mExp p q = modExp p q m
        (s:ss)   = subStr
        (ex, es) = (DC.ord x, DC.ord s)
        (b, m)   = (31, 100003)


-- | Internal Rabin-Karp rolling hash function helper that discards non-matches.
rabinKarpRollMulti :: [Char] -> DS.Set HashValue -> Int -> [(Int, [Char])]
rabinKarpRollMulti text targets ws = roll text "" 0 0
  where
    roll [] subStr hashC idx
      | DS.member hashC targets  = [(idx, subStr)]
      | otherwise                = []
    roll (x:xs) subStr hashC idx
      | length subStr < ws = roll xs (subStr ++ [x]) hashC' idx 
      | isMatch            = (idx, subStr) : roll xs (ss ++ [x]) hashR (idx + 1)
      | otherwise          = roll xs (ss ++ [x]) hashR (idx + 1)
      where
        isMatch  = length subStr == ws && DS.member hashC targets
        hashC'   = modM $ ex + modM (hashC * b)
        hashR    = modM $ ex + modM ((hashC - es * (mExp b (ws - 1))) * b)
        modM val = val `mod` m
        mExp p q = modExp p q m
        (s:ss)   = subStr
        (ex, es) = (DC.ord x, DC.ord s)
        (b, m)   = (31, 100003)


-- | Outputs indices that match pattern.
rabinKarpMatch :: [Char] -> [(Int, [Char])] -> [Int]
rabinKarpMatch pattern candidates = DL.map (\(idx, _) -> idx) matches
  where
    matches = DL.filter (\(_, str) -> str == pattern) candidates


-- | Outputs indices that match patterns.
rabinKarpMatchMulti :: DS.Set [Char] -> [(Int, [Char])] -> [([Char], [Int])] 
rabinKarpMatchMulti patterns candidates = sortIdxs matches  
  where
    sortIdxs      = DL.map (\(patt, idxs) -> (patt, DL.sort idxs)) 
    matches       = DL.filter (\(a, _) -> DS.member a patterns) candidateIdxs
    candidateIdxs = DM.toList $ DM.fromListWith (++) flipPadded
    flipPadded    = DL.map (\(idx, str) -> (str, [idx])) candidates


-- | Rabin-Karp with decoupled candidate selection and matching.
rabinKarp :: [Char] -> [Char] -> [Int]
rabinKarp pattern text = rabinKarpMatch pattern candidates
  where
    candidates    = rabinKarpRoll text patternHash patternLength
    patternHash   = polyHash 31 100003 pattern
    patternLength = length pattern


-- | Same-length multi-pattern rabin-karp.
rabinKarpMulti :: [[Char]] -> [Char] -> [([Char], [Int])] 
rabinKarpMulti patterns text = rabinKarpMatchMulti patternSet candidates
  where
    patternSet    = DS.fromList patterns
    candidates    = rabinKarpRollMulti text patternHashes patternLength
    patternHashes = DS.fromList $ DL.map (polyHash 31 100003) patterns
    patternLength = length (head patterns)


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


seqRabinKarp :: [Char] -> [Char] -> IO [Int]
seqRabinKarp pattern filePath = do
    file <- readFile filePath
    let matchesFinal = rabinKarpOnePass pattern file
    return matchesFinal
