{-
 - Implements the Rabin-Karp algorithm for single and multiple
   pattern matching
-}
module StringMatch.RabinKarp
    (
      rabinKarp
    , rabinKarpMulti
    )  where


import qualified Data.Char as DC
import qualified Data.List as DL
import qualified Data.Set as DS
import qualified Data.Map as DM
import qualified Data.Bits as DB


type HashValue = Int


-- | Modular exponentiation, taken from https://gist.github.com/trevordixon/6788535
modExp :: Int -> Int -> Int -> Int
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (DB.shiftR e 1) m `mod` m
  where
    t = if DB.testBit e 0 then b `mod` m else 1


-- | polynomial hash for rabin-karp hashing pattern
polyHash :: Int -> Int -> String -> HashValue
polyHash b m str = foldl (\acc c -> polyMod c acc) 0 str
  where
    polyMod c acc = modM $ (DC.ord c) + modM (acc * b)
    modM val      = val `mod` m


{- Modules for single-pattern Rabin-Karp String Matching -}

-- | Internal Rabin-Karp rolling hash function helper that discards non-matches.
rabinKarpRoll :: String -> HashValue -> Int -> [(Int, String)]
rabinKarpRoll text targetHash ws = roll text "" 0 0
  where
    roll [] subStr hashC idx
      | targetHash == hashC = [(idx, subStr)]
      | otherwise      = []
    roll (x:xs) subStr hashC idx
      | isShorter = roll xs (subStr ++ [x]) hashC' idx 
      | isMatch   = (idx, subStr) : roll xs (ss ++ [x]) hashR (idx + 1)
      | otherwise = roll xs (ss ++ [x]) hashR (idx + 1)
      where
        isShorter = length subStr < ws
        isMatch   = length subStr == ws && targetHash == hashC
        hashC'    = modM $ ex + modM (hashC * b)
        hashR     = modM $ ex + modM ((hashC - es * (modExp b (ws - 1) m)) * b)
        modM val  = val `mod` m
        (s:ss)    = subStr
        (ex, es)  = (DC.ord x, DC.ord s)
        (b, m)    = (31, 100003)


-- | Outputs indices that match pattern.
rabinKarpMatch :: String -> [(Int, String)]-> [Int]
rabinKarpMatch pattern candidates = DL.map (\(idx, _) -> idx) matches
  where
    matches = DL.filter (\(_, str) -> str == pattern) candidates


-- | Rabin-Karp with decoupled candidate selection and matching.
rabinKarp :: String -> String -> [Int]
rabinKarp pattern text = rabinKarpMatch pattern candidates
  where
    candidates    = rabinKarpRoll text patternHash patternLength
    patternHash   = polyHash 31 100003 pattern
    patternLength = length pattern


{- Modules for multi-pattern Rabin-Karp string matching -}

-- | Internal Rabin-Karp rolling hash function helper that discards non-matches.
rabinKarpRollMulti :: String -> DS.Set HashValue -> Int -> [(Int, String)]
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


-- | Outputs indices that match patterns.
rabinKarpMatchMulti :: DS.Set String -> [(Int, String)] -> [(String, [Int])] 
rabinKarpMatchMulti patterns candidates = sortIdxs matches  
  where
    sortIdxs      = DL.map (\(patt, idxs) -> (patt, DL.sort idxs)) 
    matches       = DL.filter (\(a, _) -> DS.member a patterns) candidateIdxs
    candidateIdxs = DM.toList $ DM.fromListWith (++) flipPadded
    flipPadded    = DL.map (\(idx, str) -> (str, [idx])) candidates


-- | Performs fixed-length multi-pattern rabin-karp matching.
rabinKarpMulti :: [String] -> String -> [(String, [Int])] 
rabinKarpMulti patterns text = rabinKarpMatchMulti patternSet candidates
  where
    patternSet    = DS.fromList patterns
    candidates    = rabinKarpRollMulti text patternHashes patternLength
    patternHashes = DS.fromList $ DL.map (polyHash 31 100003) patterns
    patternLength = length (head patterns)

