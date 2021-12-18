module StringMatch.ParRabinKarp
    (
      rabinKarpRoll
    , rabinKarpMatch
    , rabinKarp
    , rabinKarpOnePass
    , parRabinKarp
    )  where

import qualified Data.Char as DC
import qualified Data.List as DL
import System.Environment(getArgs)
import System.IO(readFile, SeekMode(RelativeSeek))
import System.Posix.IO
import System.Posix.Types(FileOffset)
import System.Directory
import qualified System.Posix.IO.ByteString as BS
import Control.Parallel.Strategies
import Control.DeepSeq

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

shift :: Int -> [Int] -> [Int]
shift x inds = map (+x) inds

parRabinKarp :: IO ()
parRabinKarp = do
  args <- getArgs
  (pattern, path) <- case args of
    [pattern, path] -> return (pattern, path)
  filesize <- getFileSize path
  let b = ceiling $ (fromIntegral filesize) / 4
      bt = fromIntegral b
      lenp = length pattern
  fd1 <- openFd path ReadOnly Nothing defaultFileFlags
  fd2 <- openFd path ReadOnly Nothing defaultFileFlags
  fd3 <- openFd path ReadOnly Nothing defaultFileFlags
  fd4 <- openFd path ReadOnly Nothing defaultFileFlags
  fdSeek fd2 RelativeSeek (fromIntegral $ b * 1)
  fdSeek fd3 RelativeSeek (fromIntegral $ b * 2)
  fdSeek fd4 RelativeSeek (fromIntegral $ b * 3)
  (s1, _) <- fdRead fd1 $ fromIntegral (b + lenp - 1)
  (s2, _) <- fdRead fd2 $ fromIntegral (b + lenp - 1)
  (s3, _) <- fdRead fd3 $ fromIntegral (b + lenp - 1)
  (s4, _) <- fdRead fd4 $ fromIntegral (b + lenp - 1)
  let solutions = runEval $ do
          a1 <- rpar (force (rabinKarpOnePass pattern s1))
          a2 <- rpar (force (rabinKarpOnePass pattern s2))
          a3 <- rpar (force (rabinKarpOnePass pattern s3))
          a4 <- rpar (force (rabinKarpOnePass pattern s4))
          _ <- rseq a1
          _ <- rseq a2
          _ <- rseq a3
          _ <- rseq a4
          let a2' = shift (fromIntegral $ b * 1) a2
          let a3' = shift (fromIntegral $ b * 2) a3
          let a4' = shift (fromIntegral $ b * 3) a4
          return (a1 ++ a2' ++ a3' ++ a4')
  --mapM_ print solutions
  putStrLn $ unlines $ map show solutions
