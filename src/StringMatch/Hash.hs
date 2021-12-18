module StringMatch.Hash where

-- import Data.ByteString.Char8 (ByteString)
import Data.Char (ord)
import Control.Parallel.Strategies (using, parList, rseq)
import Control.Monad (forM_)
import Data.Char (chr)
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peek)
import System.IO.MMap (Mode(ReadOnly), mmapFileForeignPtr)
import System.Environment(getArgs)


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


prollingHash :: [Char] -> [Char] -> Int -> [Int]
prollingHash ppattern ptext pn = phelper len2 (fmap (rollingHash ppattern) partitions `using` parList rseq)
  where
    partitions = partition len1 len2 ptext
    len1 = len2 + (lenp - 1)
    len2 = (lent `div` pn)
    lenp = length ppattern
    lent = length ptext

phelper :: Int -> [[Int]] -> [Int]
phelper x inds = concat $ zipWith (\a b -> map (+b) a) inds lx
  where
    lx = map (*x) [0..]

-- | Partitioning function without pointers
partition :: Int -> Int -> [Char] -> [[Char]]
partition _ _ [] = []
partition n i xs = partition' : partition n i (drop i xs)
    where partition' = take n xs

-- pointer stuff
doStuff :: ForeignPtr Word8 -> Int -> IO ()
doStuff fp i =
  withForeignPtr fp $ \p -> do
    let addr = p `plusPtr` i
    val <- peek addr :: IO Word8
    print $ chr $ fromIntegral val

seqHash :: IO ()
seqHash = do
  args <- getArgs
  (apattern, path) <- case args of
    [apattern, path] -> return (apattern, path)
  (p,offset,size) <- mmapFileForeignPtr path ReadOnly Nothing
  forM_ [0 .. size-1] $ \i -> do
    doStuff p (offset + i)
