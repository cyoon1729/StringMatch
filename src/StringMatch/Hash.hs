module Hash () where

import qualified Data.ByteString.Char8


-- | A Rabin-Karp polynomial hash function.
hashFn :: Int -> Int -> Char8 -> Int


-- | Compute rolling hash on string.
-- | Returns list of [(hash_value, location)].
rollingHash :: ByteString -> Int -> [(Int, Int)]


