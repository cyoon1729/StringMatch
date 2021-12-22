{-
 - Implements parallelization of the Rabin-Karp algorithm
-}
module StringMatch.Parallel
    (
      parRabinKarpN
    ) where

import qualified Data.ByteString.Lazy.Char8 as DBL
import Control.Parallel.Strategies (using, parList, runEval, rseq, rdeepseq)
import Control.DeepSeq

import StringMatch.RabinKarp (rabinKarp, rabinKarpOnePass)
import StringMatch.FileReader (getFileSize, readPartition)


-- | Run Rabin Karp in parallel, splitting text into n partitions.
parRabinKarpN :: String -> String -> Int -> IO [Int]
parRabinKarpN pattern filePath n = do
    fileSize <- getFileSize filePath
    partitions <- mapM (readPartition filePath n (length pattern)) [0..(n-1)]
    let partsB  = map DBL.unpack partitions
        matches = runEval $ do
            let ms = (map (rabinKarpOnePass pattern) partsB) `using` parList rdeepseq
            return ms
    let indicesByPart          = zip [0..(n-1)] $ map (map fromIntegral) matches 
        partSize               = (fromIntegral fileSize) `div` n
        applyOffset (np, idcs) = map (np * partSize +) idcs
        offsetCorrected        = map applyOffset indicesByPart
    return $ concat offsetCorrected

