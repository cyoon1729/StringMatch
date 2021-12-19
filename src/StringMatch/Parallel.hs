module StringMatch.Parallel
    (
      parRabinKarp4
    ) where

import qualified Data.ByteString.Char8 as DB
import qualified Data.ByteString.Lazy.Char8 as DBL
import Control.Parallel.Strategies
import Control.DeepSeq
import System.Environment (getArgs)
import System.Posix.Types

import StringMatch.RabinKarp (rabinKarpRoll, rabinKarpMatch, rabinKarp)
import StringMatch.FileReader (getFileSize, readChunk)


parRabinKarp4 :: [Char] -> [Char] -> IO [Int]
parRabinKarp4 pattern filePath = do
    fileSize <- getFileSize filePath
    let chunkSize   = fromIntegral (fileSize `div` 4) 
        chunkOffset = (fileSize `div` 4) :: FileOffset
    chunk1 <- readChunk filePath chunkSize chunkOffset 0 
    chunk2 <- readChunk filePath chunkSize chunkOffset 1
    chunk3 <- readChunk filePath chunkSize chunkOffset 2
    chunk4 <- readChunk filePath chunkSize chunkOffset 3
    let matches = runEval $ do
            m1 <- rpar $ force (rabinKarp pattern (DBL.unpack chunk1))
            m2 <- rpar $ force (rabinKarp pattern (DBL.unpack chunk2))
            m3 <- rpar $ force (rabinKarp pattern (DBL.unpack chunk3))
            m4 <- rpar $ force (rabinKarp pattern (DBL.unpack chunk4))
            _ <- rseq m1
            _ <- rseq m2
            _ <- rseq m3
            _ <- rseq m4
            return [(0, m1), (1, m2), (2, m3), (3, m4)]
    let offsetCorrected = map (\(nChunk, idxs) -> map (nChunk+) idxs) matches
        matchesFinal    = concat offsetCorrected
    return matchesFinal

