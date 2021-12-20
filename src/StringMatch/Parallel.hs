module StringMatch.Parallel
    (
      parRabinKarp4
    , parRabinKarp
    , parRabinKarp'
    ) where

import qualified Data.ByteString.Char8 as DB
import qualified Data.ByteString.Lazy.Char8 as DBL
import Control.Parallel.Strategies
import Control.DeepSeq
import System.Posix.IO
import System.Posix.Types
import System.IO(readFile, SeekMode(RelativeSeek))
import qualified System.Directory as SD

import StringMatch.RabinKarp (rabinKarpRoll, rabinKarpMatch, rabinKarp, rabinKarpOnePass)
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
    let offsetCorrected = map (\(nChunk, idxs) -> map ((nChunk * chunkSize)+) (map fromIntegral idxs)) matches
        matchesFinal    = map fromIntegral $ concat offsetCorrected
    return matchesFinal


parRabinKarp :: [Char] -> [Char] -> IO [Int]
parRabinKarp pattern filePath = do
  filesize <- getFileSize filePath
  let b = ceiling $ (fromIntegral filesize) / 4
      bt = fromIntegral b
      lenp = length pattern
  fd1 <- openFd filePath ReadOnly Nothing defaultFileFlags
  fd2 <- openFd filePath ReadOnly Nothing defaultFileFlags
  fd3 <- openFd filePath ReadOnly Nothing defaultFileFlags
  fd4 <- openFd filePath ReadOnly Nothing defaultFileFlags
  fdSeek fd2 RelativeSeek (fromIntegral $ b * 1)
  fdSeek fd3 RelativeSeek (fromIntegral $ b * 2)
  fdSeek fd4 RelativeSeek (fromIntegral $ b * 3)
  (s1, _) <- fdRead fd1 $ fromIntegral (b + lenp - 1)
  (s2, _) <- fdRead fd2 $ fromIntegral (b + lenp - 1)
  (s3, _) <- fdRead fd3 $ fromIntegral (b + lenp - 1)
  (s4, _) <- fdRead fd4 $ fromIntegral (b + lenp - 1)
  let solutions = runEval $ do
          a1 <- rpar (force (rabinKarp pattern s1))
          a2 <- rpar (force (rabinKarp pattern s2))
          a3 <- rpar (force (rabinKarp pattern s3))
          a4 <- rpar (force (rabinKarp pattern s4))
          _ <- rseq a1
          _ <- rseq a2
          _ <- rseq a3
          _ <- rseq a4
          let a2' = map (+ (fromIntegral $ b * 1)) a2
          let a3' = map (+ (fromIntegral $ b * 2)) a3
          let a4' = map (+ (fromIntegral $ b * 3)) a4
          return (a1 ++ a2' ++ a3' ++ a4')
  return solutions


parRabinKarp' :: [Char] -> [Char] -> IO [Int]
parRabinKarp' pattern filePath = do
  filesize <- getFileSize filePath
  let b = ceiling $ (fromIntegral filesize) / 4
      bt = fromIntegral b
      lenp = length pattern
      fds = runEval $ do
          fd1 <- rpar (openFd filePath ReadOnly Nothing defaultFileFlags)
          fd2 <- rpar (openFd filePath ReadOnly Nothing defaultFileFlags)
          fd3 <- rpar (openFd filePath ReadOnly Nothing defaultFileFlags)
          fd4 <- rpar (openFd filePath ReadOnly Nothing defaultFileFlags)
          _ <- rseq fd1
          _ <- rseq fd2
          _ <- rseq fd3
          _ <- rseq fd4
          return (fd1, fd2, fd3, fd4)
  let (fd1', fd2', fd3', fd4') = fds
  fd1 <- fd1'
  fd2 <- fd2'
  fd3 <- fd3'
  fd4 <- fd4'
  {-
  fdSeek fd2 RelativeSeek (fromIntegral $ b * 1)
  fdSeek fd3 RelativeSeek (fromIntegral $ b * 2)
  fdSeek fd4 RelativeSeek (fromIntegral $ b * 3)
  -}
  let fdSeeks = runEval $ do
          t2 <- rpar(fdSeek fd2 RelativeSeek (fromIntegral $ b * 1))
          t3 <- rpar(fdSeek fd3 RelativeSeek (fromIntegral $ b * 2))
          t4 <- rpar(fdSeek fd4 RelativeSeek (fromIntegral $ b * 3))
          _ <- rseq t2
          _ <- rseq t3
          _ <- rseq t4
          return (t2, t3, t4)
  let (t2'', t3'', t4'') = fdSeeks
  t2' <- t2''
  t3' <- t3''
  t4' <- t4''
  let tforce = runEval $ do
          let t2 = force $ toInteger t2'
          let t3 = force $ toInteger t3'
          let t4 = force $ toInteger t4'
          return (t2, t3, t4)
  let (t2, t3, t4) = tforce
  let fdReads = runEval $ do
          s1 <- rpar(fdRead fd1 $ fromIntegral (b + lenp - 1))
          s2 <- rpar(fdRead fd2 $ fromIntegral (b + lenp - 1))
          s3 <- rpar(fdRead fd3 $ fromIntegral (b + lenp - 1))
          s4 <- rpar(fdRead fd4 $ fromIntegral (b + lenp - 1))
          _ <- rseq s1
          _ <- rseq s2
          _ <- rseq s3
          _ <- rseq s4
          return (s1, s2, s3, s4)
  let (s1'', s2'', s3'', s4'') = fdReads
  (s1', _) <- s1''
  (s2', _) <- s2''
  (s3', _) <- s3''
  (s4', _) <- s4''
  let sforce = runEval $ do
          let s1 = rpar $ force s1'
          let s2 = rpar $ force s2'
          let s3 = rpar $ force s3'
          let s4 = rpar $ force s4'
          _ <- rseq s1
          _ <- rseq s2
          _ <- rseq s3
          _ <- rseq s4
          return (s1, s2, s3, s4)
  let (s1, s2, s3, s4) = sforce
  let solutions = runEval $ do
          a1 <- rpar (force (rabinKarpOnePass pattern $ runEval s1))
          a2 <- rpar (force (rabinKarpOnePass pattern $ runEval s2))
          a3 <- rpar (force (rabinKarpOnePass pattern $ runEval s3))
          a4 <- rpar (force (rabinKarpOnePass pattern $ runEval s4))
          _ <- rseq a1
          _ <- rseq a2
          _ <- rseq a3
          _ <- rseq a4
          let a2' = map (+ (fromIntegral $ b * 1)) a2
          let a3' = map (+ (fromIntegral $ b * 2)) a3
          let a4' = map (+ (fromIntegral $ b * 3)) a4
          return (a1 ++ a2' ++ a3' ++ a4')
  --putStrLn $ unlines $ map show solutions
  return solutions
