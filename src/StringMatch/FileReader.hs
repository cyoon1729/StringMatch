{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StringMatch.Reader
    (
      loadChunks
    ) where

import qualified Data.ByteString.Lazy as DBL
import qualified Data.ByteString.Lazy.Char8 as DBLC
import qualified System.Posix.IO as PIO
import qualified "unix-bytestring" System.Posix.IO.ByteString.Lazy as PIOB
import System.Posix.Types
import qualified System.Posix as P


getFileSize :: String -> IO FileOffset
getFileSize path = do
    stat <- P.getFileStatus path
    return (P.fileSize stat)


loadReaders chunkReader numChunks chunkOffset k 
    | k == numChunks = chunk : loadReaders chunkReader numChunks chunkOffset (k + 1)
    | otherwise      = []
  where
    chunk = chunkReader (chunkOffset * k) 


loadChunks filePath numChunks = do
    fileSize <- getFileSize filePath
    let chunkSize   = fromIntegral $ fileSize `div` numChunks
        chunkOffset::FileOffset = (fileSize `div` numChunks) :: FileOffset
    fd <- PIO.openFd filePath PIO.ReadOnly (Just (CMode 0440)) PIO.defaultFileFlags
    let chunkReader = PIOB.fdPread fd chunkSize
        readers     = loadReaders chunkReader numChunks chunkOffset 0
    return readers




