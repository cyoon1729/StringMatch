{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StringMatch.FileReader
    (
      readPartition
    , getFileSize
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


-- | Read `partNum`th chunk from a file.
readPartition :: FilePath -> Int -> Int -> Int -> IO DBLC.ByteString
readPartition filePath numParts patternLength partNum = do
    fileSize <- getFileSize filePath
    let fileMode   = Just (CMode 0440)
        partSize   = fileSize `div` (fromIntegral numParts)
        partOffset = partSize * (fromIntegral partNum)
        readSize   = if partNum == (numParts - 1) 
                         then partSize 
                         else partSize + fromIntegral (patternLength - 1)
        readSizeB  = (fromIntegral readSize) :: ByteCount
    fd <- PIO.openFd filePath PIO.ReadOnly fileMode PIO.defaultFileFlags
    chunk <- PIOB.fdPread fd readSizeB partOffset
    return chunk

