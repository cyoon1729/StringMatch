{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StringMatch.FileReader
    (
      readChunk,
      getFileSize
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


-- | Read `chunkNum`th chunk from a file.
readChunk
    :: FilePath
    -> ByteCount
    -> FileOffset
    -> FileOffset
    -> IO DBLC.ByteString
readChunk filePath chunkSize chunkOffset chunkNum = do
    let fileMode = Just (CMode 0440)
    fd <- PIO.openFd filePath PIO.ReadOnly fileMode PIO.defaultFileFlags
    chunk <- PIOB.fdPread fd chunkSize (chunkOffset * chunkNum)
    return chunk

