module Render
  ( renderFile
  ) where

import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified System.Directory as Directory

renderFile :: FilePath -> IO (Maybe ByteStringLazy.ByteString)
renderFile path = do
  absolutePath <- Directory.makeAbsolute path
  fileExists <- Directory.doesFileExist absolutePath
  if fileExists
  then
    Just <$> ByteStringLazy.readFile absolutePath
  else
    return Nothing
