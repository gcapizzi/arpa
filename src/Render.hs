module Render
  ( renderFile
  , Error
  ) where

import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified System.Directory as Directory

data Error = Error

renderFile :: FilePath -> IO (Either Error ByteStringLazy.ByteString)
renderFile path = do
  absolutePath <- Directory.makeAbsolute path
  fileExists <- Directory.doesFileExist absolutePath
  if fileExists
  then
    Right <$> ByteStringLazy.readFile absolutePath
  else
    return $ Left Error
