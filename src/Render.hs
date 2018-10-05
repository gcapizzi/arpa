module Render
  ( renderFile
  , Error(..)
  ) where

import Data.List (isInfixOf)
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified System.Directory as Directory

data Error = FileNotFound
           | InvalidPath

renderFile :: FilePath -> IO (Either Error ByteStringLazy.ByteString)
renderFile path
  | isValidPath path = do
    absolutePath <- Directory.makeAbsolute (dropRoot path)
    fileExists <- Directory.doesFileExist absolutePath
    if fileExists
    then
      Right <$> ByteStringLazy.readFile absolutePath
    else
      return $ Left FileNotFound
  | otherwise = return $ Left InvalidPath

isValidPath :: FilePath -> Bool
isValidPath = not . isInfixOf "../"

dropRoot :: FilePath -> FilePath
dropRoot = drop 1
