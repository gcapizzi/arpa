module Render
  ( renderFile
  , Error(..)
  ) where

import Data.List (isInfixOf)
import qualified CMarkGFM as CMark
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Encoding as TextLazyEncoding
import qualified Data.Text.Lazy.IO as TextLazyIO
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

data Error = FileNotFound
           | InvalidPath

renderFile :: FilePath -> IO (Either Error ByteStringLazy.ByteString)
renderFile path
  | FilePath.takeExtension path == ".md" = return $ Left FileNotFound
  | isValidPath path = do
    absolutePath <- Directory.makeAbsolute (dropRoot path)
    fileExists <- Directory.doesFileExist absolutePath
    if fileExists
    then
      Right <$> ByteStringLazy.readFile absolutePath
    else renderMarkdown absolutePath
  | otherwise = return $ Left InvalidPath

renderMarkdown :: FilePath -> IO (Either Error ByteStringLazy.ByteString)
renderMarkdown absolutePath = do
  let extension = FilePath.takeExtension absolutePath
  if extension == "" || extension == ".html"
  then do
    let mdAbsolutePath = FilePath.replaceExtension absolutePath "md"
    mdFileExists <- Directory.doesFileExist mdAbsolutePath
    if mdFileExists
    then do
      mdFileContents <- TextLazyIO.readFile mdAbsolutePath
      let strictMd5FileContents = TextLazy.toStrict mdFileContents
      let strictCommonmark = CMark.commonmarkToHtml [] [] strictMd5FileContents
      let lazyCommonmark = TextLazy.fromStrict strictCommonmark
      return $ Right $ TextLazyEncoding.encodeUtf8 lazyCommonmark
    else
      return $ Left FileNotFound
  else
    return $ Left FileNotFound

isValidPath :: FilePath -> Bool
isValidPath = not . isInfixOf "../"

dropRoot :: FilePath -> FilePath
dropRoot = drop 1
