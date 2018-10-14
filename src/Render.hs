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
  | not (isValidPath path) = return $ Left InvalidPath
  | isHiddenPath path = return $ Left FileNotFound
  | isDynamicPath path = renderDynamicFile path
  | otherwise = serveStaticFile path

isValidPath :: FilePath -> Bool
isValidPath = not . isInfixOf "../"

isHiddenPath :: FilePath -> Bool
isHiddenPath path = FilePath.takeExtension path == ".md"

isDynamicPath :: FilePath -> Bool
isDynamicPath path = extension == ".html" || extension == ""
  where extension = FilePath.takeExtension path

renderDynamicFile :: FilePath -> IO (Either Error ByteStringLazy.ByteString)
renderDynamicFile path = do
  absolutePath <- makeAbsolutePath path
  fileExists <- Directory.doesFileExist absolutePath
  if fileExists
  then Right <$> ByteStringLazy.readFile absolutePath
  else renderMarkdownFile absolutePath

renderMarkdownFile :: FilePath -> IO (Either Error ByteStringLazy.ByteString)
renderMarkdownFile absolutePath = do
  let mdAbsolutePath = FilePath.replaceExtension absolutePath "md"
  mdFileExists <- Directory.doesFileExist mdAbsolutePath
  if mdFileExists
  then Right . renderMarkdown <$> TextLazyIO.readFile mdAbsolutePath
  else return $ Left FileNotFound

renderMarkdown :: TextLazy.Text -> ByteStringLazy.ByteString
renderMarkdown = TextLazyEncoding.encodeUtf8 . TextLazy.fromStrict . CMark.commonmarkToHtml [] [] . TextLazy.toStrict

serveStaticFile :: FilePath -> IO (Either Error ByteStringLazy.ByteString)
serveStaticFile path = do
  absolutePath <- makeAbsolutePath path
  fileExists <- Directory.doesFileExist absolutePath
  if fileExists
  then Right <$> ByteStringLazy.readFile absolutePath
  else return $ Left FileNotFound

makeAbsolutePath :: FilePath -> IO FilePath
makeAbsolutePath = Directory.makeAbsolute . dropRoot

dropRoot :: FilePath -> FilePath
dropRoot = drop 1
