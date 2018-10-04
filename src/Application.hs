module Application
    ( application
    ) where

import Data.Foldable (find)
import Data.Maybe (maybe)
import Data.List (isInfixOf)

import qualified Data.ByteString.Char8 as ByteStringChar8
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.ByteString.Lazy.Char8 as ByteStringLazyChar8
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types.Status as Status
import qualified System.Directory as Directory

application :: Wai.Application
application request respond = handle request >>= respond

handle :: Wai.Request -> IO Wai.Response
handle request = maybe (return invalidPathResponse) fileResponse (requestPath request)

invalidPathResponse :: Wai.Response
invalidPathResponse = Wai.responseLBS Status.badRequest400 [] (ByteStringLazyChar8.pack "Invalid path")

fileResponse :: FilePath -> IO Wai.Response
fileResponse path = do
  absolutePath <- Directory.makeAbsolute path
  fileExists <- Directory.doesFileExist absolutePath
  if fileExists
  then
    okResponse <$> ByteStringLazy.readFile absolutePath
  else
    return notFoundResponse

okResponse :: ByteStringLazy.ByteString -> Wai.Response
okResponse = Wai.responseLBS Status.ok200 []

notFoundResponse :: Wai.Response
notFoundResponse = Wai.responseLBS Status.notFound404 [] (ByteStringLazyChar8.pack "File not found")

requestPath :: Wai.Request -> Maybe FilePath
requestPath request = find isValidPath (Just relativePath)
  where
    relativePath = dropRoot $ rawPath request

rawPath :: Wai.Request -> FilePath
rawPath request = ByteStringChar8.unpack $ Wai.rawPathInfo request

isValidPath :: FilePath -> Bool
isValidPath = not . isInfixOf "../"

dropRoot :: FilePath -> FilePath
dropRoot = drop 1
