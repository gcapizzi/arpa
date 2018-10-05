module Application
    ( application
    ) where

import qualified Data.ByteString.Char8 as ByteStringChar8
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.ByteString.Lazy.Char8 as ByteStringLazyChar8
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types.Status as Status

import qualified Render

application :: Wai.Application
application request respond = handle request >>= respond

handle :: Wai.Request -> IO Wai.Response
handle request = fileResponse (rawPath request)

fileResponse :: FilePath -> IO Wai.Response
fileResponse path = do
  bodyOrError <- Render.renderFile path
  return $ either errorResponse okResponse bodyOrError

okResponse :: ByteStringLazy.ByteString -> Wai.Response
okResponse = Wai.responseLBS Status.ok200 []

errorResponse :: Render.Error -> Wai.Response
errorResponse Render.FileNotFound = responseString Status.notFound404 "File not found"
errorResponse Render.InvalidPath = responseString Status.badRequest400 "Invalid path"

responseString :: Status.Status -> String -> Wai.Response
responseString status body = Wai.responseLBS status [] (ByteStringLazyChar8.pack body)

rawPath :: Wai.Request -> FilePath
rawPath request = ByteStringChar8.unpack $ Wai.rawPathInfo request
