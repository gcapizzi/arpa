module Application
    ( application
    ) where

import Data.Maybe (maybe)
import Data.List (isInfixOf)

import qualified Data.ByteString.Char8 as ByteStringChar8
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.ByteString.Lazy.Char8 as ByteStringLazyChar8
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types.Status as Status

application :: Wai.Application
application request respond = respond $ maybe invalidPathResponse fileResponse (fsPath request)

invalidPathResponse :: Wai.Response
invalidPathResponse = Wai.responseLBS Status.badRequest400 [] (ByteStringLazyChar8.pack "Invalid path")

fileResponse :: FilePath -> Wai.Response
fileResponse path = Wai.responseFile Status.ok200 [] path Nothing

fsPath :: Wai.Request -> Maybe FilePath
fsPath request
  | ".." `isInfixOf` requestPath = Nothing
  | otherwise = Just $ "." ++ requestPath
  where
    requestPath = rawPath request

rawPath :: Wai.Request -> FilePath
rawPath request = ByteStringChar8.unpack $ Wai.rawPathInfo request
