module Application
    ( application
    ) where

import qualified Data.ByteString.Char8 as ByteString
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types.Status as Status

application :: Wai.Application
application request respond = respond $ fileResponse $ relativePath request

fileResponse :: FilePath -> Wai.Response
fileResponse path = Wai.responseFile Status.status200 [] path Nothing

relativePath :: Wai.Request -> FilePath
relativePath request = "." ++ rawPath request

rawPath :: Wai.Request -> FilePath
rawPath request = ByteString.unpack $ Wai.rawPathInfo request
