module Application
    ( application
    ) where

import qualified Data.ByteString.Char8 as ByteString
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types.Status as Status

application :: Wai.Application
application request respond = respond $ fileResponse $ requestPath request

fileResponse :: FilePath -> Wai.Response
fileResponse path = Wai.responseFile Status.status200 [] path Nothing

requestPath :: Wai.Request -> FilePath
requestPath request = ByteString.unpack $ Wai.rawPathInfo request
