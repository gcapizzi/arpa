module Main where

import qualified Data.ByteString.Char8 as ByteString
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.HTTP.Types.Status as Status

import Lib

fileResponse :: FilePath -> Wai.Response
fileResponse path = Wai.responseFile Status.status200 [] path Nothing

requestPath :: Wai.Request -> FilePath
requestPath request = ByteString.unpack $ Wai.rawPathInfo request

application :: Wai.Application
application request respond = respond $ fileResponse $ requestPath request

main :: IO ()
main = Warp.run 8080 application
