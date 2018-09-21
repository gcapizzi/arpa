module Main where

import qualified Data.ByteString.Char8 as ByteString
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.HTTP.Types.Status as Status

import Lib

application :: Wai.Application
application request respond = respond $ Wai.responseFile Status.status200 [] path Nothing
  where
    path = ByteString.unpack $ Wai.rawPathInfo request

main :: IO ()
main = Warp.run 8080 application
