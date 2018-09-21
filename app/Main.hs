{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.HTTP.Types.Status as Status

import Lib

application :: Wai.Application
application _ respond = respond $ Wai.responseLBS Status.status200 [] "Hello, World!"

main :: IO ()
main = Warp.run 8080 application
