module Main where

import qualified Network.Wai.Handler.Warp as Warp

import Application

main :: IO ()
main = Warp.run 8080 application
