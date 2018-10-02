{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Control.Concurrent
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client
import Network.HTTP.Types.Status
import qualified Network.Wai.Handler.Warp as Warp
import qualified Application

data Response = Response Status ByteString deriving (Eq, Show)

get manager path = do
  req <- parseRequest $ "GET http://localhost:8080" ++ path
  res <- httpLbs req manager
  return $ Response (responseStatus res) (responseBody res)

setUp = do
  forkIO $ Warp.run 8080 Application.application
  newManager defaultManagerSettings

main :: IO ()
main = hspec $ beforeAll setUp $
  describe "arpa" $ do
    it "serves the file corresponding to the specified path, relative the the working dir" $ \manager ->
      get manager "/test/fixtures/mattina.txt" `shouldReturn` Response ok200 "M'illumino\nd'immenso\n"

    context "when the requested file does not exist" $
      it "returns a 404 response" $ \manager ->
        get manager "/foo" `shouldReturn` Response notFound404 "File not found"

    context "when the path contains a parent dir" $
      it "returns a 400 Bad Request" $ \manager -> do
        get manager "/test/../test/fixtures/mattina.txt" `shouldReturn` Response badRequest400 "Invalid path"
        get manager "/test/..%2Ftest/fixtures/mattina.txt" `shouldReturn` Response notFound404 "File not found"
        get manager "/test/fixtures/foo..txt" `shouldReturn` Response notFound404 "File not found"
