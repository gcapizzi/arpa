{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Control.Concurrent
import Control.Exception (catch, SomeException)
import Control.Monad (void)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Types.Status
import qualified Network.HTTP.Client as HTTPClient
import qualified Network.Wai.Handler.Warp as Warp
import qualified Application

data Response = Response Status ByteString deriving (Eq, Show)

get :: HTTPClient.Manager -> String -> IO Response
get manager reqPath = do
  req <- HTTPClient.parseRequest $ "GET http://localhost:8080" ++ reqPath
  res <- HTTPClient.httpLbs req manager
  return $ Response (HTTPClient.responseStatus res) (HTTPClient.responseBody res)

setUp :: IO HTTPClient.Manager
setUp = do
  _ <- forkIO $ Warp.run 8080 Application.application
  manager <- HTTPClient.newManager HTTPClient.defaultManagerSettings
  waitForServer manager
  return manager

waitForServer :: HTTPClient.Manager -> IO ()
waitForServer manager = catch (void $ get manager "/") retry
  where
    retry :: SomeException -> IO ()
    retry _ = waitForServer manager

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

    context "when requesting an Markdown file" $ do
      context "with the .md extension" $
        it "returns a 404 response" $ \manager ->
          get manager "/test/fixtures/mattina.md" `shouldReturn` Response notFound404 "File not found"

      context "with the .html extension" $
        it "returns the rendered file" $ \manager ->
          get manager "/test/fixtures/mattina.html" `shouldReturn` Response ok200 "<p>M'illumino\nd'immenso</p>\n"

      context "with no extension" $
        it "returns the rendered file" $ \manager ->
          get manager "/test/fixtures/mattina" `shouldReturn` Response ok200 "<p>M'illumino\nd'immenso</p>\n"

      context "with another random extension" $
        it "returns a 404 response" $ \manager ->
          get manager "/test/fixtures/mattina.csv" `shouldReturn` Response notFound404 "File not found"
