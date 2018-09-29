{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Network.HTTP.Client
import Network.HTTP.Types.Status

get manager path = do
  req <- parseRequest $ "GET http://localhost:8080" ++ path
  httpLbs req manager

main :: IO ()
main = hspec $ beforeAll (newManager defaultManagerSettings) $
  describe "arpa" $ do
    it "serves the file corresponding to the specified path, relative the the working dir" $ \manager -> do
      response <- get manager "/test/fixtures/mattina.txt"

      responseStatus response `shouldBe` ok200
      responseBody response `shouldBe` "M'illumino\nd'immenso\n"

    context "when the requested file does not exist" $
      it "returns a 404 response" $ \manager -> do
        response <- get manager "/foo"

        responseStatus response `shouldBe` notFound404
        responseBody response `shouldBe` "File not found"

    context "when the path contains .." $
      it "returns a 400 Bad Request" $ \manager -> do
        response <- get manager "/test/../test/fixtures/mattina.txt"

        responseStatus response `shouldBe` badRequest400
        responseBody response `shouldBe` "Invalid path"
