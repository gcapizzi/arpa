{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Network.HTTP.Client
import Network.HTTP.Types.Status

get manager path = do
  req <- parseRequest $ "GET http://localhost:8080" ++ path
  httpLbs req manager

main :: IO ()
main = hspec $ beforeAll (newManager defaultManagerSettings) $
  describe "arpa" $
    context "when the requested file does not exist" $
      it "returns a 404 response" $ \manager -> do
        response <- get manager "/foo"

        responseStatus response `shouldBe` notFound404
        responseBody response `shouldBe` "File not found"
