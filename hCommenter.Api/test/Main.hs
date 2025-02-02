{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Data.Data (Proxy (Proxy))
import Hedgehog
import Hedgehog.Internal.Property (PropertyT (PropertyT))
import Network.Wai (Application, Request)
import Network.Wai.Test (SResponse, Session, request, withSession)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import ApiProperties
import Server

main :: IO ()
main = do
  hspec spec

spec :: Spec
spec = do
  apiPropertySpec
  exampleHedghehogSpec

apiPropertySpec :: Spec
apiPropertySpec = before provideRequestHandler $ describe "API best practices" $ do
  x100 $ do
    it "is applied to Health API" $
      requireApiBestPracticesFor (Proxy @HealthAPI)

    it "is applied to Voting API" $
      requireApiBestPracticesFor (Proxy @VotingAPI)

    it "is applied to Comments API" $
      requireApiBestPracticesFor (Proxy @CommentsAPI)

    it "is applied to the full API as a whole" $
      -- Yes this is (mostly) a duplicate of previous tests, it's just in case a new
      -- set of endpoints get added but not added here to the tests.
      requireApiBestPracticesFor (Proxy @API)
 where
  x100 = modifyMaxSuccess (const 100)

exampleHedghehogSpec :: Spec
exampleHedghehogSpec = describe "Example tests" $ do
  it "hedgehog example" $ hedgehog $ do
    x :: Int <- forAll $ Gen.integral (Range.linear 0 1000)
    y :: Int <- forAll $ Gen.integral (Range.linear 0 5000)
    liftIO $ threadDelay (10 * x + y)

provideRequestHandler :: IO (Request -> IO SResponse)
provideRequestHandler = do
  myApp <- myapi
  pure $ \req -> do
    liftIO $ withSession myApp $ do
      request req

myapi :: IO Application
myapi = do
  env <- mkEnv
  pure $ app env

-- waiHedgehogTesting :: Property
-- waiHedgehogTesting = Group "API Properties" [
--     property $
--   ]
--   sequential $ it "should always have Cache Control for get requests" $ \ctx -> hedgehog $ do
--     x :: Int <- forAll $ Gen.integral (Range.linear 0 5)
--     liftIO $ withApplication (snd ctx) $ do
--       get "/health" `shouldRespondWith` 200 {matchHeaders = ["Cache-Control" <:> if x == 3 then fromString (show x) else "no-cache"]}

-- liftIO $ threadDelay (100 * x + y)
