{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Hedgehog
import Hedgehog.Internal.Property (PropertyT (PropertyT))
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)
import Test.Hspec.Wai
import Test.Hspec.Wai.Internal (runWithState, withApplication)

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
apiPropertySpec = describe "API" $ do
  modifyMaxSuccess (const 200) $
    it "demonstrates best practices" $
      hedgehog $ do
        response <- ApiProperties.getTestResponse

        alwaysCacheControlOnGetRequests response
        neverRespondWithInternalError response

exampleHedghehogSpec :: Spec
exampleHedghehogSpec = describe "Example tests" $ do
  it "hedgehog example" $ hedgehog $ do
    x :: Int <- forAll $ Gen.integral (Range.linear 0 1000)
    y :: Int <- forAll $ Gen.integral (Range.linear 0 5000)
    liftIO $ threadDelay (10 * x + y)

-- waiHedgehogTesting :: Property
-- waiHedgehogTesting = Group "API Properties" [
--     property $
--   ]
--   sequential $ it "should always have Cache Control for get requests" $ \ctx -> hedgehog $ do
--     x :: Int <- forAll $ Gen.integral (Range.linear 0 5)
--     liftIO $ withApplication (snd ctx) $ do
--       get "/health" `shouldRespondWith` 200 {matchHeaders = ["Cache-Control" <:> if x == 3 then fromString (show x) else "no-cache"]}

-- liftIO $ threadDelay (100 * x + y)
