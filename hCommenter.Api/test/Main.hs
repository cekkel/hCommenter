{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Hedgehog
import Servant.QuickCheck (not500, onlyJsonObjects, serverSatisfies, withServantServer, (<%>))
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)
import Test.QuickCheck (Arbitrary (..), oneof, stdArgs)

import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Server
import Utils.ArbitraryInstances

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  exampleHedghehogSpec
  servantQuickcheckSpec

servantQuickcheckSpec :: Spec
servantQuickcheckSpec = around withEnvData $ describe "API" $ do
  it "demonstrates best practices" $ \env -> do
    withServantServer fullAPI (pure $ serverAPI env) $ \baseUrl ->
      serverSatisfies
        fullAPI
        baseUrl
        stdArgs
        ( not500
            <%> onlyJsonObjects
            <%> mempty
        )

exampleHedghehogSpec :: Spec
exampleHedghehogSpec = describe "Example tests" $ do
  it "hedgehog example" $ hedgehog $ do
    x :: Int <- forAll $ Gen.integral (Range.linear 0 1000)
    y :: Int <- forAll $ Gen.integral (Range.linear 0 5000)
    liftIO $ threadDelay (100 * x + y)

withEnvData :: (Env -> IO ()) -> IO ()
withEnvData action = do
  env <- mkEnv
  action env
