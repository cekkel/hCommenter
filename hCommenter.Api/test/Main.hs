{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Server
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  servantQuickcheckSpec

servantQuickcheckSpec :: Spec
servantQuickcheckSpec = describe "API" $ do
  it "demonstrates best practices" $ hedgehog $ do
    x :: Int <- forAll $ Gen.integral (Range.linear 0 1000)
    y :: Int <- forAll $ Gen.integral (Range.linear 0 5000)
    liftIO $ threadDelay (100 * x + y)
