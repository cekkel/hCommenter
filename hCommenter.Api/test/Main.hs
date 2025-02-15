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

import ApiRules.HttpClient (runInstanceRuleTests)
import ApiRules.Wai
import Server

main :: IO ()
main = do
  putStrLn "Hi there"

-- runWaiRuleTests

-- runInstanceRuleTests
