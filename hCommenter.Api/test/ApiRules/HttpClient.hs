{-# OPTIONS_GHC -Wno-missing-signatures #-}

module ApiRules.HttpClient where

import Control.Concurrent (forkIO)
import Data.Data (Proxy (Proxy))
import Data.Maybe (fromJust)
import Hedgehog
import Hedgehog.Servant
import Network.HTTP.Client
  ( Manager
  , Request (..)
  , Response (..)
  , defaultManagerSettings
  , newManager
  , responseClose
  , responseOpen
  , withResponse
  )
import Network.HTTP.Types (Status (statusCode), hCacheControl, methodGet)
import Servant.Client (parseBaseUrl)
import Test.Hspec
import Test.Hspec (describe)
import Test.Hspec.Hedgehog (hedgehog)

import Server
import Utils.Generators (genCommentKey, genKey, genNewComment, genSortBy, genText)

testPort :: Int
testPort = 3001

runInstanceRuleTests :: IO ()
runInstanceRuleTests = do
  _ <- forkIO $ messageConsoleAndRun testPort SQLite

  hspec specs

specs :: Spec
specs = do
  before (newManager defaultManagerSettings) $
    describe "API best practices with a running server" $ do
      it "Health" $ do
        requireApiBestPracticesFor (Proxy @HealthAPI)
      it "Voting" $ do
        requireApiBestPracticesFor (Proxy @VotingAPI)
      it "Comments" $ do
        requireApiBestPracticesFor (Proxy @CommentsAPI)
      it "is applied to whole API" $ do
        requireApiBestPracticesFor (Proxy @API)

requireApiBestPracticesFor apiProxy manager = hedgehog $ do
  req <- forAll $ genReq apiProxy
  response <- liftIO $ responseOpen req manager
  liftIO $ responseClose response

  -- neverRespondWithInternalError response
  alwaysCacheControlOnGetRequests req response

genReq apiProxy =
  genRequest apiProxy (genCommentKey :*: genSortBy :*: genText :*: genNewComment :*: GNil)
    <&> \makeReq -> makeReq $ fromJust $ parseBaseUrl $ "localhost:" ++ show testPort

------------------------------------------
-- RULES
------------------------------------------

alwaysCacheControlOnGetRequests :: Request -> Response a -> PropertyT IO ()
alwaysCacheControlOnGetRequests req response = do
  when (method req == methodGet) $ do
    diff (hCacheControl, "no-cache") elem (responseHeaders response)

neverRespondWithInternalError :: Response a -> PropertyT IO ()
neverRespondWithInternalError response = do
  diff (statusCode $ responseStatus response) (<) 500
