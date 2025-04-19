{-# OPTIONS_GHC -Wno-missing-signatures #-}

module ApiRules.HttpClient where

import Control.Concurrent (forkIO)
import Data.Data (Proxy (Proxy))
import Data.Maybe (fromJust)
import Hedgehog
import Hedgehog.Servant
import Network.HTTP.Client
  ( Request (..)
  , Response (..)
  , defaultManagerSettings
  , newManager
  , responseClose
  , responseOpen
  )
import Network.HTTP.Types (Status (statusCode), hCacheControl, methodGet)
import Servant.Client (parseBaseUrl)
import System.Environment (setEnv)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import RestAPI.Server
import Utils.Generators (genCommentKey, genInt64, genNewComment, genSortBy, genText)

testPort :: Int
testPort = 3001

runInstanceRuleTests :: IO ()
runInstanceRuleTests = do
  setEnv "APP__PORT" (show testPort)

  _ <- forkIO messageConsoleAndRun

  hspec specs

specs :: Spec
specs = do
  before (newManager defaultManagerSettings) $
    x10 $ -- These are really slow, so we run them less often
      describe "API best practices with a running server" $ do
        it "Health" $ do
          requireApiBestPracticesFor (Proxy @HealthAPI)
        it "Voting" $ do
          requireApiBestPracticesFor (Proxy @VotingAPI)
        it "Comments" $ do
          requireApiBestPracticesFor (Proxy @CommentsAPI)
        it "is applied to whole API" $ do
          requireApiBestPracticesFor (Proxy @API)
 where
  x10 = modifyMaxSuccess (const 10)

genReq apiProxy =
  genRequest apiProxy (genCommentKey :*: genSortBy :*: genText :*: genNewComment :*: genInt64 :*: GNil)
    <&> \makeReq -> makeReq $ fromJust $ parseBaseUrl $ "localhost:" ++ show testPort

requireApiBestPracticesFor apiProxy manager = hedgehog $ do
  req <- forAll $ genReq apiProxy
  response <- liftIO $ responseOpen req manager
  liftIO $ responseClose response

  alwaysCacheControlOnGetRequests req response
  neverRespondWithInternalError response

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
