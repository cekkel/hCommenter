{-# LANGUAGE IncoherentInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module ApiRules.Wai where

import Data.Maybe (fromJust)
import Hedgehog
import Hedgehog.Servant
import Network.HTTP.Types (Status (statusCode), hCacheControl, methodGet)
import Network.Wai (Request (..))
import Network.Wai.Test
  ( SResponse (simpleHeaders, simpleStatus)
  , defaultRequest
  , request
  , setPath
  , withSession
  )
import Servant.Client
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Network.HTTP.Client qualified as Client

import RestAPI.Server
import Utils.Generators

runWaiRuleTests :: IO ()
runWaiRuleTests = hspec apiPropertySpec

apiPropertySpec :: Spec
apiPropertySpec = before provideRequestHandler $ describe "API best practices" $ do
  x100 $ do
    it "is applied to Health API" $
      requireApiBestPracticesFor (Proxy @HealthAPI)

    it "is applied to Voting API" $
      requireApiBestPracticesFor (Proxy @VotingAPI)

    x50 $ -- only do 50 since this one is slower
      it "is applied to Comments API" $
        requireApiBestPracticesFor (Proxy @CommentsAPI)

    it "is applied to the full API as a whole" $
      -- Yes this is (mostly) a duplicate of previous tests, it's just in case a new
      -- set of endpoints get added but not added here to the tests.
      -- It should also include the swagger endpoint.
      requireApiBestPracticesFor (Proxy @API)
 where
  x100 = modifyMaxSuccess (const 100)
  x50 = modifyMaxSuccess (const 50)

provideRequestHandler :: IO (Request -> IO SResponse)
provideRequestHandler = do
  myApp <- app <$> readEnv
  pure $ liftIO . withSession myApp . request

genReq apiProxy =
  genRequest apiProxy (genCommentKey :*: genSortBy :*: genText :*: genNewComment :*: genInt64 :*: GNil)
    <&> \makeReq -> toWaiRequest $ makeReq $ fromJust $ parseBaseUrl ""

toWaiRequest :: Client.Request -> Request
toWaiRequest creq =
  defaultRequest {requestMethod = creqMethod, requestHeaders = creqHeaders}
    & flip setPath creqPath
 where
  creqMethod = Client.method creq
  creqPath = Client.path creq
  creqHeaders = Client.requestHeaders creq

requireApiBestPracticesFor apiProxy makeReq = hedgehog $ do
  req <- forAll $ genReq apiProxy
  response <- liftIO (makeReq req)

  alwaysCacheControlOnGetRequests req response
  neverRespondWithInternalError response

------------------------------------------
-- RULES
------------------------------------------

alwaysCacheControlOnGetRequests :: Request -> SResponse -> PropertyT IO ()
alwaysCacheControlOnGetRequests req response = do
  when (requestMethod req == methodGet) $ do
    diff (hCacheControl, "no-cache") elem (simpleHeaders response)

neverRespondWithInternalError :: SResponse -> PropertyT IO ()
neverRespondWithInternalError response = do
  diff (statusCode (simpleStatus response)) (<) 500
