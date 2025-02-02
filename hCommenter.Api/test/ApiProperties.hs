{-# LANGUAGE TemplateHaskell #-}

module ApiProperties where

import Data.Data (Proxy (Proxy))
import Data.Function ((&))
import Data.Maybe (fromJust)
import Hedgehog
import Hedgehog.Servant
import Network.HTTP.Types (Status (statusCode), hCacheControl, methodGet, methodPost, ok200)
import Network.Wai (Application, Request (requestMethod), mapRequestHeaders)
import Network.Wai.Test
  ( SRequest (SRequest, simpleRequest)
  , SResponse (simpleHeaders, simpleStatus)
  , defaultRequest
  , request
  , setPath
  , withSession
  )
import Servant.Client
import Test.Hspec.Hedgehog (hedgehog)

import Data.ByteString.Lazy.Internal qualified as Lazy
import Hedgehog.Gen qualified as Gen
import Network.HTTP.Client qualified as Client

import Server (CommentsAPI, HealthAPI, VotingAPI, app, mkEnv)
import Utils.Generators (genCommentKey, genKey, genNewComment, genSortBy, genText)

getTestResponse :: PropertyT IO SResponse
getTestResponse = do
  req <-
    forAll $
      Gen.choice
        [ genReq (Proxy @CommentsAPI)
        , genReq (Proxy @HealthAPI)
        , genReq (Proxy @VotingAPI)
        ]

  -- requestMethod req === methodPost

  testRequest req

alwaysCacheControlOnGetRequests :: SResponse -> PropertyT IO ()
alwaysCacheControlOnGetRequests response = do
  diff (hCacheControl, "no-cache") elem (simpleHeaders response)

neverRespondWithInternalError :: SResponse -> PropertyT IO ()
neverRespondWithInternalError response = do
  diff (statusCode (simpleStatus response)) (<) 400

testRequest :: Request -> PropertyT IO SResponse
testRequest req = do
  myApp <- liftIO api
  liftIO $ withSession myApp $ do
    request req

api :: IO Application
api = do
  env <- mkEnv
  pure $ app env

genReq apiProxy =
  genRequest apiProxy (genCommentKey :*: genSortBy :*: genText :*: genNewComment :*: GNil)
    <&> \makeReq -> toWaiRequest $ makeReq $ fromJust $ parseBaseUrl ""

toWaiRequest :: Client.Request -> Request
toWaiRequest creq =
  defaultRequest
    & flip setPath creqPath
    & mapRequestHeaders (mappend creqHeaders)
 where
  creqPath = Client.path creq
  creqHeaders = Client.requestHeaders creq
