{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module ApiProperties where

import Data.Data (Proxy (Proxy))
import Data.Function ((&))
import Data.Maybe (fromJust)
import Hedgehog
import Hedgehog.Servant
import Network.HTTP.Types (Status (statusCode), hCacheControl, methodGet, methodPost, ok200)
import Network.Wai (Application, Request (requestHeaders, requestMethod), mapRequestHeaders)
import Network.Wai.Test
  ( SRequest (SRequest, simpleRequest)
  , SResponse (simpleHeaders, simpleStatus)
  , defaultRequest
  , request
  , setPath
  , withSession
  )
import Servant.API
import Servant.API.Verbs
import Servant.Client
import Test.Hspec.Hedgehog (hedgehog)

import Data.ByteString.Lazy.Internal qualified as Lazy
import Hedgehog.Gen qualified as Gen
import Network.HTTP.Client qualified as Client

import Server (FunctionalAPI, app, mkEnv)
import Utils.Generators (genCommentKey, genKey, genNewComment, genSortBy, genText)

requireApiBestPracticesFor apiProxy makeReq = hedgehog $ do
  req <- forAll $ genReq apiProxy
  response <- liftIO (makeReq req)

  alwaysCacheControlOnGetRequests req response
  neverRespondWithInternalError response

alwaysCacheControlOnGetRequests :: Request -> SResponse -> PropertyT IO ()
alwaysCacheControlOnGetRequests req response = do
  when (requestMethod req == methodGet) $ do
    diff (hCacheControl, "no-cache") elem (simpleHeaders response)

neverRespondWithInternalError :: SResponse -> PropertyT IO ()
neverRespondWithInternalError response = do
  diff (statusCode (simpleStatus response)) (<) 500

genReq apiProxy =
  genRequest apiProxy (genCommentKey :*: genSortBy :*: genText :*: genNewComment :*: GNil)
    <&> \makeReq -> toWaiRequest $ makeReq $ fromJust $ parseBaseUrl ""

toWaiRequest :: Client.Request -> Request
toWaiRequest creq =
  defaultRequest {requestMethod = creqMethod, requestHeaders = creqHeaders}
    & flip setPath creqPath
 where
  creqMethod = Client.method creq
  creqPath = Client.path creq
  creqHeaders = Client.requestHeaders creq
