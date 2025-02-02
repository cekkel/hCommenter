{-# LANGUAGE TemplateHaskell #-}

module ApiProperties where

import Data.Data (Proxy (Proxy))
import Data.Function ((&))
import Data.Maybe (fromJust)
import Hedgehog
import Hedgehog.Servant
import Network.HTTP.Types (hCacheControl, ok200)
import Network.Wai (Application, Request, mapRequestHeaders)
import Network.Wai.Test (SRequest (SRequest), SResponse (simpleHeaders, simpleStatus), defaultRequest, setPath, srequest, withSession)
import Servant.Client
import Test.Hspec.Hedgehog (hedgehog)

import Data.ByteString.Lazy.Internal qualified as Lazy
import Hedgehog.Gen qualified as Gen
import Network.HTTP.Client qualified as Client

import Server (HealthAPI, app, mkEnv)

alwaysCacheControlOnGetRequests :: PropertyT IO ()
alwaysCacheControlOnGetRequests = do
  request <- forAll $ genReq "localhost:8080/health"

  response <- testRequest request (Just "")
  diff (hCacheControl, "no-cache") elem (simpleHeaders response)

-- simpleStatus response === ok200

testRequest :: Request -> Maybe Lazy.ByteString -> PropertyT IO SResponse
testRequest req body = do
  myApp <- liftIO api
  liftIO $ withSession myApp $ do
    srequest (SRequest req (fromMaybe "" body))

api :: IO Application
api = do
  env <- mkEnv
  pure $ app env

genReq :: String -> Gen Request
genReq url =
  genRequest (Proxy @HealthAPI) (GNil)
    <&> \makeReq ->
      toWaiRequest $
        makeReq $
          fromJust $
            parseBaseUrl url

toWaiRequest :: Client.Request -> Request
toWaiRequest creq =
  defaultRequest
    & flip setPath creqPath
    & mapRequestHeaders (mappend creqHeaders)
 where
  creqPath = Client.path creq
  creqHeaders = Client.requestHeaders creq
