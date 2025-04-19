{-# LANGUAGE QuasiQuotes #-}

module Middleware.Requests where

import Data.ByteString.Builder (toLazyByteString)
import Data.UUID (toASCIIBytes)
import Data.UUID.V4 (nextRandom)
import Effectful (runEff)
import Network.HTTP.Types (Status (statusMessage), statusIsSuccessful)
import Network.Wai
  ( Request (rawPathInfo, requestHeaders, requestMethod)
  , Response
  , mapRequestHeaders
  , responseStatus
  , responseToStream
  )
import PyF (fmt)

import Logging.LogContext (LogField (CorrelationID, RequestMethod, RequestPath, ResponseBody, StatusCode))
import Logging.LogEffect (runLog)
import Logging.Utilities (addLogContext, logError, logInfo, logWarn)
import Middleware.Headers (correlationIDHeaderName)
import Utils.Environment (Env)

getCorrelationId :: Request -> Maybe Text
getCorrelationId req = do
  let
    headers = requestHeaders req

  decodeUtf8 . snd <$> find (\(name, _) -> name == correlationIDHeaderName) headers

addCorrelationIdIfMissing :: Env -> Request -> IO (Request, Text)
addCorrelationIdIfMissing env req = do
  case getCorrelationId req of
    Just correlationId -> pure (req, correlationId)
    Nothing -> do
      uuid <- nextRandom

      let
        correlationId = tshow uuid

      runEff . runLog env . addLogContext [CorrelationID correlationId] $
        logWarn [fmt|Request is missing 'Correlation-Id' header, generating a uuidv4 at random: {correlationId}|]

      pure (mapRequestHeaders ((correlationIDHeaderName, toASCIIBytes uuid) :) req, tshow uuid)

-- PERF: use more efficient logging here.
logRequest :: Env -> Text -> Request -> IO ()
logRequest env correlationId req = do
  runEff
    . runLog env
    . addLogContext
      [ CorrelationID correlationId
      , RequestMethod method
      , RequestPath path
      ]
    $ logInfo
      [fmt|STARTING REQUEST: {method} {path}|]
 where
  method = requestMethod req
  path = decodeUtf8 $ rawPathInfo req

logResponse :: Env -> Text -> Response -> IO Response
logResponse env correlationId response = do
  let
    isError = not $ statusIsSuccessful status

  content <- if isError then getResponseContent response else pure "Hidden"

  runEff
    . runLog env
    . addLogContext
      [ CorrelationID correlationId
      , StatusCode status
      , ResponseBody content
      ]
    $ do
      (if isError then logError else logInfo)
        [fmt|REQUEST COMPLETE: Responded with {statusMessage status}|]

  pure response
 where
  status = responseStatus response

getResponseContent :: Response -> IO Text
getResponseContent response =
  let
    (_, _, extractContentWith) = responseToStream response
  in
    extractContentWith $ \f -> do
      content <- newIORef mempty
      f (\chunk -> modifyIORef' content (<> chunk)) (return ())
      decodeUtf8 . toStrict . toLazyByteString <$> readIORef content
