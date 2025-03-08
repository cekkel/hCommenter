{-# LANGUAGE QuasiQuotes #-}

module Middleware.Requests where

import Data.UUID (toASCIIBytes)
import Data.UUID.V4 (nextRandom)
import Effectful (runEff)
import Network.HTTP.Types (Status (statusMessage), status300)
import Network.Wai
  ( Request (rawPathInfo, requestHeaders, requestMethod)
  , Response
  , mapRequestHeaders
  , responseStatus
  )
import PyF (fmt)

import Logging.LogContext (LogField (CorrelationID, StatusCode))
import Logging.LogEffect (addLogContext, logError, logInfo, logWarn, runLog)
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

-- TODO: use more efficient logging here.
logRequest :: Env -> Text -> Request -> IO ()
logRequest env correlationId req = do
  runEff . runLog env . addLogContext [CorrelationID correlationId] $
    logInfo
      [fmt|{requestMethod req} {rawPathInfo req}|]

logResponse :: Env -> Text -> Response -> IO Response
logResponse env correlationId response = do
  runEff . runLog env . addLogContext [CorrelationID correlationId, StatusCode status] $
    (if status < status300 then logInfo else logError)
      [fmt|Responded: {statusMessage status}|]

  pure response
 where
  status = responseStatus response
