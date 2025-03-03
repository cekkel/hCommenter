module Middleware.Requests where

import Data.UUID (toASCIIBytes)
import Data.UUID.V4 (nextRandom)
import Effectful (runEff)
import Katip (logStr)
import Network.HTTP.Types (Status (statusMessage))
import Network.Wai
  ( Request (rawPathInfo, requestHeaders, requestMethod)
  , Response
  , mapRequestHeaders
  , responseStatus
  )
import Servant (Application, ResponseHeader (Header))

import Logging.LogContext (LogField (CorrelationID))
import Logging.LogEffect (addLogContext, logInfo, runLog)
import Middleware.Headers (correlationIDHeaderName)
import Utils.Environment (Env)

getCorrelationId :: Request -> IO (Request, Text)
getCorrelationId req = do
  let
    headers = requestHeaders req
    maybeCorrelationId = decodeUtf8 . snd <$> find (\(name, _) -> name == correlationIDHeaderName) headers

  uuid <- nextRandom -- Due to laziness this only gets generated if the header isn't there.
  pure $ case maybeCorrelationId of
    Just cID -> (req, cID)
    Nothing -> (mapRequestHeaders ((correlationIDHeaderName, toASCIIBytes uuid) :) req, tshow uuid)

-- TODO: use more efficient logging here.
logRequest :: Env -> Text -> Request -> IO ()
logRequest env correlationId req = do
  runEff . runLog env $
    addLogContext [CorrelationID correlationId] $ do
      logInfo . logStr . mconcat $
        [ requestMethod req
        , " "
        , rawPathInfo req
        ]

logResponse :: Env -> Text -> Response -> IO Response
logResponse env correlationId response = runEff . runLog env $
  addLogContext [CorrelationID correlationId] $ do
    logInfo . logStr . mconcat $
      [ "Responded: "
      , (statusMessage $ responseStatus response)
      ]
    pure response
