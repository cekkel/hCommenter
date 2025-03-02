module Middleware.Requests where

import Effectful (runEff)
import Katip (logStr)
import Network.HTTP.Types (Status (statusMessage))
import Network.Wai
  ( Request (rawPathInfo, requestHeaders, requestMethod)
  , Response
  , responseStatus
  )
import Servant (Application)

import Logging.LogContext (LogField (CorrelationID))
import Logging.LogEffect (addLogContext, logInfo, runLog)
import Middleware.Headers (correlationIDHeaderName)
import Utils.Environment (Env)

getCorrelationId :: Request -> Text
getCorrelationId req = fromMaybe "No CorrelationID" maybeCorrelationId
 where
  headers = requestHeaders req
  maybeCorrelationId = decodeUtf8 . snd <$> find (\(name, _) -> name == correlationIDHeaderName) headers

-- TODO: use more efficient logging here.
logRequest :: Env -> Text -> Request -> IO ()
logRequest env correlationId req = runEff . runLog env $
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
