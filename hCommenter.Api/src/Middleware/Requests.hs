module Middleware.Requests where

import Effectful (runEff)
import Katip (logStr)
import Logging.LogContext (LogField (CorrelationID))
import Logging.LogEffect (addLogContext, logInfo, runLog)
import Middleware.Headers (correlationIDHeaderName)
import Network.HTTP.Types (Status (statusMessage))
import Network.Wai
  ( Request (rawPathInfo, requestHeaders, requestMethod)
  , Response
  , responseStatus
  )
import Servant (Application)
import Server.ServerTypes (Env)

addRequestLogging :: Env -> Application -> Application
addRequestLogging env baseApp req responseF = do
  let
    headers = requestHeaders req
    maybeCorrelationId = decodeUtf8 . snd <$> find (\(name, _) -> name == correlationIDHeaderName) headers
    correlationId = fromMaybe "No CorrelationID" maybeCorrelationId

  logRequest env correlationId req
  liftIO $ baseApp req (responseF <=< logResponse env correlationId)

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
