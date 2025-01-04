module Middleware.Requests where

import ClassyPrelude
import Effectful (runEff)
import Katip (logStr)
import Logging (logInfo, runLog)
import Network.HTTP.Types (Status (statusMessage))
import Network.Wai
  ( Request (rawPathInfo, requestMethod)
  , Response
  , responseStatus
  )
import Servant (Application)
import Server.ServerTypes (Env)

addRequestLogging :: Env -> Application -> Application
addRequestLogging env baseApp req responseF = do
  logRequest env req
  liftIO $ baseApp req (responseF <=< logResponse env)

logRequest :: Env -> Request -> IO ()
logRequest env req = runEff . runLog env $ do
  logInfo . logStr . mconcat $
    [ requestMethod req
    , " "
    , rawPathInfo req
    ]

logResponse :: Env -> Response -> IO Response
logResponse env response = runEff $ runLog env $ do
  logInfo . logStr . mconcat $
    [ "Responded: "
    , (statusMessage $ responseStatus response)
    ]
  pure response
