module Middleware.Combined (addCustomMiddleware) where

import Network.Wai (Application, Request (requestMethod), Response, ResponseReceived)

import Middleware.Headers (addGlobalHeadersToResponse)
import Middleware.Requests (getCorrelationId, logRequest, logResponse)
import Utils.AppContext (AppContext, mkAppContext)
import Utils.Environment (Env)

addCustomMiddleware :: Env -> (AppContext -> Application) -> Application
addCustomMiddleware env baseAppClosure req responseF = do
  let
    method = requestMethod req

  (updatedReq, correlationId) <- getCorrelationId req

  let
    appContext = mkAppContext env correlationId
    baseApp = baseAppClosure appContext

  -- Middleware to log incoming request
  logRequest env correlationId updatedReq

  liftIO $
    baseApp
      updatedReq
      ( (pure . addGlobalHeadersToResponse method)
          -- Middleware to log outgoing request
          >=> logResponse env correlationId
          >=> responseF
      )
