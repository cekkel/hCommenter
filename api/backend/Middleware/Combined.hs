module Middleware.Combined (addCustomMiddleware) where

import Network.Wai (Application, Request (requestMethod))

import Middleware.Headers (addGlobalHeadersToResponse)
import Middleware.Requests (addCorrelationIdIfMissing, logRequest, logResponse)
import Utils.Environment (Env)
import Utils.RequestContext (RequestContext, mkRequestContext)

addCustomMiddleware :: Env -> (RequestContext -> Application) -> Application
addCustomMiddleware env baseAppClosure req responseF = do
  let
    method = requestMethod req

  (updatedReq, correlationId) <- addCorrelationIdIfMissing env req

  let
    requestContext = mkRequestContext env correlationId
    baseApp = baseAppClosure requestContext

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
