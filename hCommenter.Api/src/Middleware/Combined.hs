module Middleware.Combined (addCustomMiddleware) where

import Network.Wai (Application, Request (requestMethod))

import Middleware.Headers (addGlobalHeadersToResponse)
import Middleware.Requests (getCorrelationId, logRequest, logResponse)
import Utils.Environment (Env)

addCustomMiddleware :: Env -> Application -> Application
addCustomMiddleware env baseApp req responseF = do
  let
    correlationId = getCorrelationId req
    method = requestMethod req

  logRequest env correlationId req
  liftIO $
    baseApp
      req
      ( (pure . addGlobalHeadersToResponse method)
          >=> logResponse env correlationId
          >=> responseF
      )
