{-# LANGUAGE QuasiQuotes #-}

module Middleware.Exceptions where

import Effectful (runEff)
import Katip (showLS)
import Network.Wai (Request)
import PyF (fmt)
import Prelude hiding (Handler)

import Logging.LogContext (LogField (CorrelationID))
import Logging.LogEffect
  ( addLogContext
  , logError
  , runLog
  )
import Middleware.Requests (getCorrelationId)
import Utils.Environment (Env)

logOnException :: Env -> (Maybe Request -> SomeException -> IO ())
logOnException env mReq e = do
  let
    correlationId = getCorrelationId =<< mReq

  runEff . runLog env $
    -- Note: generated correlationId may not be here yet, since this function is executed
    -- by warp before servant gets involved.
    addLogContext [CorrelationID $ fromMaybe "No provided Correlation-Id" correlationId] $
      logError [fmt|Exception occurred: {showLS e}|]
