{-# LANGUAGE QuasiQuotes #-}

module Middleware.Exceptions where

import Effectful (runEff)
import Network.Wai (Request)

import Logging.LogContext (LogField (CorrelationID))
import Logging.LogEffect (runLog)
import Logging.Utilities (addLogContext, logCritical)
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
      logCritical [fmt|Exception occurred: {tshow e}|]
