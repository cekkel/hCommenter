module Middleware.Headers (Enriched, enrichApiWithHeaders, correlationIDHeaderName) where

import Effectful (Eff)
import Effectful qualified as E
import Logging.LogContext (LogField (CorrelationID))
import Logging.LogEffect (Log, addLogContext)
import Servant

correlationIDHeaderName :: (IsString a) => a
correlationIDHeaderName = "CorrelationID"

type Enriched api = Header "CorrelationID" Text :> api

enrichApiWithHeaders ::
  forall api es.
  (HasServer api '[], Log E.:> es) =>
  Proxy api ->
  ServerT api (Eff es) ->
  ServerT (Enriched api) (Eff es)
enrichApiWithHeaders api server mCorrelationId =
  hoistServer
    api
    -- Not sure why, but ghc needs help here to work out the 'es' type.
    (addLogContext @es [CorrelationID correlationId])
    server
  where
    correlationId = fromMaybe "No CorrelationID" mCorrelationId
