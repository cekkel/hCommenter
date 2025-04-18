module Middleware.Headers (Enriched, enrichApiWithHeaders, correlationIDHeaderName, addGlobalHeadersToResponse) where

import Effectful (Eff)
import Network.HTTP.Types (hCacheControl)
import Network.Wai (Response, mapResponseHeaders)
import Servant

import Effectful qualified as E

import Logging.LogContext (LogField (CorrelationID))
import Logging.LogEffect (Log)
import Logging.Utilities (addLogContext)

correlationIDHeaderName :: (IsString a) => a
correlationIDHeaderName = "Correlation-Id"

type Enriched api = Header "Correlation-Id" Text :> api

enrichApiWithHeaders
  :: forall api es
   . (HasServer api '[], Log E.:> es)
  => Proxy api
  -> ServerT api (Eff es)
  -> ServerT (Enriched api) (Eff es)
enrichApiWithHeaders api server mCorrelationId =
  hoistServer
    api
    addCorrelationId
    server
 where
  -- Note that the correlationID should ALWAYS be present, as it gets added in Middleware.Requests
  correlationId = fromMaybe "MISSING" mCorrelationId

  addCorrelationId :: Eff es a -> Eff es a
  addCorrelationId eff = addLogContext [CorrelationID correlationId] eff

addGlobalHeadersToResponse :: ByteString -> Response -> Response
addGlobalHeadersToResponse = \case
  "GET" -> mapResponseHeaders addGetHeaders
  _ -> id
 where
  addGetHeaders =
    mappend
      [ (hCacheControl, "no-cache")
      ]
