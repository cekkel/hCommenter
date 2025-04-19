module RestAPI.Endpoints.Health (healthServer, HealthAPI) where

import Servant
  ( Description
  , GetNoContent
  , HasServer (ServerT)
  , NoContent (NoContent)
  , type (:>)
  )

import Effectful qualified as E

import Logging.LogEffect (Log)
import Logging.Utilities (logInfo)

-- | Note that 'GetNoContent' cannot be used here, as it does not allow response headers.
type HealthAPI = "health" :> Description "Returns 200 if service is online" :> GetNoContent

healthServer
  :: (Log E.:> es)
  => ServerT HealthAPI (E.Eff es)
healthServer = do
  logInfo "Health checked"
  pure NoContent
