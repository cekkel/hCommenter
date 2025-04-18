module Server.Health (healthServer, HealthAPI) where

import Effectful.Error.Static (Error)
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
import Server.ServerTypes (InputError)

-- | Note that 'GetNoContent' cannot be used here, as it does not allow response headers.
type HealthAPI = "health" :> Description "Returns 200 if service is online" :> GetNoContent

healthServer
  :: ( Error InputError E.:> es
     , Log E.:> es
     )
  => ServerT HealthAPI (E.Eff es)
healthServer = do
  logInfo "Health checked"
  pure NoContent
