module Server.Health (healthServer, HealthAPI) where

import ClassyPrelude
import Effectful qualified as E
import Effectful.Error.Static (Error)
import Logging (Log, logInfo)
import Servant
  ( Description
  , GetNoContent
  , HasServer (ServerT)
  , NoContent (NoContent)
  , type (:>)
  )
import Server.ServerTypes (InputError)

type HealthAPI = "health" :> Description "Returns 200 if service is online" :> GetNoContent

healthServer ::
  ( Log E.:> es
  , Error InputError E.:> es
  ) =>
  ServerT HealthAPI (E.Eff es)
healthServer = do
  logInfo "Health checked"
  pure NoContent
