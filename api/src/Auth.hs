{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Auth
  ( UserAuth (..)
  , authenticated
  )
where

import Data.Aeson
import Effectful (Eff)
import Servant (HasServer (ServerT, hoistServerWithContext), type (:>))
import Servant.Auth.Server as SAS
import Servant.Server.Experimental.Auth (AuthServerData)

import Effectful qualified as Eff
import Effectful.Error.Static qualified as ES
import Effectful.Reader.Static qualified as RS

import RestAPI.ServerTypes (ApiContexts, InputError (AuthError))
import Utils.RequestContext (RequestContext (authUsername))

newtype UserAuth = UserAuth
  { username :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance ToJSON UserAuth

instance FromJSON UserAuth

instance ToJWT UserAuth

instance FromJWT UserAuth

-- | This is the user type that will be returned when a user is authenticated.
type instance AuthServerData (Auth auths UserAuth) = UserAuth

-- TODO: Could replace the request context with an enhanced context that includes the user information
authenticated
  :: forall api es auths
   . ( ES.Error InputError Eff.:> es
     , HasServer api ApiContexts
     , RS.Reader RequestContext Eff.:> es
     )
  => ServerT api (Eff es)
  -> ServerT (Auth auths UserAuth :> api) (Eff es)
authenticated api authInfo =
  hoistServerWithContext
    (Proxy @api)
    (Proxy @ApiContexts)
    (handleAuth authInfo)
    api
 where
  handleAuth :: AuthResult UserAuth -> Eff es a -> Eff es a
  handleAuth = \case
    Authenticated (UserAuth username) -> RS.local (\ctx -> ctx {authUsername = Just username})
    _ -> const $ ES.throwError $ AuthError "Authentication failed"
