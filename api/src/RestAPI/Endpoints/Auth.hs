{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module RestAPI.Endpoints.Auth (AuthAPI, authServer) where

import Database.Persist.Sql (Key, PersistValue (PersistText), keyToValues)
import Optics ((^.))
import Servant
import Servant.Auth.Server (Auth, Cookie, JWT, SetCookie)

import Effectful qualified as Eff

import Database.Users.Interface
import Logging.LogEffect (Log)
import Logging.Utilities (logInfo)
import Mapping.ExternalTypes (ViewUser)

import Auth qualified
import Database.Schema qualified as Schema

type AuthTypes = '[JWT, Cookie]

type ProtectedRoutes = Auth AuthTypes Auth.User

type AuthAPI =
  "auth"
    :> ( "register" :> ReqBody '[JSON] Schema.NewUser :> PostCreated '[JSON] Text
           :<|> "login"
             :> ReqBody '[JSON] Schema.NewUser
             :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
           :<|> "me" :> ProtectedRoutes :> Get '[JSON] ViewUser
       )

authServer :: (Log Eff.:> es, UserStorage Eff.:> es) => ServerT AuthAPI (Eff.Eff es)
authServer = register :<|> login :<|> me
 where
  register newUser = do
    logInfo [fmt|Registering new user: {newUser ^. #username}|]
    getUsername <$> insertUser newUser

  login :: Schema.NewUser -> Eff.Eff es (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
  login _ = error "login should be handled by servant-auth"

  me = error "Not yet implemented"

getUsername :: Key Schema.User -> Text
getUsername key = case keyToValues key of
  [PersistText username] -> username
  -- TODO: Handle error here properly?
  _ -> error "Primary key is not as expected for User table"
