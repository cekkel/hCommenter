{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RestAPI.Endpoints.Auth
  ( AuthAPI
  , AuthTypes
  , authServer
  , CookieSettings
  , JWTSettings
  )
where

-- import Data.OpenApi
--   ( ApiKeyLocation (..)
--   , ApiKeyParams (..)
--   , SecurityRequirement (SecurityRequirement)
--   , SecurityScheme (..)
--   , SecuritySchemeType (..)
--   )
-- import Data.OpenApi.Lens (components, security, securitySchemes)
import Database.Persist.Sql (Key, PersistValue (PersistText), keyToValues)
-- import Optics ((&), (.~), (^.))
import Optics ((^.))
import Servant
import Servant.Auth.Server (Auth, Cookie, CookieSettings, JWT, JWTSettings, SetCookie)
import Servant.OpenApi
import Utils.Auth (UserAuth)

import Effectful qualified as Eff

import Database.Users.Interface
import Logging.LogEffect (Log)
import Logging.Utilities (logInfo)
import Mapping.ExternalTypes (ViewUser)

import Database.Schema qualified as Schema

type AuthTypes = '[JWT, Cookie]

type ProtectedRoutes = Auth AuthTypes UserAuth

deriving instance HasOpenApi (ProtectedRoutes :> api)

-- toOpenApi _ =
--   toOpenApi (Proxy :: Proxy api)
--     & components
--     . securitySchemes
--     <>~ securityDefinitions
--     & security
--     .~ [jwtSecurityRequirement]
--  where
--   securityDefinitions =
--     mempty
--       & at "jwt"
--       ?~ jwtSecurityScheme
--   jwtSecurityScheme =
--     SecurityScheme
--       { _securitySchemeType = SecuritySchemeHttp $ HttpSchemeBearer "JWT"
--       , _securitySchemeDescription = Just "JWT-based authentication"
--       }
--   jwtSecurityRequirement = SecurityRequirement $ mempty & at "jwt" ?~ []

type AuthAPI =
  "auth"
    :> ( "register" :> ReqBody '[JSON] Schema.NewUser :> PostCreated '[JSON] Text
           :<|> "login"
             :> ReqBody '[JSON] Schema.NewUser
             :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] ())
           :<|> "me" :> ProtectedRoutes :> Get '[JSON] ViewUser
       )

authServer :: (Log Eff.:> es, UserStorage Eff.:> es) => ServerT AuthAPI (Eff.Eff es)
authServer = register :<|> login :<|> me
 where
  register newUser = do
    logInfo [fmt|Registering new user: {newUser ^. #username}|]
    getUsername <$> insertUser newUser

  login :: Schema.NewUser -> Eff.Eff es (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] ())
  login _ = error "login should be handled by servant-auth"

  me = error "Not yet implemented"

getUsername :: Key Schema.User -> Text
getUsername key = case keyToValues key of
  [PersistText username] -> username
  -- TODO: Handle error here properly?
  _ -> error "Primary key is not as expected for User table"
