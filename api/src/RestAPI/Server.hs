{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RestAPI.Server
  ( initDevSqliteDB
  , app
  , serverAPI
  , readEnv
  , API
  , FunctionalAPI
  , HealthAPI
  , CommentsAPI
  , VotingAPI
  , messageConsoleAndRun
  )
where

import Data.OpenApi (OpenApi)
import Effectful (Eff)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setOnException, setPort)
import Optics
import Servant
  ( Application
  , Context (EmptyContext, (:.))
  , HasServer (hoistServerWithContext)
  , Server
  , serveWithContext
  , type (:<|>) (..)
  , type (:>)
  )
import Servant.Auth.Server (Auth)
import Servant.OpenApi (HasOpenApi (toOpenApi))

import Auth (UserAuth, authenticated)
import Database.Mockserver (initDevSqliteDB)
import Middleware.Combined (addCustomMiddleware)
import Middleware.Exceptions (logOnException)
import Middleware.Headers (Enriched, enrichApiWithHeaders)
import Middleware.ServantErrorFormatters (customFormatters)
import RestAPI.EffectInjection (effToHandler)
import RestAPI.Endpoints.Auth (AuthAPI, AuthTypes, authServer)
import RestAPI.Endpoints.Comment (CommentsAPI, commentServer)
import RestAPI.Endpoints.Health (HealthAPI, healthServer)
import RestAPI.Endpoints.Swagger (SwaggerAPI, withMetadata)
import RestAPI.Endpoints.Voting (VotingAPI, votingServer)
import RestAPI.ServerTypes (ApiContexts)
import Utils.Environment (Env, readEnv)
import Utils.RequestContext (RequestContext)

type ProtectedAPI = CommentsAPI :<|> VotingAPI

type FunctionalAPI = HealthAPI :<|> AuthAPI :<|> (Auth AuthTypes UserAuth :> ProtectedAPI)

type API = SwaggerAPI :<|> FunctionalAPI

enrichedAPI :: Proxy (Enriched API)
enrichedAPI = Proxy

functionalAPI :: Proxy FunctionalAPI
functionalAPI = Proxy

swaggerServer :: Eff es OpenApi
swaggerServer = pure $ withMetadata $ toOpenApi functionalAPI

serverAPI :: (HasServer API ApiContexts) => RequestContext -> Server (Enriched API)
serverAPI ctx = do
  hoistServerWithContext enrichedAPI (Proxy @ApiContexts) (effToHandler ctx) $
    enrichApiWithHeaders @API $
      swaggerServer
        :<|> ( healthServer
                 :<|> authServer
                 :<|> authenticated @ProtectedAPI (commentServer :<|> votingServer)
             )

app :: Env -> Application
app env =
  let
    -- context is needed to be able to provide custom error formatters for servant,
    -- as well as cookie and JWT settings for authentication.
    apiContext :: Context ApiContexts
    apiContext =
      customFormatters
        :. (env ^. #cookieSettings)
        :. (env ^. #jwtSettings)
        :. EmptyContext
  in
    addCustomMiddleware env $ \ctx ->
      serveWithContext enrichedAPI apiContext $
        serverAPI ctx

messageConsoleAndRun :: IO ()
messageConsoleAndRun = do
  env <- readEnv

  let
    settings =
      defaultSettings
        & setPort (env ^. #port)
        & setOnException (logOnException env)

  initDevSqliteDB env
  putStrLn [fmt|\nListening in {env ^. #backend} mode, on port {env ^. #port}...\n|]
  runSettings settings $ app env
