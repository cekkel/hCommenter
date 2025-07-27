{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

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
  )
import Servant.OpenApi (HasOpenApi (toOpenApi))

import Database.Mockserver (initDevSqliteDB)
import Middleware.Combined (addCustomMiddleware)
import Middleware.Exceptions (logOnException)
import Middleware.Headers (Enriched, enrichApiWithHeaders)
import Middleware.ServantErrorFormatters (customFormatters)
import RestAPI.EffectInjection (effToHandler)
import RestAPI.Endpoints.Auth (AuthAPI, authServer)
import RestAPI.Endpoints.Comment (CommentsAPI, commentServer)
import RestAPI.Endpoints.Health (HealthAPI, healthServer)
import RestAPI.Endpoints.Swagger (SwaggerAPI, withMetadata)
import RestAPI.Endpoints.Voting (VotingAPI, votingServer)
import RestAPI.ServerTypes (ApiContexts)
import Utils.Environment (Env, readEnv)
import Utils.RequestContext (RequestContext)

type FunctionalAPI = (HealthAPI :<|> AuthAPI :<|> CommentsAPI :<|> VotingAPI)

type API = SwaggerAPI :<|> FunctionalAPI

enrichedAPI :: Proxy (Enriched API)
enrichedAPI = Proxy

fullAPI :: Proxy API
fullAPI = Proxy

functionalAPI :: Proxy FunctionalAPI
functionalAPI = Proxy

swaggerServer :: Eff es OpenApi
swaggerServer = pure $ withMetadata $ toOpenApi functionalAPI

serverAPI :: RequestContext -> Server (Enriched API)
serverAPI ctx = do
  hoistServerWithContext enrichedAPI (Proxy @ApiContexts) (effToHandler ctx) $
    enrichApiWithHeaders fullAPI $
      swaggerServer :<|> (healthServer :<|> authServer :<|> commentServer :<|> votingServer)

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
