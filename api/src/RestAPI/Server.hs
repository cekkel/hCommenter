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

import Data.Swagger (Swagger)
import Effectful (Eff)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setOnException, setPort)
import Optics
import PyF (fmt)
import Servant (Application, Context (EmptyContext, (:.)), Proxy (..), Server, hoistServer, serveWithContext, type (:<|>) (..))
import Servant.Swagger (HasSwagger (toSwagger))
import Prelude hiding (Handler)

import Database.Mockserver (initDevSqliteDB)
import Middleware.Combined (addCustomMiddleware)
import Middleware.Exceptions (logOnException)
import Middleware.Headers (Enriched, enrichApiWithHeaders)
import Middleware.ServantErrorFormatters (customFormatters)
import RestAPI.EffectInjection (effToHandler)
import RestAPI.Endpoints.Comment (CommentsAPI, commentServer)
import RestAPI.Endpoints.Health (HealthAPI, healthServer)
import RestAPI.Endpoints.Swagger (SwaggerAPI, withMetadata)
import RestAPI.Endpoints.Voting (VotingAPI, votingServer)
import Utils.Environment (Env, readEnv)
import Utils.RequestContext (RequestContext)

type FunctionalAPI = (HealthAPI :<|> CommentsAPI :<|> VotingAPI)

type API = SwaggerAPI :<|> FunctionalAPI

enrichedAPI :: Proxy (Enriched API)
enrichedAPI = Proxy

fullAPI :: Proxy API
fullAPI = Proxy

functionalAPI :: Proxy FunctionalAPI
functionalAPI = Proxy

swaggerServer :: Eff es Swagger
swaggerServer = pure $ withMetadata $ toSwagger functionalAPI

serverAPI :: RequestContext -> Server (Enriched API)
serverAPI ctx = do
  hoistServer enrichedAPI (effToHandler ctx) $
    enrichApiWithHeaders fullAPI $
      swaggerServer :<|> (healthServer :<|> commentServer :<|> votingServer)

app :: Env -> Application
app env =
  addCustomMiddleware env $ \ctx ->
    -- context is needed to be able to provide custom error formatters for servant.
    serveWithContext enrichedAPI (customFormatters :. EmptyContext) $
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
