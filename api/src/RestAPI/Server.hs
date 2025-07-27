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
import PyF (fmt)
import RestAPI.Endpoints.Auth (AuthAPI, authServer)
import Servant (Application, Context (EmptyContext, (:.)), Server, hoistServer, serveWithContext, type (:<|>) (..))
import Servant.OpenApi (HasOpenApi (toOpenApi))

import Auth (NewUser (..), User (..))
import Database.Authors.Interface (findAuthorByUsername)
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
  hoistServer enrichedAPI (effToHandler ctx) $
    enrichApiWithHeaders fullAPI $
      swaggerServer :<|> (healthServer :<|> authServer :<|> commentServer :<|> votingServer)

app :: Env -> Application
app env =
  let
    checkPassword creds = do
      result <- effToHandler (mempty & #authors .~ (env ^. #pool)) $ do
        mUser <- findAuthorByUsername (creds ^. #username)
        pure $
          ( \user ->
              ( user ^. #password
              , User (user ^. #username)
              )
          )
            <$> mUser
      case result of
        Left _ -> pure Nothing
        Right r -> pure r

    -- This is the context that will be passed to the server.
    -- It contains the JWT and cookie settings.
    authContext =
      checkPassword
        :. customFormatters
        :. (env ^. #cookieSettings)
        :. (env ^. #jwtSettings)
        :. EmptyContext
  in
    addCustomMiddleware env $ \ctx ->
      -- context is needed to be able to provide custom error formatters for servant.
      serveWithContext enrichedAPI authContext $
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
