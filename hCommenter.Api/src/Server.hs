{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module Server
  ( initDevSqliteDB
  , app
  , Backend (..)
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

import Control.Monad.Trans.Except (except)
import Data.Bifoldable (bitraverse_)
import Data.Either.Extra (mapLeft)
import Data.Swagger (Swagger)
import Effectful (Eff, IOE, runEff)
import Effectful.Error.Static
  ( CallStack
  , Error
  , prettyCallStack
  , runError
  )
import Katip (showLS)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setOnException, setPort)
import Optics
import PyF (fmt)
import Servant
  ( Application
  , Handler (Handler)
  , Proxy (..)
  , Server
  , ServerError (errBody, errHTTPCode, errHeaders)
  , err400
  , err404
  , err500
  , hoistServer
  , serve
  , type (:<|>) (..)
  )
import Servant.Swagger (HasSwagger (toSwagger))
import Prelude hiding (Handler)

import Data.Aeson qualified as JSON
import Effectful qualified as E
import Effectful.Reader.Static qualified as ES

import Database.Interface (CommentStorage)
import Database.Mockserver
  ( initDevSqliteDB
  )
import Database.SqlPool (SqlPool)
import Database.SqlStorage (runCommentStorageSQL)
import Database.StorageTypes
import Logging.LogContext (LogField (CorrelationID))
import Logging.LogEffect
  ( Log
  , addLogContext
  , logError
  , runLog
  )
import Middleware.Combined (addCustomMiddleware)
import Middleware.Exceptions (logOnException)
import Middleware.Headers (Enriched, enrichApiWithHeaders)
import Server.Comment (CommentsAPI, commentServer)
import Server.Health (HealthAPI, healthServer)
import Server.ServerTypes (Backend (..), CustomError (..), ErrorResponse (ErrorResponse), InputError (..))
import Server.Swagger (SwaggerAPI, withMetadata)
import Server.Voting (VotingAPI, votingServer)
import Utils.AppContext (AppContext)
import Utils.Environment (Env, readEnv)

app :: Env -> Application
app env =
  addCustomMiddleware env $ \ctx ->
    serve enrichedAPI $
      serverAPI ctx

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

serverAPI :: AppContext -> Server (Enriched API)
serverAPI ctx = do
  hoistServer enrichedAPI (effToHandler ctx) $
    enrichApiWithHeaders fullAPI $
      swaggerServer :<|> (healthServer :<|> commentServer :<|> votingServer)

effToHandler
  :: AppContext
  -> Eff [CommentStorage, SqlPool, Error InputError, Error StorageError, Log, ES.Reader AppContext, IOE] a
  -> Handler a
effToHandler ctx m = do
  result <-
    liftIO
      $ runEff
        . ES.runReader ctx
        . runLog (ctx ^. #env)
        . addGlobalLogContext
        . logExplicitErrors
        . runAndLiftError StorageError
        . runAndLiftError InputError
        . fmap Right
        . commentHandler
      $ m
  Handler $ except $ handleServerResponse result
 where
  commentHandler = case ctx ^. #env % #backend of
    LocalFile -> error "Mode not supported"
    sqlBackend -> runCommentStorageSQL sqlBackend

runAndLiftError
  :: (e -> CustomError)
  -> Eff (Error e : es) (Either (CallStack, CustomError) a)
  -> Eff es (Either (CallStack, CustomError) a)
runAndLiftError f = fmap (join . mapLeft (second f)) . runError

logExplicitErrors
  :: ( ES.Reader AppContext E.:> es
     , Log E.:> es
     , Show e
     )
  => Eff es (Either (CallStack, e) a)
  -> Eff es (Either (CallStack, e) a)
logExplicitErrors currEff = do
  value <- currEff
  bitraverse_ handleLeft pure value
  pure value
 where
  handleLeft (callStack, err) =
    logError [fmt|Custom error '{showLS err}' with callstack: {prettyCallStack callStack}|]

addGlobalLogContext
  :: ( ES.Reader AppContext E.:> es
     , Log E.:> es
     )
  => Eff es a
  -> Eff es a
addGlobalLogContext eff = do
  corrId <- ES.asks $ view #correlationId
  addLogContext [CorrelationID corrId] eff

handleServerResponse :: Either (CallStack, CustomError) a -> Either ServerError a
handleServerResponse (Right val) = Right val
handleServerResponse (Left (_, err)) = case err of
  StorageError innerErr -> Left $ case innerErr of
    CommentNotFound -> servantErrorWithText err404 "Can't find the comment"
    UserNotFound -> servantErrorWithText err404 "Can't find the user"
    ConvoNotFound -> servantErrorWithText err404 "Can't find the conversation"
    UnhandledStorageError _ -> servantErrorWithText err500 "An unhandled storage exception occurred. Sorry!"
  InputError innerErr -> Left $ servantErrorWithText err400 $ case innerErr of
    BadArgument txt -> [fmt|Bad argument: {txt}|]
 where
  servantErrorWithText sErr msg =
    sErr
      { errBody = JSON.encode $ ErrorResponse msg (errHTTPCode sErr)
      , errHeaders = [(fromString "Content-Type", "application/json;charset=utf-8")]
      }

messageConsoleAndRun :: IO ()
messageConsoleAndRun = do
  env <- readEnv

  case env ^. #backend of
    SQLite -> initDevSqliteDB env
    _ -> pure ()

  let
    settings =
      defaultSettings
        & setPort (env ^. #port)
        & setOnException (logOnException env)

  putStrLn [fmt|\nListening in {env ^. #backend} mode, on port {env ^. #port}...\n|]
  runSettings settings $ app env
