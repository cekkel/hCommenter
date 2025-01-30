{-# LANGUAGE UndecidableInstances #-}

module Server (initDevSqliteDB, app, Backend (..), Env (Env), getConsoleScribe, fullAPI, functionalAPI, serverAPI) where

import Control.Monad.Trans.Except (except)
import Data.Aeson qualified as JSON
import Data.Bifoldable (bitraverse_)
import Data.Either.Extra (mapLeft)
import Data.Swagger (Swagger)
import Database.Interface (CommentStorage)
import Database.Mockserver
  ( initDevSqliteDB
  )
import Database.SqlPool (SqlPool)
import Database.SqlStorage (runCommentStorageSQL)
import Database.StorageTypes
import Effectful (Eff, IOE, runEff)
import Effectful qualified as E
import Effectful.Error.Static
  ( CallStack
  , Error
  , prettyCallStack
  , runError
  )
import Katip (showLS)
import Logging.LogEffect
  ( Log
  , getConsoleScribe
  , logError
  , logExceptions
  , runLog
  )
import Middleware.Headers (Enriched, enrichApiWithHeaders)
import Middleware.Requests (addRequestLogging)
import Optics
import Servant
  ( Application
  , Handler (Handler)
  , Proxy (..)
  , Server
  , ServerError (errBody, errHTTPCode, errHeaders)
  , err400
  , err404
  , hoistServer
  , serve
  , type (:<|>) (..)
  )
import Servant.Swagger (HasSwagger (toSwagger))
import Server.Comment (CommentsAPI, commentServer)
import Server.Health (HealthAPI, healthServer)
import Server.ServerTypes (Backend (..), CustomError (..), Env (..), ErrorResponse (ErrorResponse), InputError (..), backend)
import Server.Swagger (SwaggerAPI, withMetadata)
import Server.Voting (VotingAPI, votingServer)
import Prelude hiding (Handler)

type FunctionalAPI = (HealthAPI :<|> CommentsAPI :<|> VotingAPI)

type API = SwaggerAPI :<|> Enriched FunctionalAPI

swaggerServer :: Eff es Swagger
swaggerServer = pure $ withMetadata $ toSwagger functionalAPI

serverAPI :: Env -> Server API
serverAPI env = do
  hoistServer fullAPI (effToHandler env) $
    swaggerServer :<|> enrichApiWithHeaders functionalAPI (healthServer :<|> commentServer :<|> votingServer)

functionalAPI :: Proxy FunctionalAPI
functionalAPI = Proxy

fullAPI :: Proxy API
fullAPI = Proxy

app :: Env -> Application
app env =
  addRequestLogging env $
    serve fullAPI $
      serverAPI env

effToHandler :: Env -> Eff [CommentStorage, SqlPool, Error InputError, Error StorageError, Log, IOE] a -> Handler a
effToHandler env m = do
  result <-
    liftIO
      $ runEff
        . runLog env
        . logExceptions
        . logExplicitErrors
        . runAndLiftError StorageError
        . runAndLiftError InputError
        . fmap Right
        . commentHandler
      $ m
  Handler $ except $ handleServerResponse result
 where
  commentHandler = case env ^. backend of
    LocalFile -> error "Mode not supported"
    sqlBackend -> runCommentStorageSQL sqlBackend

runAndLiftError
  :: (e -> CustomError)
  -> Eff (Error e : es) (Either (CallStack, CustomError) a)
  -> Eff es (Either (CallStack, CustomError) a)
runAndLiftError f = fmap (join . mapLeft (second f)) . runError

logExplicitErrors :: (Show e, Log E.:> es) => Eff es (Either (CallStack, e) a) -> Eff es (Either (CallStack, e) a)
logExplicitErrors currEff = do
  value <- currEff
  bitraverse_ handleLeft pure value
  pure value
 where
  handleLeft (callStack, err) = do
    logError $ "Custom error '" <> showLS err <> "' with callstack: " <> showLS (prettyCallStack callStack)

handleServerResponse :: Either (CallStack, CustomError) a -> Either ServerError a
handleServerResponse (Right val) = Right val
handleServerResponse (Left (_, err)) = case err of
  StorageError innerErr -> Left $ servantErrorWithText err404 $ case innerErr of
    CommentNotFound -> "Can't find the comment"
    UserNotFound -> "Can't find the user"
    ConvoNotFound -> "Can't find the conversation"
  InputError innerErr -> Left $ servantErrorWithText err400 $ case innerErr of
    BadArgument txt -> "Bad argument: " <> txt
 where
  servantErrorWithText sErr msg =
    sErr
      { errBody = JSON.encode $ ErrorResponse msg (errHTTPCode sErr)
      , errHeaders = [(fromString "Content-Type", "application/json;charset=utf-8")]
      }
