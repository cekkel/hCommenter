{-# LANGUAGE UndecidableInstances #-}

module Server
  ( initDevSqliteDB
  , app
  , Backend (..)
  , Env (Env)
  , getConsoleScribe
  , serverAPI
  , mkEnv
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
import Katip (Verbosity (V3), showLS)
import Network.Wai.Handler.Warp (run)
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
import System.Log.Raven (initRaven, stderrFallback)
import System.Log.Raven.Transport.HttpConduit (sendRecord)
import Prelude hiding (Handler)

import Data.Aeson qualified as JSON
import Effectful qualified as E

import Database.Interface (CommentStorage)
import Database.Mockserver
  ( initDevSqliteDB
  )
import Database.SqlPool (SqlPool)
import Database.SqlStorage (runCommentStorageSQL)
import Database.StorageTypes
import Logging.LogEffect
  ( Log
  , addLogContext
  , getConsoleScribe
  , getFileScribe
  , logError
  , logExceptions
  , runLog
  )
import Logging.Raven (mkRavenScribe)
import Middleware.Combined (addCustomMiddleware)
import Middleware.Headers (Enriched, enrichApiWithHeaders)
import Server.Comment (CommentsAPI, commentServer)
import Server.Health (HealthAPI, healthServer)
import Server.ServerTypes (Backend (..), CustomError (..), Env (..), ErrorResponse (ErrorResponse), InputError (..), backend)
import Server.Swagger (SwaggerAPI, withMetadata)
import Server.Voting (VotingAPI, votingServer)
import Utils.Environment (getAppEnv, getSentryDSN)

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
  addCustomMiddleware env $
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

logExplicitErrors
  :: (Log E.:> es, Show e)
  => Eff es (Either (CallStack, e) a)
  -> Eff es (Either (CallStack, e) a)
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

mkEnv :: IO Env
mkEnv = do
  -- scribe <- getConsoleScribe
  appEnv <- getAppEnv
  sentryDSN <- getSentryDSN
  sentryService <- initRaven sentryDSN id sendRecord stderrFallback

  ravenScribe <- mkRavenScribe sentryService (const $ pure True) V3

  pure $ Env SQLite "hCommenter.Api" appEnv "Raven" ravenScribe

messageConsoleAndRun :: Int -> Backend -> IO ()
messageConsoleAndRun port requestedBackend = do
  env <- mkEnv

  case requestedBackend of
    SQLite -> initDevSqliteDB requestedBackend env
    LocalFile -> pure ()
    ToBeDeterminedProd -> pure ()

  putStrLn $ "\nListening in " <> tshow requestedBackend <> " mode, on port " <> tshow port <> "...\n"
  run port $ app env
