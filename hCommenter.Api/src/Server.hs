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
import Logging.LogEffect
  ( Log
  , addLogContext
  , getFileScribe
  , logError
  , logExceptions
  , runLog
  )
import Middleware.Combined (addCustomMiddleware)
import Middleware.Headers (Enriched, enrichApiWithHeaders)
import Server.Comment (CommentsAPI, commentServer)
import Server.Health (HealthAPI, healthServer)
import Server.ServerTypes (Backend (..), CustomError (..), ErrorResponse (ErrorResponse), InputError (..))
import Server.Swagger (SwaggerAPI, withMetadata)
import Server.Voting (VotingAPI, votingServer)
import Utils.AppContext (AppContext (env), mkAppContext)
import Utils.Environment (Env (backend), readEnv)

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
        . logExceptions
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

messageConsoleAndRun :: Int -> Backend -> IO ()
messageConsoleAndRun port requestedBackend = do
  env <- readEnv

  case requestedBackend of
    SQLite -> initDevSqliteDB requestedBackend env
    LocalFile -> pure ()
    ToBeDeterminedProd -> pure ()

  putStrLn $ "\nListening in " <> tshow requestedBackend <> " mode, on port " <> tshow port <> "...\n"
  run port $ app env
