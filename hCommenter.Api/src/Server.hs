{-# LANGUAGE UndecidableInstances #-}

module Server (initialiseLocalFile, initDevSqliteDB, app, Backend (..), Env (Env), getConsoleScribe) where

import           ClassyPrelude              hiding (Handler)
import           Control.Lens               ((^.))
import           Control.Monad.Trans.Except (except)
import qualified Data.Aeson                 as JSON
import           Data.Bifoldable            (bitraverse_)
import           Data.Either.Extra          (mapLeft)
import           Data.Swagger               (Swagger)
import           Database.Interface         (CommentStorage)
import           Database.LocalStorage      (runCommentStorageIO)
import           Database.Mockserver        (fileName, initDevSqliteDB,
                                             initialiseLocalFile)
import           Database.SqlPool           (SqlPool)
import           Database.SqlStorage        (runCommentStorageSQL)
import           Database.StorageTypes
import           Effectful                  (Eff, IOE, runEff, (:>))
import           Effectful.Error.Static     (CallStack, Error, prettyCallStack,
                                             runError)
import           Katip                      (showLS)
import           Logging                    (Log, getConsoleScribe, logError,
                                             logExceptions, runLog)
import           Middleware.Requests        (addRequestLogging)
import           Servant                    (Application, Handler (Handler),
                                             Proxy (..), Server,
                                             ServerError (errBody, errHTTPCode, errHeaders),
                                             err400, err404, hoistServer, serve,
                                             type (:<|>) (..))
import           Servant.Swagger            (HasSwagger (toSwagger))
import           Server.Comment             (CommentsAPI, commentServer)
import           Server.ServerTypes
import           Server.Swagger             (SwaggerAPI, withMetadata)
import           Server.Voting              (VotingAPI, votingServer)

type FunctionalAPI = CommentsAPI :<|> VotingAPI
type API = SwaggerAPI :<|> FunctionalAPI

swaggerServer :: Eff es Swagger
swaggerServer = pure $ withMetadata $ toSwagger functionalAPI

serverAPI :: Env -> Server API
serverAPI env = do
  hoistServer fullAPI (effToHandler env) $
    swaggerServer :<|> commentServer :<|> votingServer

functionalAPI :: Proxy FunctionalAPI
functionalAPI = Proxy

fullAPI :: Proxy API
fullAPI = Proxy

app :: Env -> Application
app env =
  addRequestLogging env
    $ serve fullAPI
    $ serverAPI env

effToHandler :: Env -> Eff [CommentStorage, SqlPool, Error InputError, Error StorageError, Log, IOE] a -> Handler a
effToHandler env m = do
  result <- liftIO $ runEff
            . runLog env
            . logExceptions
            . logExplicitErrors
            . runAndLiftError StorageError
            . runAndLiftError InputError . fmap Right
            . commentHandler
            $ m
  Handler $ except $ handleServerResponse result

  where
    commentHandler = case env ^. backend of
      LocalFile  -> runCommentStorageIO fileName
      sqlBackend -> runCommentStorageSQL sqlBackend

runAndLiftError :: (e -> CustomError) -> Eff (Error e : es) (Either (CallStack, CustomError) a) -> Eff es (Either (CallStack, CustomError) a)
runAndLiftError f = fmap (join . mapLeft (second f)) . runError

logExplicitErrors :: (Show e, Log :> es) => Eff es (Either (CallStack, e) a) -> Eff es (Either (CallStack, e) a)
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
    UserNotFound    -> "Can't find the user"
    ConvoNotFound   -> "Can't find the conversation"
  InputError innerErr -> Left $ servantErrorWithText err400 $ case innerErr of
    BadArgument txt -> "Bad argument: " <> txt

  where
    servantErrorWithText sErr msg =
      sErr
        { errBody = JSON.encode $ ErrorResponse msg (errHTTPCode sErr),
          errHeaders = [(fromString "Content-Type", "application/json;charset=utf-8")]
        }
