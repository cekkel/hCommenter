{-# LANGUAGE UndecidableInstances #-}

module Server (swaggerDefinition, initialiseLocalFile, app, Backend (..), Env (Env), getConsoleScribe) where

import           ClassyPrelude              hiding (Handler)
import           Control.Lens               (makeLenses, (&), (.~), (^.))
import           Control.Monad.Trans.Except (except)
import qualified Data.Aeson                 as JSON
import           Data.Aeson.Encode.Pretty   (encodePretty)
import           Data.Bifoldable            (bitraverse_)
import           Data.Binary                (encodeFile)
import qualified Data.ByteString.Lazy.Char8 as BS8 (ByteString)
import           Data.Either.Extra          (mapLeft)
import           Data.Swagger               (HasInfo (info), HasTitle (title))
import           Database.Interface         (CommentStorage)
import           Database.LocalStorage      (runCommentStorageIO)
import           Database.Mockserver        (mockComments)
import           Database.SqlPool           (SqlPool, runSqlPool)
import           Database.StorageTypes
import           Effectful                  (Eff, IOE, runEff, (:>))
import           Effectful.Error.Static     (CallStack, Error, prettyCallStack,
                                             runError)
import           Katip                      (Scribe, Verbosity (V0), showLS)
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
import           Server.Voting              (VotingAPI, votingServer)
import           System.Directory.Extra     (doesFileExist)

type API = CommentsAPI :<|> VotingAPI

swaggerDefinition :: BS8.ByteString
swaggerDefinition =
  encodePretty $ toSwagger (Proxy :: Proxy API)
    & info.title .~ "hCommenter API"

serverAPI :: Env -> Server API
serverAPI env = do
  hoistServer fullAPI (effToHandler env) $
    commentServer :<|> votingServer

fullAPI :: Proxy API
fullAPI = Proxy

app :: Env -> Application
app env =
  addRequestLogging env
    $ serve fullAPI
    $ serverAPI env

fileName :: FilePath
fileName = "localStorage.txt"

initialiseLocalFile :: IO ()
initialiseLocalFile = do
  exists <- doesFileExist fileName
  unless exists
    $ encodeFile fileName mockComments

effToHandler :: Env -> Eff [CommentStorage, SqlPool, Error InputError, Error StorageError, Log, IOE] a -> Handler a
effToHandler env m = do
  result <- liftIO $ runEff
            -- . runLog "hCommenter-API" "Dev" "Console" (env ^. scribe)
            . runLog env
            . logExceptions
            . logExplicitErrors
            . runAndLiftError StorageError
            . runAndLiftError InputError . fmap Right
            . runSqlPool
            . commentHandler
            $ m
  Handler $ except $ handleServerResponse result

  where
    commentHandler = case env ^. backend of
      LocalFile          -> runCommentStorageIO fileName
      ToBeDeterminedProd -> runCommentStorageIO fileName

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
  InputError innerErr -> Left $ servantErrorWithText err400 $ case innerErr of
    BadArgument txt -> "Bad argument: " <> txt

  where
    servantErrorWithText sErr msg =
      sErr
        { errBody = JSON.encode $ ErrorResponse msg (errHTTPCode sErr),
          errHeaders = [(fromString "Content-Type", "application/json;charset=utf-8")]
        }
