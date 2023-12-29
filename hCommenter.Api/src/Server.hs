{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Server (swaggerDefinition, initialiseLocalFile, app, Backend (..)) where

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
import           Database.PureStorage       (runCommentStoragePure)
import           Database.StorageTypes
import           Effectful                  (Eff, IOE, runEff, (:>))
import           Effectful.Error.Static     (CallStack, Error, prettyCallStack,
                                             runError)
import           Katip                      (Verbosity (V0), showLS)
import           Logging                    (Log, getConsoleScribe, logError,
                                             logExceptions, runLog)
import           Servant                    (Application, Handler (Handler),
                                             Proxy (..), Server,
                                             ServerError (errBody, errHTTPCode, errHeaders),
                                             err400, err404, hoistServer, serve,
                                             type (:<|>) (..))
import           Servant.Swagger            (HasSwagger (toSwagger))
import           Server.Comment             (CommentsAPI, commentServer)
import           Server.Reply               (ReplyAPI, replyServer)
import           Server.ServerTypes
import           Server.Voting              (VotingAPI, votingServer)
import           System.Directory.Extra     (doesFileExist)

type API = CommentsAPI :<|> ReplyAPI :<|> VotingAPI

swaggerDefinition :: BS8.ByteString
swaggerDefinition =
  encodePretty $ toSwagger (Proxy :: Proxy API)
    & info.title .~ "hCommenter API"

data Backend
  = Static
  | LocalFile
  | ToBeDeterminedProd
  deriving (Show)

newtype Env = Env {
  _backend :: Backend
}
makeLenses ''Env

serverAPI :: Env -> Server API
serverAPI env = do
  hoistServer fullAPI (effToHandler env) $
    commentServer :<|> replyServer :<|> votingServer

fullAPI :: Proxy API
fullAPI = Proxy

app :: Backend -> Application
app = serve fullAPI . serverAPI . Env

fileName :: FilePath
fileName = "localStorage.txt"

initialiseLocalFile :: IO ()
initialiseLocalFile = do
  exists <- doesFileExist fileName
  unless exists
    $ encodeFile fileName mockComments

effToHandler :: Env -> Eff [CommentStorage, Error InputError, Error StorageError, Log, IOE] a -> Handler a
effToHandler env m = do
  scribe <- liftIO $ getConsoleScribe V0
  result <- liftIO $ runEff
            . runLog "hCommenter-API" "Dev" "Console" scribe
            . logExceptions
            . logExplicitErrors
            . runAndLiftError StorageError
            . runAndLiftError InputError . fmap Right
            . commentHandler
            $ m
  Handler $ except $ handleServerResponse result

  where
    commentHandler = case env ^. backend of
      Static             -> runCommentStoragePure mockComments
      LocalFile          -> runCommentStorageIO fileName
      ToBeDeterminedProd -> runCommentStoragePure mockComments

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
