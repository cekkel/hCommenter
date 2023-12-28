{-# LANGUAGE UndecidableInstances #-}

module Server (app, swaggerDefinition) where

import           ClassyPrelude              hiding (Handler)
import           Control.Lens               ((&), (.~))
import           Control.Monad.Trans.Except (except)
import qualified Data.Aeson                 as JSON
import           Data.Aeson.Encode.Pretty   (encodePretty)
import           Data.Bifoldable            (bitraverse_)
import qualified Data.ByteString.Lazy.Char8 as BS8 (ByteString)
import           Data.Either.Extra          (mapLeft)
import           Data.Swagger               (HasInfo (info), HasTitle (title))
import           Database.Interface         (CommentStorage)
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

type API = CommentsAPI :<|> ReplyAPI :<|> VotingAPI

swaggerDefinition :: BS8.ByteString
swaggerDefinition =
  encodePretty $ toSwagger (Proxy :: Proxy API)
    & info.title .~ "hCommenter API"

serverAPI :: Server API
serverAPI = do
  hoistServer fullAPI effToHandler $
    commentServer :<|> replyServer :<|> votingServer

fullAPI :: Proxy API
fullAPI = Proxy

app :: Application
app = serve fullAPI serverAPI

effToHandler :: Eff [CommentStorage, Error InputError, Error StorageError, Log, IOE] a -> Handler a
effToHandler m = do
  scribe <- liftIO $ getConsoleScribe V0
  result <- liftIO $ runEff
            . runLog "hCommenter-API" "Dev" "Console" scribe
            . logExceptions
            . logExplicitErrors
            . runAndLiftError StorageError
            . runAndLiftError InputError . fmap Right
            . runCommentStoragePure mockComments
            $ m
  Handler $ except $ mapLeft errorToServerResponse result

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

errorToServerResponse :: (CallStack, CustomError) -> ServerError
errorToServerResponse (_, err) = case err of
  StorageError innerErr -> servantErrorWithText err404 $ case innerErr of
    CommentNotFound -> "Can't find the comment"
  InputError innerErr -> servantErrorWithText err400 $ case innerErr of
    BadArgument txt -> "Bad argument: " <> txt

  where
    servantErrorWithText sErr msg =
      sErr
        { errBody = JSON.encode $ ErrorResponse msg (errHTTPCode sErr),
          errHeaders = [(fromString "Content-Type", "application/json;charset=utf-8")]
        }
