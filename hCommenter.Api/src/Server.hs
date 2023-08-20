module Server (app, swaggerDefinition) where

import           ClassyPrelude              hiding (Handler)
import           Control.Lens               ((&), (.~))
import           Control.Monad.Trans.Except (except, withExceptT)
import qualified Data.Aeson                 as JSON
import           Data.Aeson.Encode.Pretty   (encodePretty)
import           Data.ByteString.Lazy.Char8 as BS8 (ByteString)
import           Data.Swagger               (HasInfo (info), HasTitle (title))
import           Database.Interface         (CommentStorage,
                                             runCommentStoragePure)
import           Database.Mockserver        (mockComments)
import           Database.StorageTypes
import           Effectful                  (Eff, runPureEff)
import           Effectful.Error.Static     (Error, runErrorNoCallStack)
import           Handlers.Comment           (CommentsAPI, commentServer)
import           Handlers.Reply             (ReplyAPI, replyServer)
import           Handlers.Voting            (VotingAPI, votingServer)
import           Servant                    (Application, Handler (Handler),
                                             Proxy (..), Server,
                                             ServerError (errBody, errHTTPCode, errHeaders),
                                             err404, hoistServer, serve,
                                             type (:<|>) (..))
import           Servant.Swagger            (HasSwagger (toSwagger))
import qualified ServerTypes                as T

type API = CommentsAPI :<|> ReplyAPI :<|> VotingAPI

swaggerDefinition :: BS8.ByteString
swaggerDefinition =
  encodePretty $ toSwagger (Proxy :: Proxy API)
    & info.title .~ "hCommenter API"

serverAPI :: Server API
serverAPI = hoistServer fullAPI effToHandler $
  commentServer :<|> replyServer :<|> votingServer

fullAPI :: Proxy API
fullAPI = Proxy

app :: Application
app = serve fullAPI serverAPI

effToHandler :: Eff [CommentStorage, Error StorageError] a -> Handler a
effToHandler m = do
  Handler
    . withExceptT toServerError
    . except
    . runPureEff
    . runErrorNoCallStack @StorageError
    . runCommentStoragePure mockComments
    $ m
  where
    toServerError = \case
      CommentNotFound -> servantErrorWithText err404 "Can't find the comment"

servantErrorWithText ::
  ServerError ->
  Text ->
  ServerError
servantErrorWithText sErr msg =
  sErr
    { errBody = errorBody (errHTTPCode sErr),
      errHeaders = [jsonHeaders]
    }
  where
    errorBody code = JSON.encode $ T.Error msg code

    jsonHeaders =
      (fromString "Content-Type", "application/json;charset=utf-8")
