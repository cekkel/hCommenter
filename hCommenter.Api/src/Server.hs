module Server (app, swaggerDefinition) where

import           ClassyPrelude
import           Control.Lens               ((&), (.~))
import           Data.Aeson.Encode.Pretty   (encodePretty)
import           Data.ByteString.Lazy.Char8 as BS8 (ByteString)
import           Data.Swagger               (HasInfo (info), HasTitle (title))
import           Handlers.Comment           (CommentsAPI, commentServer)
import           Handlers.Reply             (ReplyAPI, replyServer)
import           Handlers.Voting            (VotingAPI, votingServer)
import           Servant                    (Application, Proxy (..), Server,
                                             serve, type (:<|>) (..))
import           Servant.Swagger            (HasSwagger (toSwagger))

type API = CommentsAPI :<|> ReplyAPI :<|> VotingAPI

swaggerDefinition :: BS8.ByteString
swaggerDefinition =
  encodePretty $ toSwagger (Proxy :: Proxy API)
    & info.title .~ "hCommenter API"

serverAPI :: Server API
serverAPI = commentServer :<|> replyServer :<|> votingServer

fullAPI :: Proxy API
fullAPI = Proxy

app :: Application
app = serve fullAPI serverAPI
