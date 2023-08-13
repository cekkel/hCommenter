{-# LANGUAGE DataKinds #-}
module MyLib (swaggerDefinition) where

import           ClassyPrelude
import           Control.Lens
import           Data.Aeson.Encode.Pretty   (encodePretty)
import           Data.ByteString.Lazy.Char8 as BS8
import           Data.Swagger
import           GHC.Num.Natural            (naturalOne)
import           Numeric.Natural
import           Servant
import           Servant.Swagger

type ID = Natural

type API = CommentsAPI :<|> ReplyAPI :<|> VotingAPI

type CommentsAPI =
  "comments" :> "range"
    :> Description "Get all comments in a range"
    :> QueryParam "from" ID
    :> QueryParam "to" ID
    :> QueryParam "sortby" SortBy
    :> Get '[JSON] [Comment]

  :<|> "comment" :>
    (
      Description "Create a new comment" :> ReqBody '[JSON] Comment :> Post '[JSON] Comment :<|>
      Description "Edit an existing comment" :> Capture "id" ID :> Put '[JSON] Comment :<|>
      Description "Delete a comment" :> Capture "id" ID :> Delete '[JSON] Comment
    )

type ReplyAPI = "reply" :> Capture "id" ID :>
  (
    Description "Reply to a comment" :> ReqBody '[JSON] Comment :> Post '[JSON] Comment :<|>
    Description "Get replies in a range"
      :> QueryParam "from" ID :> QueryParam "to" ID :> Get '[JSON] [Comment]
  )

-- | Easy to abuse, needs authentication added later
type VotingAPI =
  "comment" :> Capture "id" ID :>
    (
      "upvote" :> Description "Upvote a comment" :> Post '[JSON] Comment :<|>
      "downvote" :> Description "Downvote a comment" :> Post '[JSON] Comment
    )


replyServer :: Server ReplyAPI
replyServer commentID = postReply :<|> getReplies
  where
    getReplies from to = return []
    postReply = return

commentServer :: Server CommentsAPI
commentServer = getComments :<|> (postComment :<|> editComment :<|> deleteComment)
  where
    getComments from to sortBy = return []
    postComment = return
    editComment commentID = return emptyComment
    deleteComment commentID = return emptyComment

votingServer :: Server VotingAPI
votingServer commentID = postUpvote :<|> postDownvote
  where
    postUpvote = return emptyComment
    postDownvote = return emptyComment

data SortBy = Old | New deriving (Generic)

instance ToParamSchema SortBy

data Comment = Comment
  { commentId :: ID
  , message   :: Text
  } deriving (Generic)

instance ToSchema Comment

emptyComment :: Comment
emptyComment = Comment { commentId=naturalOne, message="" }

swaggerDefinition :: BS8.ByteString
swaggerDefinition =
  encodePretty $ toSwagger (Proxy :: Proxy API)
    & info.title .~ "hCommenter API"
