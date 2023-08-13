module Handlers.Comment (commentServer, ID, SortBy (..), Comment (..), CommentsAPI) where

import           ClassyPrelude
import           Data.Aeson      (FromJSON, ToJSON)
import           Data.Swagger    (ToParamSchema, ToSchema)
import           GHC.Num         (naturalOne)
import           Numeric.Natural (Natural)
import           Servant         (Capture, Delete, Description,
                                  FromHttpApiData (parseQueryParam), Get, JSON,
                                  Post, Put, QueryParam, ReqBody, Server,
                                  type (:<|>) (..), type (:>))

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

commentServer :: Server CommentsAPI
commentServer = getComments :<|> (postComment :<|> editComment :<|> deleteComment)
  where
    getComments from to sortBy = return []
    postComment = return
    editComment commentID = return emptyComment
    deleteComment commentID = return emptyComment

type ID = Natural

data Comment = Comment
  { commentId :: ID
  , message   :: Text
  } deriving (Read, Generic)

instance ToJSON Comment
instance FromJSON Comment
instance ToSchema Comment
instance FromHttpApiData Comment where
  parseQueryParam :: Text -> Either Text Comment
  parseQueryParam = maybe (Left "Comment not formatted correctly") Right . readMay

data SortBy = Old | New | Popular | Controversial
  deriving (Eq, Read, Generic)

instance ToParamSchema SortBy
instance FromHttpApiData SortBy where
  parseQueryParam :: Text -> Either Text SortBy
  parseQueryParam = maybe (Left "Invalid sorting method") Right . readMay

emptyComment :: Comment
emptyComment = Comment { commentId=naturalOne, message="" }
