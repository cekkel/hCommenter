module Handlers.Comment (commentServer, ID, SortBy (..), Comment, CommentsAPI) where

import           ClassyPrelude          hiding (Handler, sortBy)
import           Control.Lens           ((.~))
import qualified Database.Interface     as DB
import           Database.StorageTypes  (Comment, ID, SortBy (..), StorageError,
                                         message)
import qualified Effectful              as E
import           Effectful.Error.Static (Error)
import           Servant                (Capture, Description, Get,
                                         HasServer (ServerT), JSON,
                                         NoContent (NoContent), Post,
                                         PostCreated, PostNoContent, QueryParam,
                                         ReqBody, type (:<|>) (..), type (:>))

type CommentsAPI =
  "comment" :>
    (
      Description "Get a single comment by ID"
        :> Capture "id" ID
        :> Get '[JSON] Comment :<|>
      Description "Get all comments in a range"
        :> "range"
        :> QueryParam "from" ID
        :> QueryParam "to" ID
        :> QueryParam "sortby" SortBy
        :> Get '[JSON] [Comment] :<|>
      Description "Create a new comment and get new ID"
        :> "new"
        :> ReqBody '[JSON] Comment
        :> PostCreated '[JSON] ID :<|>
      Description "Edit an existing comment"
        :> "edit"
        :> Capture "id" ID
        :> ReqBody '[JSON] Text
        :> Post '[JSON] Comment :<|>
      Description "Delete a comment"
        :> "delete"
        :> Capture "id" ID
        :> PostNoContent
    )

commentServer
  :: DB.CommentStorage E.:> es
  => Error StorageError E.:> es
  => ServerT CommentsAPI (E.Eff es)
commentServer = DB.getComment :<|> getComments :<|> DB.newComment :<|> editComment :<|> deleteComment
  where
    getComments mStart mEnd mSort =
      DB.getManyComments (fromMaybe 0 mStart) (fromMaybe 10 mEnd) (fromMaybe Popular mSort)

    editComment cID commentText =
      DB.editComment cID (message .~ commentText)

    deleteComment cID =
      DB.deleteComment cID >> pure NoContent
