module Handlers.Comment (commentServer, ID, SortBy (..), Comment, CommentsAPI) where

import           ClassyPrelude          hiding (Handler, log, sortBy)
import           Control.Lens           ((.~))
import qualified Database.Interface     as DB
import           Database.StorageTypes  (Comment, ID, SortBy (..), StorageError,
                                         message)
import qualified Effectful              as E
import           Effectful.Error.Static (Error)
import           Logging                (Log, addLogNamespace, logInfo)
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
  :: ( DB.CommentStorage E.:> es
     , Log E.:> es
     , Error StorageError E.:> es
     )
  => ServerT CommentsAPI (E.Eff es)
commentServer = getComment :<|> getComments :<|> newComment :<|> editComment :<|> deleteComment
  where
    getComment cID = addLogNamespace "GetComment" $ do
      logInfo "Getting a comment"
      DB.getComment cID

    getComments mStart mEnd mSort = addLogNamespace "GetComments" $ do
      logInfo "Getting comments"
      DB.getManyComments (fromMaybe 0 mStart) (fromMaybe 10 mEnd) (fromMaybe Popular mSort)

    newComment comment = addLogNamespace "NewComment" $ do
      logInfo "Creating new comment"
      DB.newComment comment

    editComment cID commentText = addLogNamespace "EditComment" $ do
      logInfo "Editing comment"
      DB.editComment cID (message .~ commentText)

    deleteComment cID = addLogNamespace "DeleteComment" $ do
      logInfo "Deleting comment"
      NoContent <$ DB.deleteComment cID
