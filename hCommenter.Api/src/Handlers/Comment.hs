module Handlers.Comment (commentServer, ID, SortBy (..), Comment, CommentsAPI, InputError (..)) where

import           ClassyPrelude          hiding (Handler, log, sortBy)
import           Control.Lens           ((.~))
import qualified Database.Interface     as DB
import           Database.StorageTypes  (Comment, ID, SortBy (..), StorageError,
                                         message)
import qualified Effectful              as E
import           Effectful.Error.Static (Error, throwError)
import           Katip                  (showLS)
import           Logging                (Log, addLogContext, addLogNamespace,
                                         logInfo)
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

newtype InputError = BadArgument Text

commentServer
  :: ( DB.CommentStorage E.:> es
     , Log E.:> es
     , Error StorageError E.:> es
     , Error InputError E.:> es
     )
  => ServerT CommentsAPI (E.Eff es)
commentServer = getComment :<|> getComments :<|> newComment :<|> editComment :<|> deleteComment
  where
    getComment cID = addLogNamespace "Comment" . addLogContext cID $ do
      logInfo "Getting a comment"
      comment <- DB.getComment cID
      addLogContext comment $ do
        logInfo "Returning comment"
        pure comment

    getComments mStart mEnd mSort = addLogNamespace "GetComments" $ case (mStart, mEnd) of
      (Nothing, _) -> throwError $ BadArgument "Missing 'start' parameter"
      (_, Nothing) -> throwError $ BadArgument "Missing 'end' parameter"
      (Just start, Just end) -> addLogContext [start, end] . addLogContext sortMethod $ do
        logInfo $
          (
            if isJust mSort
            then "Getting comments with sorting method: '"
            else "Getting comments with default sorting method: '"
          ) <> showLS sortMethod <> "'."

        comments <- DB.getManyComments start end sortMethod

        addLogContext comments $ do
          logInfo "Returning comments successfully"
          pure comments

        where
          sortMethod = fromMaybe Popular mSort

    newComment comment = addLogNamespace "NewComment" . addLogContext comment $ do
      logInfo "Creating new comment"

      cID <- DB.newComment comment

      addLogContext cID $ do
        logInfo "New comment created"
        pure cID

    editComment cID commentText = addLogNamespace "EditComment" $ do
      logInfo "Editing comment"

      updatedComment <- DB.editComment cID (message .~ commentText)

      addLogContext updatedComment $ do
        logInfo "Comment updated successfully"
        pure updatedComment

    deleteComment cID = addLogNamespace "DeleteComment" $ do
      logInfo "Deleting comment"

      DB.deleteComment cID

      logInfo "Comment deleted successfully"
      pure NoContent
