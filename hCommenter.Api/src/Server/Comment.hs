{-# LANGUAGE QuasiQuotes #-}

module Server.Comment (commentServer, SortBy (..), Comment, CommentsAPI) where

import Database.Persist.Sql (PersistEntity (Key), fromSqlKey)
import Effectful.Error.Static (Error)
import Katip (showLS)
import Optics
import PyF (fmt)
import Servant
  ( Capture
  , Description
  , Get
  , HasServer (ServerT)
  , JSON
  , NoContent (NoContent)
  , Post
  , PostCreated
  , PostNoContent
  , QueryParam
  , ReqBody
  , type (:<|>) (..)
  , type (:>)
  )
import Prelude hiding (Handler, log, sortBy)

import Effectful qualified as E

import Database.StorageTypes
  ( Comment
  , NewComment
  , SortBy (..)
  )
import Logging.LogContext (LogField (ConvoUrl, ParentId, Username))
import Logging.LogEffect
  ( Log
  , addLogContext
  , addLogNamespace
  , logError
  , logInfo
  )
import Mapping.ExternalTypes (ViewComment)
import Server.ServerTypes (InputError)

import Database.Interface qualified as DB

type CommentsAPI =
  "comments"
    :> QueryParam "sortby" SortBy
    :> ( ( Description "Get all comments for a particular conversation (page)"
             :> "conversation"
             :> Capture "convoUrl" Text
             :> Get '[JSON] [ViewComment]
         )
           :<|> ( Description "Get all comments for a particular user"
                    :> "user"
                    :> Capture "username" Text
                    :> Get '[JSON] [ViewComment]
                )
           :<|> ( Description "Get all replies for a particular comment"
                    :> Capture "id" (Key Comment)
                    :> "replies"
                    :> Get '[JSON] [ViewComment]
                )
           :<|> ( Description "Create a new comment and get new ID"
                    :> "new"
                    :> ReqBody '[JSON] NewComment
                    :> PostCreated '[JSON] Int64
                )
           :<|> ( Description "Edit an existing comment"
                    :> "edit"
                    :> Capture "id" (Key Comment)
                    :> ReqBody '[JSON] Text
                    :> Post '[JSON] ViewComment
                )
           :<|> ( Description "Delete a comment"
                    :> "delete"
                    :> Capture "id" (Key Comment)
                    :> PostNoContent
                )
       )

commentServer
  :: ( DB.CommentStorage E.:> es
     , Error InputError E.:> es
     , Log E.:> es
     )
  => ServerT CommentsAPI (E.Eff es)
commentServer mSort = getConvoComments :<|> getUserComments :<|> getReplies :<|> insertComment :<|> editComment :<|> deleteComment
 where
  mSortBy = maybe (logInfo "Defaulting missing sort method to 'Popular'" >> pure Popular) pure mSort

  getConvoComments convoUrl =
    addLogNamespace "GetConvoComments"
      . addLogContext [ConvoUrl convoUrl]
      $ do
        sortBy <- mSortBy

        logInfo [fmt|Getting all comments for conversation, sorted by '{show sortBy}'|]

        comments <- DB.getCommentsForConvo convoUrl sortBy

        logInfo [fmt|{length comments} conversation comments retrieved successfully.|]
        pure comments

  getUserComments username =
    addLogNamespace "GetUserComments"
      . addLogContext [Username username]
      $ do
        sortBy <- mSortBy

        logInfo [fmt|Getting all comments for conversation, sorted by {show sortBy}|]

        comments <- DB.getCommentsForUser username sortBy

        when (null comments) $ do
          logError [fmt|No comments found for this user with username: {username}|]

        logInfo [fmt|{length comments} user comments retrieved successfully.|]
        pure comments

  getReplies cID =
    addLogNamespace "GetUserComments" $
      do
        sortBy <- mSortBy

        logInfo [fmt|Getting all replies for comment with ID: {show cID}|]

        replies <- DB.getReplies cID sortBy

        when (null replies) $ do
          logError [fmt|No replies found for this comment with id: {show cID}|]

        logInfo [fmt|{length replies} replies retrieved successfully.|]
        pure replies

  insertComment comment =
    addLogNamespace "NewComment"
      . addLogContext
        [ ConvoUrl (comment ^. #convoUrl)
        , ParentId (comment ^. #parent)
        ]
      $ do
        logInfo "Creating new comment"

        cID <- DB.insertComment comment

        logInfo $ [fmt|New comment created with ID: {show cID}|]
        pure $ fromSqlKey cID

  editComment cID commentText =
    addLogNamespace "EditComment" $
      do
        logInfo [fmt|Editing comment with ID: {showLS cID}|]

        updatedComment <- DB.editComment cID (#message .~ commentText)

        logInfo "Comment updated successfully"
        pure updatedComment

  deleteComment cID =
    addLogNamespace "DeleteComment" $
      do
        logInfo "Deleting comment"

        DB.deleteComment cID

        logInfo "Comment deleted successfully"
        pure NoContent
