module Server.Comment (commentServer, SortBy (..), Comment, CommentsAPI) where

import ClassyPrelude hiding (Handler, log, sortBy)
import Data.Aeson (object, (.=))
import Database.Interface qualified as DB
import Database.Persist.Sql (fromSqlKey)
import Database.StorageTypes
  ( Comment
  , CommentId
  , NewComment
  , SortBy (..)
  )
import Effectful qualified as E
import Effectful.Error.Static (Error)
import Katip (showLS)
import Logging
  ( Log
  , addLogContext
  , addLogNamespace
  , logInfo
  )
import Mapping.ExternalTypes (ViewComment)
import Optics
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
import Server.ServerTypes (InputError)

type CommentsAPI =
  "comments"
    :> ( ( Description "Get all comments for a particular conversation (page)"
            :> "conversation"
            :> Capture "convoUrl" Text
            :> QueryParam "sortby" SortBy
            :> Get '[JSON] [ViewComment]
         )
          :<|> ( Description "Get all comments for a particular user"
                  :> "user"
                  :> Capture "username" Text
                  :> QueryParam "sortby" SortBy
                  :> Get '[JSON] [ViewComment]
               )
          :<|> ( Description "Get all replies for a particular comment"
                  :> Capture "id" CommentId
                  :> "replies"
                  :> QueryParam "sortby" SortBy
                  :> Get '[JSON] [ViewComment]
               )
          :<|> ( Description "Create a new comment and get new ID"
                  :> "new"
                  :> ReqBody '[JSON] NewComment
                  :> PostCreated '[JSON] Int64
               )
          :<|> ( Description "Edit an existing comment"
                  :> "edit"
                  :> Capture "id" CommentId
                  :> ReqBody '[JSON] Text
                  :> Post '[JSON] ViewComment
               )
          :<|> ( Description "Delete a comment"
                  :> "delete"
                  :> Capture "id" CommentId
                  :> PostNoContent
               )
       )

commentServer ::
  ( DB.CommentStorage E.:> es
  , Log E.:> es
  , Error InputError E.:> es
  ) =>
  ServerT CommentsAPI (E.Eff es)
commentServer = getConvoComments :<|> getUserComments :<|> getReplies :<|> insertComment :<|> editComment :<|> deleteComment
  where
    getConvoComments convoUrl mSort =
      addLogNamespace "GetConvoComments"
        . addLogContext (object ["ConvoURL" .= convoUrl])
        $ do
          sortBy <- case mSort of
            Nothing -> do
              logInfo "Defaulting sort method to 'Popular'"
              pure Popular
            Just val -> ClassyPrelude.error "hello" >> pure val

          logInfo $ "Getting all comments for conversation, sorted by " <> showLS sortBy

          comments <- DB.getCommentsForConvo convoUrl sortBy

          logInfo $ showLS (length comments) <> " conversation comments retrieved successfully."
          pure comments

    getUserComments username mSort =
      addLogNamespace "GetUserComments"
        . addLogContext (object ["Username" .= username])
        $ do
          sortBy <- case mSort of
            Nothing -> do
              logInfo "Defaulting sort method to 'Popular'"
              pure Popular
            Just val -> pure val

          logInfo $ "Getting all comments for conversation, sorted by " <> showLS sortBy

          comments <- DB.getCommentsForUser username sortBy

          logInfo $ showLS (length comments) <> " user comments retrieved successfully."
          pure comments

    getReplies cID mSort =
      addLogNamespace "GetUserComments" $
        do
          sortBy <- case mSort of
            Nothing -> do
              logInfo "Defaulting sort method to 'Popular'"
              pure Popular
            Just val -> pure val

          logInfo $ "Getting all replies for comment with ID: " <> showLS cID

          replies <- DB.getReplies cID sortBy

          logInfo $ showLS (length replies) <> " replies retrieved successfully."
          pure replies

    insertComment comment =
      addLogNamespace "NewComment"
        . addLogContext
          ( object
              [ "ConvoUrl" .= (comment ^. #convoUrl)
              , "ParentId" .= (comment ^. #parent)
              ]
          )
        $ do
          logInfo "Creating new comment"

          cID <- DB.insertComment comment

          logInfo $ "New comment created with ID: " <> showLS cID
          pure $ fromSqlKey cID

    editComment cID commentText =
      addLogNamespace "EditComment" $
        do
          logInfo $ "Editing comment with ID: " <> showLS cID

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
