module Handlers.Comment (commentServer, ID, SortBy (..), Comment, CommentsAPI) where

import           ClassyPrelude         hiding (Handler, sortBy)
import           Database.Interface    (CommentStorage, deleteComment,
                                        editComment, getComment,
                                        getManyComments, newComment)
import           Database.StorageTypes (Comment, ID, SortBy (..))
import qualified Effectful             as E
import           Servant               (Capture, DeleteNoContent, Description,
                                        Get, HasServer (ServerT), JSON, Post,
                                        PutNoContent, QueryParam, ReqBody,
                                        type (:<|>) (..), type (:>))

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
        :> Post '[JSON] ID :<|>
      Description "Edit an existing comment"
        :> Capture "id" ID
        :> ReqBody '[JSON] Comment
        :> PutNoContent :<|>
      Description "Delete a comment"
        :> Capture "id" ID
        :> DeleteNoContent
    )

commentServer
  :: CommentStorage E.:> es
  => ServerT CommentsAPI (E.Eff es)
commentServer = getComment :<|> getComments :<|> newComment :<|> replaceComment :<|> deleteComment
  where
    getComments mStart mEnd mSort =
      getManyComments (fromMaybe 0 mStart) (fromMaybe 10 mEnd) (fromMaybe Popular mSort)

    replaceComment cID comment = editComment cID (const comment)
