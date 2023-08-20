module Handlers.Comment (commentServer, ID, SortBy (..), Comment, CommentsAPI) where

import           ClassyPrelude         hiding (Handler, sortBy)
import           Database.Interface    (CommentStorage, deleteComment,
                                        editComment, getComment,
                                        getManyComments, newComment)
import           Database.StorageTypes (Comment, ID, SortBy (..))
import qualified Effectful             as E
import           Servant               (Capture, Delete, Description, Get,
                                        HasServer (ServerT), JSON, Post, Put,
                                        QueryParam, ReqBody, type (:<|>) (..),
                                        type (:>))

type CommentsAPI =
  "comments" :> "range"
    :> Description "Get all comments in a range"
    :> QueryParam "from" ID
    :> QueryParam "to" ID
    :> QueryParam "sortby" SortBy
    :> Get '[JSON] [Comment]

  :<|> "comment" :>
    (
      Description "Get a single comment by ID"
        :> Capture "id" ID :> Get '[JSON] Comment :<|>
      Description "Create a new comment and get new ID"
        :> "new" :> ReqBody '[JSON] Comment :> Post '[JSON] ID :<|>
      Description "Edit an existing comment"
        :> Capture "id" ID :> ReqBody '[JSON] Comment :> Put '[JSON] () :<|>
      Description "Delete a comment"
        :> Capture "id" ID :> Delete '[JSON] ()
    )

commentServer
  :: CommentStorage E.:> es
  => ServerT CommentsAPI (E.Eff es)
commentServer = getComments :<|> (getComment :<|> newComment :<|> replaceComment :<|> deleteComment)
  where
    getComments mStart mEnd mSort =
      getManyComments (fromMaybe 0 mStart) (fromMaybe 10 mEnd) (fromMaybe Popular mSort)

    replaceComment cID comment = editComment cID (const comment)
