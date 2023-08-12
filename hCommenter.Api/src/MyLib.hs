{-# LANGUAGE DataKinds #-}
module MyLib (someFunc) where

import           ClassyPrelude
import           GHC.Num.Natural (naturalOne)
import           Numeric.Natural
import           Servant

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type ID = Natural

type ReplyAPI = "reply" :> Capture "id" ID :>
  (
    Description "Get replies in a range"
      :> QueryParam "from" ID :> QueryParam "to" ID :> Get '[JSON] [Comment]
    :<|> Description "Reply to a comment"
      :> ReqBody '[JSON] Comment :> Post '[JSON] Comment
  )

type CommentsAPI =
  "comments" :> "range"
    :> Description "Get all comments in a range"
    :> QueryParam "from" ID
    :> QueryParam "to" ID
    :> QueryParam "sortby" SortBy
    :> Get '[JSON] [Comment]

  :<|> "comment" :>
    (
      Description "Create a new comment" :> ReqBody '[JSON] Comment :> Post '[JSON] Comment
      :<|> Description "Edit an existing comment" :> Capture "id" ID :> Put '[JSON] Comment
      :<|> Description "Delete a comment" :> Capture "id" ID :> Delete '[JSON] Comment
    )

data SortBy = Old | New

replyServer :: Server ReplyAPI
replyServer commentID = getReplies :<|> postReply
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

data Comment = Comment
  { commentId :: ID
  , message   :: Text
  }

emptyComment :: Comment
emptyComment = Comment { commentId=naturalOne, message="" }
