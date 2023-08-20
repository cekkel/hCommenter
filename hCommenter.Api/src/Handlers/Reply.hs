module Handlers.Reply (ReplyAPI, replyServer) where

import           ClassyPrelude         hiding ((<|))
import           Control.Lens
import           Database.Interface    (CommentStorage, editComment, getComment,
                                        newComment)
import           Database.StorageTypes
import qualified Effectful             as E
import           Servant

type ReplyAPI = "reply" :> Capture "id" ID :>
  (
    Description "Reply to a comment" :> ReqBody '[JSON] Comment :> Post '[JSON] () :<|>
    Description "Get replies in a range"
      :> QueryParam "from" Int :> QueryParam "to" Int :> Get '[JSON] [Comment]
  )

replyServer
  :: CommentStorage E.:> es
  => ServerT ReplyAPI (E.Eff es)
replyServer cID = postReply cID :<|> getReplies cID

postReply
  :: CommentStorage E.:> es
  => ID
  -> Comment
  -> E.Eff es ()
postReply cID comment = do
  replyID <- newComment comment
  editComment cID (replies <>~ pure replyID)

getReplies
  :: CommentStorage E.:> es
  => ID
  -> Maybe Int
  -> Maybe Int
  -> E.Eff es [Comment]
getReplies cID mFrom mTo = do
  parent <- getComment cID
  let (fromPos, toPos) = fromMaybe (0, 10) $ (,) <$> mFrom <*> mTo
  -- TODO: Possible optimisation since take & drop are quite inefficient.
  let replyIDs = drop fromPos $ take toPos $ parent ^. replies
  mapM getComment replyIDs
