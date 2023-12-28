module Server.Reply (ReplyAPI, replyServer) where

import           ClassyPrelude          hiding ((<|))
import           Control.Lens           ((<>~), (^.))
import           Data.Aeson             (object, (.=))
import           Database.Interface     (CommentStorage, editComment,
                                         getComment, newComment)
import           Database.StorageTypes
import qualified Effectful              as E
import           Effectful.Error.Static (Error, throwError)
import           Logging                (Log, addLogContext, addLogNamespace,
                                         logInfo)
import           Servant                hiding (throwError)

type ReplyAPI = "reply" :> Capture "id" ID :>
  (
    Description "Reply to a comment" :> ReqBody '[JSON] Comment :> PostNoContent :<|>
    Description "Get replies in a range"
      :> QueryParam "from" Int :> QueryParam "to" Int :> Get '[JSON] [Comment]
  )

replyServer
  :: ( CommentStorage E.:> es
     , Error StorageError E.:> es
     , Log E.:> es
     )
  => ServerT ReplyAPI (E.Eff es)
replyServer cID = postReply :<|> getReplies
  where
    postReply comment = addLogNamespace "CreateReply" . addLogContext (object ["ParentID" .= cID]) $ do
      logInfo "Creating reply"
      replyID <- newComment comment

      addLogContext (object ["ReplyID" .= replyID]) $ do
        logInfo "Reply created"
        _ <- editComment cID (replies <>~ pure replyID)
        logInfo "Reply attached to parent"
        pure NoContent

    getReplies mFrom mTo = addLogNamespace "GetReplies" $ case (mFrom, mTo) of
      (Nothing, _) -> throwError CommentNotFound
      (_, Nothing) -> throwError CommentNotFound
      (Just fromPos, Just toPos) -> addLogContext (object ["fromReplyPosition" .= fromPos, "toReplyPosition" .= toPos]) $ do
        logInfo "Getting parent"
        parent <- getComment cID

        -- TODO: Possible optimisation since take & drop are quite inefficient.
        let replyIDs = drop fromPos $ take toPos $ parent ^. replies

        addLogContext (object ["ReplyIDs" .= replyIDs]) $ do
          logInfo "Getting replies"
          allReplies <- mapM getComment replyIDs
          logInfo "Replies found successfully"
          pure allReplies
