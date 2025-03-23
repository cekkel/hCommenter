module Server.Voting (VotingAPI, votingServer) where

import Database.Persist.Sql (fromSqlKey)
import Servant
  ( Capture
  , Description
  , HasServer (ServerT)
  , NoContent (NoContent)
  , PostNoContent
  , type (:<|>) (..)
  , type (:>)
  )

import Effectful qualified as E

import Database.Interface (CommentStorage, CommentUpdate (SendDownvote, SendUpvote), editComment)
import Database.StorageTypes (Comment, Key)
import Logging.LogContext (LogField (CommentId))
import Logging.LogEffect (Log)
import Logging.Utilities (addLogContext, addLogNamespace, logInfo)

-- | Easy to abuse, needs authentication added later
type VotingAPI =
  "comments"
    :> Capture "id" (Key Comment)
    :> ( "upvote" :> Description "Upvote a comment" :> PostNoContent
           :<|> "downvote" :> Description "Downvote a comment" :> PostNoContent
       )

votingServer
  :: ( CommentStorage E.:> es
     , Log E.:> es
     )
  => ServerT VotingAPI (E.Eff es)
votingServer cID = upvote :<|> downvote
 where
  upvote =
    addLogNamespace "Upvote" . addLogContext [CommentId $ fromSqlKey cID] $ do
      _ <- editComment cID [SendUpvote]
      logInfo "Comment upvoted successfully"
      pure NoContent

  downvote =
    addLogNamespace "Downvote" . addLogContext [CommentId $ fromSqlKey cID] $ do
      _ <- editComment cID [SendDownvote]
      logInfo "Comment downvoted successfully"
      pure NoContent
