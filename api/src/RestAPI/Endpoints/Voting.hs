module RestAPI.Endpoints.Voting (VotingAPI, votingServer) where

import Database.Persist.Sql (fromSqlKey)
import Servant
  ( Capture
  , Description
  , HasServer (ServerT)
  , JSON
  , Post
  , type (:<|>) (..)
  , type (:>)
  )

import Effectful qualified as E

import Database.Comments.Interface (CommentStorage, CommentUpdate (SendDownvote, SendUpvote), editComment)
import Database.Schema (Comment, Key)
import Logging.LogContext (LogField (CommentId))
import Logging.LogEffect (Log)
import Logging.Utilities (addLogContext, addLogNamespace, logInfo)

-- | Easy to abuse, needs authentication added later
type VotingAPI =
  "comments"
    :> Capture "id" (Key Comment)
    :> ( "upvote" :> Description "Upvote a comment" :> Post '[JSON] ()
           :<|> "downvote" :> Description "Downvote a comment" :> Post '[JSON] ()
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
      pure ()

  downvote =
    addLogNamespace "Downvote" . addLogContext [CommentId $ fromSqlKey cID] $ do
      _ <- editComment cID [SendDownvote]
      logInfo "Comment downvoted successfully"
      pure ()
