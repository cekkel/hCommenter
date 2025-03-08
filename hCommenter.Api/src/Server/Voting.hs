module Server.Voting (VotingAPI, votingServer) where

import Database.Persist.Sql (fromSqlKey)
import Optics
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

import Database.Interface (CommentStorage, editComment)
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
votingServer cID = vote "UpvoteComment" #upvotes :<|> vote "DownvoteComment" #downvotes
 where
  vote namespace voteBox =
    addLogNamespace namespace . addLogContext [CommentId $ fromSqlKey cID] $ do
      logInfo "Incrementing vote box"
      _ <- editComment cID (voteBox %~ (+ 1))
      logInfo "Vote box incremented successfully"
      pure NoContent
